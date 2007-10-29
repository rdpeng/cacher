################################################################################

clonecache <- function(origin, cachedir = ".cache", all.files = FALSE, id = NULL) {
	## 'origin' is a URL like http://asdf.org/.cache, or
	## file:///home/rpeng/.cache or a simple path like ~/.cache

	if(!is.null(id))
		origin <- packageArchive(id)
	mkdirs(cachedir)
	setConfig("cachedir", cachedir)
	initDownload(origin)
	writeLines(origin, file.path(cachedir, "origin"))

	if(all.files)
		downloadCacheDB(cachedir, FALSE, origin)
}


## To create the directory for the archived cache, we take the first 4
## chars of the SHA1 checksum as the 'prefix' directory and the
## remaining chars as the 'suffix' or package directory.  The package
## contents are stored in <baseurl>/<prefix>/<suffix>.  This prevents
## having a single directory with a lot of files.

PREFIX_BEGIN <- 1
PREFIX_END <- 4
SUFFIX_BEGIN <- 5

getIDdir <- function(id) {
	file.path(substring(id, PREFIX_BEGIN, PREFIX_END),
		  substring(id, SUFFIX_BEGIN))
}

matchPkgID <- function(id) {
	baseurl <- getConfig("archive")
	digestList <- readLines(file.path(baseurl, "cpkg", "PKG_DIGESTS"))
	idx <- pmatch(id, digestList)

	if(is.na(idx))
		stop(gettextf("exact match not found for '%s'", id))
	digestList[idx]
}

packageArchive <- function(id) {
	if(length(id) > 1)
		stop("only one 'id' can be cloned at a time")
	if(nchar(id) < 40)
		id <- matchPkgID(id)
	baseurl <- getConfig("archive")
	
	if(is.null(baseurl))
		stop("archive URL not set")
	file.path(baseurl, "cpkg", getIDdir(id))
}

downloadCacheDB <- function(cachedir = ".cache", skip.existing = TRUE,
			    origin = NULL) {
	vmessage("downloading cache database files")
	dbfiles <- readLines(file.path(cachedir, "dbfiles"))

	if(is.null(origin))
		origin <- readLines(file.path(cachedir, "origin"))
	for(i in seq_along(dbfiles)) {
		src <- file.path(dbdir(origin), dbfiles[i])
		dest <- file.path(dbdir(cachedir), dbfiles[i])

		if(file.exists(dest) && skip.existing)
			next
		download(src, dest)
	}
	vmessage("\nfinished")
}

isClone <- function() {
	originfile <- file.path(cache(), "origin")
	file.exists(originfile) && (file.info(originfile)$size > 0)
}

transferCacheFile <- function(cacheFile, cachedir) {
	if(file.exists(cacheFile))
		return(NULL)
	vmessage("/ transferring cache db file ", basename(cacheFile))
	origin <- readLines(file.path(cachedir, "origin"))
	src <- file.path(dbdir(origin), basename(cacheFile))

	download(src, cacheFile, mode = "wb")
}

download <- function(url, destfile, method = "auto", quiet = TRUE,
		     mode = "w", cacheOK = TRUE) {
	isLocal <- length(grep("^\\w+://", url, perl = TRUE)) == 0
	isFileURL <- length(grep("^file://", url, perl = TRUE)) > 0

	if(isLocal)
		file.copy(url, destfile, overwrite = TRUE)
	else if(isFileURL) {
		url <- sub("^file://", "", url, perl = TRUE)
		file.copy(url, destfile, overwrite = TRUE)
	}
	else
		download.file(url, destfile, method, quiet, mode, cacheOK)
}

initDownload <- function(id) {
	cachedir <- cache()

	vmessage("downloading source file list")
	download(file.path(id, "srcfiles"), file.path(cachedir, "srcfiles"),
		   mode = "w")
	srcfiles <- readLines(file.path(cachedir, "srcfiles"))

	for(srcfile in srcfiles) {
		metafile <- file.path(metadir(id),
				      paste(srcfile, "meta", sep = "."))
		vmessage("downloading metadata")
		download(metafile,
			 file.path(metadir(cachedir), basename(metafile)),
			 mode = "w")
		vmessage("downloading source files")
		download(file.path(srcdir(id), srcfile),
			 file.path(srcdir(cachedir), srcfile), mode = "w")
		vmessage("downloading cache database file list")
		download(file.path(id, "dbfiles"),
			 file.path(cachedir, "dbfiles"), mode = "w")
	}
}

