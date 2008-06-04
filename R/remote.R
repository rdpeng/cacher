################################################################################

clonecache <- function(origin, cachedir = ".cache", all.files = FALSE, id = NULL) {
        ## 'origin' is a URL like http://asdf.org/.cache, or
        ## file:///home/rpeng/.cache or a simple path like ~/.cache

        if(!is.null(id))
                origin <- packageArchive(id)
        mkdirs(cachedir)
        message(gettextf("created cache directory '%s'", cachedir))
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
        cat("Starting download of cache database files\n")
        cat(gettext("Counting files: "))
        dbfiles <- readLines(file.path(cachedir, "dbfiles"))

        if(is.null(origin))
                origin <- readLines(file.path(cachedir, "origin"))
        nfiles <- length(dbfiles)

        cat(nfiles, "\n", sep = "")
        cat(gettext("Downloading files: "))

        for(i in seq_along(dbfiles)) {
                msg <- sprintf("%d%% (%d/%d)", round(100 * i / nfiles),
                               i, nfiles)
                cat(msg)
                src <- file.path(dbdir(origin), dbfiles[i])
                dest <- file.path(dbdir(cachedir), dbfiles[i])

                if(!(file.exists(dest) && skip.existing)) {
                        start <- proc.time()
                        download(src, dest, mode = "wb")
                        end <- proc.time()
                        diff <- end - start

                        ## This is a cheap hack to prevent the server
                        ## from being hammered by repeated requests
                        if(diff["elapsed"] < 0.1)
                                Sys.sleep(0.25)
                }
                back <- paste(rep("\b", nchar(msg)), collapse = "")
                cat(back)
        }
        cat("\n")
}

isClone <- function() {
        originfile <- file.path(cache(), "origin")
        file.exists(originfile) && (file.info(originfile)$size > 0)
}

transferCacheFile <- function(cacheFile, cachedir) {
        if(file.exists(cacheFile))
                return(NULL)
        msg <- gettext("/ transferring cache db file ", basename(cacheFile))
        msg <- paste(msg, collapse = "")
        msg <- truncateLine(msg)

        vmessage(msg)
        origin <- readLines(file.path(cachedir, "origin"))
        src <- file.path(dbdir(origin), basename(cacheFile))

        download(src, cacheFile, mode = "wb")
}

download <- function(url, destfile, method = "auto", quiet = TRUE,
                     mode = "w", cacheOK = TRUE) {
        isLocal <- length(grep("^\\w+://", url, perl = TRUE)) == 0
        isFileURL <- length(grep("^file://", url, perl = TRUE)) > 0

        if(isLocal) {
                status <- file.copy(url, destfile, overwrite = TRUE)

                if(!status)
                        warning("problem copying file ", url)
        }
        else if(isFileURL) {
                url <- sub("^file://", "", url, perl = TRUE)
                status <- file.copy(url, destfile, overwrite = TRUE)

                if(!status)
                        warning("problem copying file ", url)
        }
        else {
                status <- download.file(url, destfile, method, quiet,
                                        mode, cacheOK)
                if(status > 0)
                        warning("problem downloading file ", url)
        }
        status
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

