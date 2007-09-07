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

getIDdir <- function(id) {
        file.path(substring(id, 1, 4), substring(id, 5))
}

packageArchive <- function(id) {
        baseurl <- getConfig("archive")
        
        if(is.null(baseurl))
                stop("archive URL not set")
        file.path(baseurl, "packages", getIDdir(id))
}

downloadCacheDB <- function(cachedir = ".cache", skip.existing = TRUE,
                            origin = NULL) {
        message("downloading cache database files")
        dbfiles <- readLines(file.path(cachedir, "dbfiles"))

        if(is.null(origin))
                origin <- readLines(file.path(cachedir, "origin"))
        for(i in seq_along(dbfiles)) {
                src <- file.path(dbdir(origin), dbfiles[i])
                dest <- file.path(dbdir(cachedir), dbfiles[i])

                showMeter(i, length(dbfiles))

                if(file.exists(dest) && skip.existing)
                        next
                download(src, dest)
        }
        message("\nfinished")
}

showMeter <- function(i, n) {
        if(n < getOption("width"))
                message("=", appendLF = FALSE)
        else {
                mark <- i * getOption("width") / n

                if(floor(mark) == ceiling(mark))
                        message("=", appendLF = FALSE)
        }
}

isClone <- function() {
        file.exists(file.path(cache(), "origin"))
}

transferCacheFile <- function(cacheFile, cachedir) {
        if(file.exists(cacheFile))
                return(NULL)
        message("transferring cache db file ", basename(cacheFile))
        origin <- readLines(file.path(cachedir, "origin"))
        src <- file.path(dbdir(origin), basename(cacheFile))

        download(src, cacheFile, mode = "wb")
}

download <- function(url, destfile, method, quiet = TRUE, mode = "w",
                       cacheOK = TRUE) {
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

        message("downloading source file list")
        download(file.path(id, "srcfiles"), file.path(cachedir, "srcfiles"),
                   mode = "w")
        srcfiles <- readLines(file.path(cachedir, "srcfiles"))

        for(srcfile in srcfiles) {
                metafile <- file.path(metadir(id),
                                      paste(srcfile, "meta", sep = "."))
                message("downloading metadata")
                download(metafile,
                         file.path(metadir(cachedir), basename(metafile)),
                         mode = "w")
                message("downloading source files")
                download(file.path(srcdir(id), srcfile),
                         file.path(srcdir(cachedir), srcfile), mode = "w")
                message("downloading cache database file list")
                download(file.path(id, "dbfiles"),
                         file.path(cachedir, "dbfiles"), mode = "w")
        }
}

