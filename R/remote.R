################################################################################

clonecache <- function(origin, cachedir = ".cache", download.cache = FALSE) {
        ## For now, 'origin' is a URL like http://asdf.org/.cache
        mkdirs(cachedir)
        setConfig("cachedir", cachedir)
        initDownload(origin)
        writeLines(origin, file.path(cachedir, "ORIGIN"))

        if(download.cache) {
                message("downloading cache database files")
                dbfiles <- readLines(file.path(cachedir, "DBFILES"))

                for(i in seq_along(dbfiles)) {
                        src <- file.path(dbdir(origin), dbfiles[i])
                        dest <- file.path(dbdir(cachedir), dbfiles[i])

                        showMeter(i, length(dbfiles))
                        download(src, dest)
                }
                cat("\nfinished\n")
        }
}

showMeter <- function(i, n) {
        if(n < getOption("width"))
                cat("=", sep = "")
        else {
                mark <- i * getOption("width") / n

                if(floor(mark) == ceiling(mark))
                        cat("=", sep = "")
        }
}

isClone <- function() {
        cachedir <- getConfig("cachedir")
        file.exists(file.path(cachedir, "ORIGIN"))
}

transferCacheFile <- function(cacheFile, cachedir) {
        if(file.exists(cacheFile))
                return(NULL)
        message("transferring cache db file")
        origin <- readLines(file.path(cachedir, "ORIGIN"))
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
        cachedir <- getConfig("cachedir")

        download(file.path(id, "SRCFILES"), file.path(cachedir, "SRCFILES"),
                   mode = "w")
        srcfiles <- readLines(file.path(cachedir, "SRCFILES"))

        for(srcfile in srcfiles) {
                metafile <- file.path(metadir(id),
                                      paste(srcfile, "meta", sep = "."))
                download(metafile,
                         file.path(metadir(cachedir), basename(metafile)),
                         mode = "w")
                download(file.path(srcdir(id), srcfile),
                         file.path(srcdir(cachedir), srcfile), mode = "w")
                download(file.path(id, "DBFILES"),
                         file.path(cachedir, "DBFILES"), mode = "w")
        }
}

