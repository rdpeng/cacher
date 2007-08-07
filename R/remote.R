################################################################################

clonecache <- function(id, cachedir = ".cache") {
        ## For now, 'id' is a URL like http://asdf.org/.cache
        mkdirs(cachedir)
        setConfig("cachedir", cachedir)
        initDownload(id)
        writeLines(id, file.path(cachedir, "ORIGIN"))
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
        isLocal <- length(grep("^file://", url, perl = TRUE)) > 0

        if(isLocal) {
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
        }
}

