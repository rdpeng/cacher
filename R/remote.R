################################################################################

clonecache <- function(id) {
        ## For now, 'id' is a URL like http://asdf.org/.cache
        cachedir <- ".cache"
        mkdirs(cachedir)
        setConfig("cachedir", cachedir)
        initDownload(id)
}

ccdownload <- function(url, destfile, method, quiet = FALSE, mode = "w",
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

        ccdownload(file.path(id, "SRCFILES"), file.path(cachedir, "SRCFILES"))
        srcfiles <- readLines(file.path(cachedir, "SRCFILES"))

        for(srcfile in srcfiles) {
                metafile <- file.path(metadir(id),
                                      paste(srcfile, "meta", sep = "."))
                ccdownload(metafile,
                           file.path(metadir(cachedir), basename(metafile)))
                ccdownload(file.path(srcdir(id), srcfile),
                           file.path(srcdir(cachedir), srcfile))
        }
}
