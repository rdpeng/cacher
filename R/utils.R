package <- function(cachedir, other = character(0)) {
        if(missing(cachedir))
                cachedir <- getConfig("cachedir")
        name <- paste(tempfile(), "zip", sep = ".")

        cmd <- paste("zip -r", name, cachedir, paste(other, collapse = " "))
        out <- system(cmd, intern = TRUE)
        checksum <- md5sum(name)
        newname <- paste("./cpkg-", checksum, ".zip", sep = "")
        message(gettextf("creating package '%s'", basename(newname)))
        status <- file.copy(name, newname)

        if(!status)
                warning("problem creating package file")
        invisible(out)
}
