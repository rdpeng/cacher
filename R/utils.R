## Package files are of the form 'cpkg-<32 char checksum>.zip'.  In
## order to extract the checksum/ID from the file name, we need to
## substring starting at 6 and ending at 6 + 32 - 1.  This is only
## true because we use MD5 checksums (it would be different if we used
## SHA1 for example).

CHECKSUM_BEGIN <- 6
CHECKSUM_END <- 6 + 32 - 1


package <- function(cachedir) {
        if(missing(cachedir))
                cachedir <- getConfig("cachedir")
        if(cachedir == ".")
                stop("'cachedir' cannot be '.' when creating a package")
        name <- paste(tempfile(), "zip", sep = ".")

        cwd <- getwd()
        setwd(dirname(cachedir))        
        on.exit(setwd(cwd))

        message("zipping cache directory...")
        cmd <- paste("zip -r -X", name, basename(cachedir))
        out <- system(cmd, intern = TRUE)
        
        checksum <- md5sum(name)
        newname <- paste("./cpkg-", checksum, ".zip", sep = "")
        message(gettextf("creating package '%s'", basename(newname)))

        if(file.exists(newname))
                warning(gettextf("existing package file overwritten",
                                 newname))
        status <- file.copy(name, newname, overwrite = TRUE)

        if(!status)
                warning("problem copying package file")
        invisible(basename(newname))
}

################################################################################

pkgupload <- function(pkgname) {
        if(!require(RCurl))
                stop("'RCurl' required for uploading cache packages")
        message(gettextf("uploading cache package '%s'", pkgname))
        
        out <- postForm("http://10.253.164.24/cgi-bin/cpkg-upload.pl",
                        file1 = normalizePath(pkgname))
        invisible(out)
}
