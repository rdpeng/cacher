## Package files are of the form 'cpkg-<40 char checksum>.zip'.  In
## order to extract the checksum/ID from the file name, we need to
## substring starting at 6 and ending at 6 + 40 - 1.  This is only
## true because we use SHA1 checksums

DIGESTLEN <- 40
CHECKSUM_BEGIN <- 6
CHECKSUM_END <- CHECKSUM_BEGIN + DIGESTLEN - 1


package <- function(cachedir) {
        if(missing(cachedir))
                cachedir <- getConfig("cachedir")
        if(cachedir == ".")
                stop("'cachedir' cannot be '.' when creating a package")
        if(!nzchar(Sys.which("zip")))
                stop("'zip' utility not found in path")
        name <- paste(tempfile(), "zip", sep = ".")

        cwd <- getwd()
        setwd(dirname(cachedir))        
        on.exit(setwd(cwd))

        message("zipping cache directory...")
        cmd <- paste("zip -r -X", name, basename(cachedir))
        out <- system(cmd, intern = TRUE)
        
        checksum <- hashFile(name)
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

        fileInfo <- fileUpload(normalizePath(pkgname))
        uploadscript <- getConfig("uploadscript")
        out <- postForm(uploadscript, cpkgfile = fileInfo)
        invisible(out)
}
