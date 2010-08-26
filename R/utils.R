## Package files are of the form 'cpkg-<40 char SHA-1 digest>.zip'.
## In order to extract the digest/ID from the file name, we need to
## substring starting at 6 and ending at 6 + 40 - 1.  This is only
## true because we use SHA1 checksums

DIGESTLEN <- 40
CHECKSUM_BEGIN <- 6
CHECKSUM_END <- CHECKSUM_BEGIN + DIGESTLEN - 1

package <- function(cachedir) {
        warning("'package' is deprecated.\nUse 'cachepackage' instead.\n")
        cachepackage(cachedir)
}

cachepackage <- function(cachedir) {
        if(missing(cachedir))
                cachedir <- getConfig("cachedir")
        if(cachedir == ".")
                stop("'cachedir' cannot be '.' when creating a package")
        checkzip <- Sys.which("zip") 
        if(!nzchar(checkzip) || !file.exists(checkzip))
                stop("'zip' utility not found in path")
        name <- paste(tempfile(), "zip", sep = ".")

        cwd <- getwd()
        setwd(dirname(cachedir))        
        on.exit(setwd(cwd))

        message("zipping cache directory...")
        cmd <- paste("zip -r -X", name, basename(cachedir))
        out <- system(cmd, intern = TRUE)

        pkgdigest <- packageDigest(cachedir)
        newname <- paste("./cpkg-", pkgdigest, ".zip", sep = "")
        message(gettextf("creating package '%s'", basename(newname)))

        if(file.exists(newname))
                warning("existing package file overwritten")
        status <- file.copy(name, newname, overwrite = TRUE)

        if(!status)
                warning("problem copying package file")
        else {
                status <- file.remove(name)

                if(!status)
                        warning("problem removing temporary file")
        }
        invisible(basename(newname))
}

packageDigest <- function(cachedir) {
        srcfiles <- file.path(srcdir(cachedir), readLines(file.path(cachedir, "srcfiles")))
        tmp <- tempfile()
        status <- file.copy(file.path(cachedir, "dbfiles"), tmp)

        for(sf in srcfiles)
                status <- status && file.append(tmp, sf)
        if(!status)
                stop("unable to create package digest")
        hashFile(tmp)
}

################################################################################

## pkgupload <- function(pkgname) {
##         if(!require(RCurl))
##                 stop("'RCurl' required for uploading cache packages")
##         message(gettextf("uploading cache package '%s'", pkgname))
## 
##         fileInfo <- fileUpload(normalizePath(pkgname))
##         uploadscript <- getConfig("uploadscript")
##         out <- postForm(uploadscript, cpkgfile = fileInfo)
##         invisible(out)
## }
