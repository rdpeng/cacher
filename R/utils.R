package <- function(cachedir) {
        if(missing(cachedir))
                cachedir <- getConfig("cachedir")
        if(cachedir == ".")
                stop("'cachedir' cannot be '.' when creating a package")
        name <- paste(tempfile(), "zip", sep = ".")

        cwd <- getwd()
        setwd(dirname(cachedir))        
        on.exit(setwd(cwd))
        
        cmd <- paste("zip -r -X", name, basename(cachedir))
        out <- system(cmd, intern = TRUE)
        
        checksum <- md5sum(name)
        newname <- paste("./cpkg-", checksum, ".zip", sep = "")
        message(gettextf("creating package '%s'", basename(newname)))

        if(file.exists(newname))
                warning(gettextf("existing package file '%s' overwritten",
                                 newname))
        status <- file.copy(name, newname, overwrite = TRUE)

        if(!status)
                warning("problem copying package file")
        invisible(out)
}



################################################################################

## For administrators only

pkgunzip <- function(filename, pkgdir = "~/projects/rr-website/html/packages") {
        tdir <- tempfile()
        message("creating temp directory ", tdir)

        if(!dir.create(tdir))
                stop("unable to create temporary directory")

        message("copying package to temp directory")
        file.copy(filename, tdir)

        ## Get ID from filename
        id <- substring(filename, 6, 6 + 32 - 1)

        cwd <- getwd()
        setwd(tdir)
        on.exit(setwd(cwd))

        if(length(dir()) != 1)
                stop("temp directory should only contain one file")
        message("unzipping package file")
        system(paste("unzip -q", basename(filename)))
        newdir <- setdiff(dir(all.files = TRUE), c(filename, ".", ".."))
        newdir <- paste(newdir, "/", sep = "")

        destdir <- file.path(pkgdir, getIDdir(id))
        message("creating destination directory ", destdir)
        dir.create(destdir, recursive = TRUE, showWarnings = FALSE)

        message("rsyncing files")
        cmd <- paste("rsync -a", newdir, destdir)
        system(cmd)
}
