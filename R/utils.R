package <- function(name, srcfile, cachedir) {
        if(missing(srcfile))
                srcfile <- getConfig("srcfile")
        if(missing(cachedir))
                cachedir <- getConfig("cachedir")
        if(length(grep("\\.zip$", name, perl = TRUE)) == 0)
                name <- paste(name, "zip", sep = ".")
        message(gettextf("creating zip file '%s'", name))

        cmd <- paste("zip -r", name, cachedir, srcfile)
        out <- system(cmd, intern = TRUE)
        invisible(out)
}
