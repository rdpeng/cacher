################################################################################
## Tools for exploring the cache

cache <- function(dir = NULL) {
        if(is.null(dir))
                getConfig("cachedir")
        else
                setConfig("cachedir", dir)
}

sourcefile <- function(srcfile = NULL) {
        cachedir <- cache()

        ## Get it
        if(is.null(srcfile)) {
                sf <- getConfig("srcfile")
                return(sf)
        }
        ## Set it
        cache.srcfile <- file.path(srcdir(cachedir), srcfile)

        if(file.exists(cache.srcfile))
                setConfig("srcfile", cache.srcfile)
        else
                stop(gettextf("source file '%s' not in cache directory",
                              srcfile))
}

showfiles <- function() {
        cachedir <- cache()
        sf <- readLines(file.path(cachedir, "srcfiles"))
        sf
}

showExpressions <- function(num, srcref) {
        tfile <- tempfile()
        con <- file(tfile, "w")
        on.exit(close(con))

        skip <- skipcode()
        srcfile <- sourcefile()
        writeLines(paste("source file:", basename(srcfile)), con)

        for(i in num) {
                expr <- as.character(srcref[[i]])
                exprnum <- as.character(i)

                if(i %in% skip)
                        exprnum <- paste(exprnum, "*", sep = "")
                if(length(expr) > 1) {
                        indent <- c(exprnum,
                                    rep(paste(rep(" ", nchar(exprnum)),
                                              collapse = ""),
                                        length(expr) - 1))
                }
                else
                        indent <- exprnum
                writeLines(paste(indent, expr, sep = "  "), con)
        }
        close(con)
        on.exit()
        file.show(tfile)
}

metafile <- function(srcfile) {
        cachedir <- cache()
        file.path(metadir(cachedir),
                  paste(basename(srcfile), "meta", sep = "."))
}

code <- function(num = NULL, full = FALSE) {
        srcfile <- sourcefile()

        if(is.null(srcfile))
                stop("set source file with 'sourcefile'; use 'showfiles()' to see available files")
        cachedir <- cache()
        exprList <- parse(srcfile)

        if(is.null(num))
                num <- seq_len(length(exprList))
        if(!full) {
                expr.print <- sapply(exprList, function(x) {
                        deparse(x, width = getConfig("exprDeparseWidth"))[1]
                })
        }
        else
                expr.print <- attr(exprList, "srcref")
        showExpressions(num, expr.print)
}

showobjects <- function(num) {
        cachedir <- cache()
        srcfile <- sourcefile()

        if(is.null(srcfile))
                stop("set 'srcfile' with 'setConfig'")
        meta <- read.dcf(metafile(srcfile))

        if(missing(num))
                num <- seq_len(nrow(meta))
        obj <- strsplit(meta[num, "objects"], ";", fixed = TRUE)
        unique(unlist(obj))
}

loadcache <- function(num, env = parent.frame()) {
        cachedir <- cache()
        srcfile <- sourcefile()
        meta <- read.dcf(metafile(srcfile))

        if(missing(num))
                num <- seq_len(nrow(meta))
        out <- vector("list", length = length(num))

        for(i in num) {
                if(as.integer(meta[i, "forceEval"]))
                        next
                cacheFile <- file.path(dbdir(cachedir), meta[i, "exprID"])
                out[[i]] <- cacheLazyLoad(cacheFile, env)
        }
        invisible(unique(unlist(out)))
}

runcode <- function(num, env = parent.frame(), forceAll = FALSE) {
        cachedir <- cache()
        srcfile <- sourcefile()

        if(is.null(srcfile))
                stop("set 'srcfile' with 'setConfig'")
        meta <- read.dcf(metafile(srcfile))
        exprList <- parse(srcfile)
        forceEval <- as.logical(as.numeric(meta[, "forceEval"]))
        skip <- skipcode()

        if(is.null(skip))
                skip <- numeric(0)
        for(i in num) {
                if(i %in% skip) {
                        message("skipping expression ", i)
                        next
                }
                if(!forceEval[i] && !forceAll) {
                        message("loading cache for expression ", i)
                        loadcache(i, env)
                }
                else {
                        expr <- exprList[i]
                        message("evaluating expression ", i)
                        tryCatch({
                                eval(expr, env, globalenv())
                        }, error = function(err) {
                                message("ERROR: unable to evaluate expression")
                                message(conditionMessage(err))
                        })
                }
        }
}

skipcode <- function(num, append = TRUE) {
        if(missing(num))
                getConfig("skipcode")
        else if(is.null(num) || !append)
                setConfig("skipcode", num)
        else {
                current <- getConfig("skipcode")
                setConfig("skipcode", sort(unique(c(current, num))))
        }
}
