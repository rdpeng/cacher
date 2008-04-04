################################################################################
## Tools for exploring the cache

cache <- function() {
        getConfig("cachedir")
}

deletecache <- function(cachedir = NULL) {
        if(is.null(cachedir))
                cachedir <- cache()
        if(!is.character(cachedir) || is.null(cachedir))
                stop("cache directory not found")
        unlink(cachedir, recursive = TRUE)
}

sourcefile <- function(srcfile = NULL) {
        cachedir <- cache()

        ## Get it
        if(is.null(srcfile)) {
                sf <- getConfig("srcfile")

                if(is.null(sf))
                        return(sf)
                else
                        return(basename(sf))
        }
        ## Set it
        cache.srcfile <- file.path(srcdir(cachedir), basename(srcfile))

        if(file.exists(cache.srcfile))
                setConfig("srcfile", cache.srcfile)
        else
                stop(gettextf("source file '%s' not in cache directory",
                              srcfile))
        invisible(basename(cache.srcfile))
}

showfiles <- function() {
        cachedir <- cache()
        sf <- tryCatch({
                readLines(file.path(cachedir, "srcfiles"))
        }, condition = function(cond) {
                NULL
        })
        sf
}

showExpressions <- function(num, srcref) {
        tfile <- tempfile()
        con <- file(tfile, "w")
        on.exit(close(con))

        skip <- skipcode()
        srcfile <- getConfig("srcfile")
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

showcode <- function() {
        srcfile <- checkSourceFile()
        file.show(srcfile)
}

checkSourceFile <- function() {
        srcfile <- getConfig("srcfile")
        
        if(!is.null(srcfile))
                srcfile
        else if(is.null(srcfile) && length(showfiles()) == 1) {
                sourcefile(showfiles())
                getConfig("srcfile")
        }
        else {
                available.files <- showfiles()
                stop("set source file with 'sourcefile'; ",
                     "available files are\n\t",
                     paste(available.files, collapse = ", "))
        }
}

code <- function(num = NULL, full = FALSE) {
        srcfile <- checkSourceFile()
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
        srcfile <- checkSourceFile()

        meta <- read.dcf(metafile(srcfile))

        if(missing(num))
                num <- seq_len(nrow(meta))
        obj <- strsplit(meta[num, "objects"], ";", fixed = TRUE)
        unique(unlist(obj))
}

loadcache <- function(num, env = parent.frame()) {
        cachedir <- cache()
        srcfile <- checkSourceFile()
        
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
        force(env)
        cachedir <- cache()
        srcfile <- checkSourceFile()
        
        meta <- read.dcf(metafile(srcfile))
        exprList <- parse(srcfile)
        forceEval <- as.logical(as.numeric(meta[, "forceEval"]))

        if(missing(num))
                num <- seq_len(nrow(meta))
        skip <- skipcode()

        if(is.null(skip))
                skip <- numeric(0)
        for(i in num) {
                if(i %in% skip) {
                        vmessage("skipping expression ", i)
                        next
                }
                if(!forceEval[i] && !forceAll) {
                        vmessage("loading cache for expression ", i)
                        loadcache(i, env)
                        next
                }
                expr <- exprList[i]
                vmessage("evaluating expression ", i)

                status <- tryCatch({
                        withVisible(eval(expr, env, globalenv()))
                }, error = function(err) {
                        vmessage("ERROR: unable to evaluate expression")
                        vmessage(conditionMessage(err))
                        err
                })
                if(!inherits(status, "condition") && status$visible)
                        print(status$value)
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
