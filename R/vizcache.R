################################################################################
## Tools for exploring the cache

showExpressions <- function(num, srcref) {
        tfile <- tempfile()
        con <- file(tfile, "w")
        on.exit(close(con))
        
        skip <- skipcode()

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

code <- function(num = NULL) {
        cachedir <- getConfig("cachedir")
        srcfile <- getConfig("srcfile")
        
        if(is.null(srcfile))
                stop("set 'srcfile' with 'setConfig'")
        exprList <- parse(srcfile)
        
        if(is.null(num)) {
                expr.print <- sapply(exprList, function(x) {
                        deparse(x, width = getConfig("exprDeparseWidth"))[1]
                })
                num <- seq_len(length(exprList))
        }
        else
                expr.print <- attr(exprList, "srcref")
        showExpressions(num, expr.print)
}

showobjects <- function(num) {
        cachedir <- getConfig("cachedir")
        srcfile <- getConfig("srcfile")
        meta <- read.dcf(file.path(cachedir, paste(srcfile, "meta", sep = ".")))
        obj <- strsplit(meta[num, "objects"], ";", fixed = TRUE)
        unique(unlist(obj))
}

loadcache <- function(num, env = parent.frame()) {
        cachedir <- getConfig("cachedir")
        srcfile <- getConfig("srcfile")
        meta <- read.dcf(file.path(cachedir, paste(srcfile, "meta", sep=".")))

        if(missing(num))
                num <- seq_len(nrow(meta))
        out <- vector("list", length = length(num))
        
        for(i in num) {
                cacheFile <- file.path(cachedir, meta[i, "exprID"])
                out[[i]] <- cacheLazyLoad(cacheFile, env)
        }
        invisible(unique(unlist(out)))
}

runcode <- function(num, env = parent.frame(), forceAll = FALSE) {
        cachedir <- getConfig("cachedir")
        srcfile <- getConfig("srcfile")
        
        if(is.null(srcfile))
                stop("set 'srcfile' with 'setConfig'")
        meta <- read.dcf(file.path(cachedir, paste(srcfile, "meta", sep=".")))
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
                        cacheFile <- file.path(cachedir, meta[i, "exprID"])
                        cacheLazyLoad(cacheFile, env)
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
