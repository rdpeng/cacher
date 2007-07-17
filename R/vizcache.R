################################################################################
## Tools for visualizing the cache

code <- function(num = NULL, srcfile, cachedir = ".cache") {
        if(missing(srcfile))
                srcfile <- getConfig("srcfile")
        if(is.null(srcfile))
                stop("set 'srcfile' with 'setConfig'")
        meta <- read.dcf(file.path(cachedir, paste(srcfile, "meta", sep=".")))
        exprList <- parse(srcfile)
        srcref <- attr(exprList, "srcref")

        if(is.null(num)) {
                index <- paste(seq_len(nrow(meta)), meta[, "expr"], sep = "  ")
                writeLines(index)
                return(invisible(index))
        }
        ## 'num' is an integer vector
        for(i in num) {
                expr <- as.character(srcref[[i]])
                if(length(expr) > 1) {
                        exprnum <- as.character(i)
                        indent <- c(exprnum,
                                    rep(paste(rep(" ", nchar(exprnum)),
                                              collapse = ""),
                                        length(expr) - 1))
                }
                else
                        indent <- as.character(i)
                writeLines(paste(indent, expr, sep = "  "))
        }
        invisible(exprList[num])
}

runcode <- function(num, env = parent.frame(), cachedir = ".cache",
                    forceAll = FALSE) {
        srcfile <- getConfig("srcfile")
        
        if(is.null(srcfile))
                stop("set 'srcfile' with 'setConfig'")
        meta <- read.dcf(file.path(cachedir, paste(srcfile, "meta", sep=".")))
        exprList <- parse(srcfile)
        forceEval <- as.logical(as.numeric(meta[, "forceEval"]))
        skip <- getConfig("skipcode")

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
