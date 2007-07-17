################################################################################
## Tools for visualizing the cache

code <- function(num = NULL, cachedir = ".cache") {
        srcfile <- getConfig("srcfile")

        if(is.null(srcfile))
                stop("set 'srcfile' with 'setConfig'")
        meta <- read.dcf(file.path(cachedir, paste(srcfile, "meta", sep=".")))
        expr <- parse(srcfile)
        srcref <- attr(expr, "srcref")

        if(is.null(num)) {
                index <- paste(seq_len(nrow(meta)), meta[, "expr"], sep = "  ")
                writeLines(index)
        }
        else {
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
        }                
}
