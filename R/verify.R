################################################################################
## Code for verifying/checking an analysis

checkobjects <- function(obj, env, checkenv) {
        if(length(obj) == 0) {
                message("= no objects to check, OK")
                return(NULL)
        }
        test <- logical(length(obj))

        for(j in seq_along(obj)) {
                test[j] <- isTRUE(all.equal(get(obj[j], env),
                                            get(obj[j], checkenv)))
        }
        if(all(test))
                message(sprintf(ngettext(length(obj),
                                         "+ object %s OK",
                                         "+ objects %s OK"),
                                paste(sQuote(obj), collapse = ", ")))
        else {
                failed <- which(!test)
                msg <- ngettext(sum(failed),
                                "- object %s not verified, FAILED",
                                "- objects %s not verified, FAILED")
                message(sprintf(msg, paste(sQuote(obj[failed]), collapse=", ")))
        }
}

checkcode <- function(num, env = parent.frame()) {
        cachedir <- cache()
        srcfile <- checkSourceFile()

        meta <- read.dcf(metafile(srcfile))
        forceEval <- as.logical(as.numeric(meta[, "forceEval"]))
        exprList <- parse(srcfile)

        if(missing(num))
                num <- seq_len(nrow(meta))
        
        for(i in num) {
                expr <- exprList[i]
                message("checking expression ", i, "")
                checkenv <- new.env(parent = emptyenv())
                loadcache(i, checkenv)
                
                status <- tryCatch({
                        eval(expr, env, globalenv())
                }, condition = function(cond) {
                        message("- problem evaluating expression, FAILED")
                        msg <- conditionMessage(cond)

                        if(length(msg) > 0)
                                vmessage(gettextf("- %s: '%s'",
                                                 class(cond)[1], msg))
                        if(!forceEval[i]) {
                                vmessage("- loading objects from cache")
                                obj <- loadcache(i, env)
                                forceDownload(obj, env)
                        }
                        cond
                })
                if(!inherits(status, "condition")) {
                        obj <- strsplit(meta[i, "objects"], ";",
                                        fixed = TRUE)[[1]]
                        checkobjects(obj, env, checkenv)
                }
        }
}


forceDownload <- function(objectList, env) {
        for(obj in objectList) {
                get(obj, env, inherits = FALSE)
        }
}
