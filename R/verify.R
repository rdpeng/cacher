checkobjects <- function(obj, env, checkenv) {
        if(length(obj) == 0) {
                message("++ no objects to check, OK")
                return(NULL)
        }
        test <- logical(length(obj))

        for(j in seq_along(obj)) {
                test[j] <- isTRUE(all.equal(get(obj[j], env),
                                            get(obj[j], checkenv)))
        }
        if(all(test))
                message(sprintf(ngettext(length(obj),
                                         "++ object %s OK",
                                         "++ objects %s OK"),
                                paste(sQuote(obj), collapse = ", ")))
        else {
                message("-- FAILED")
                failed <- which(!test)
                msg <- sprintf(ngettext(sum(failed),
                                        "-- object %s not verified",
                                        "-- objects %s not verified"),
                               paste(sQuote(obj[failed]), collapse = ", "))
                message(msg)
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
        checkenv <- new.env(parent = emptyenv())
        loadcache(num, checkenv)
        
        for(i in num) {
                expr <- exprList[i]
                message("checking expression ", i, "")
                
                status <- tryCatch({
                        eval(expr, env, globalenv())
                }, error = function(err) {
                        message("-- unable to evaluate expression, FAILED")
                        message(gettextf("-- error: '%s'", conditionMessage(err)))

                        if(!forceEval[i]) {
                                message("-- loading cache for expression ", i)
                                loadcache(i, env)
                        }
                        err
                })
                if(!inherits(status, "condition")) {
                        obj <- strsplit(meta[i, "objects"], ";",
                                        fixed = TRUE)[[1]]
                        checkobjects(obj, env, checkenv)
                }
        }
}


