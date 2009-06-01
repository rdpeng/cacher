################################################################################
## Code for verifying/checking an analysis

compare_objects <- function(obj, env, checkenv) {
        if(length(obj) == 0) {
                message("= no objects to check, OK")
                return(NULL)
        }
        test <- logical(length(obj))

        for(j in seq_along(obj)) {
                objnew <- get(obj[j], env, inherits = FALSE)
                objcheck <- get(obj[j], checkenv, inherits = FALSE)
                testmsg <- all.equal(objnew, objcheck)
                test[j] <- isTRUE(testmsg)
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
                message(paste("-", testmsg, collapse = "\n"))
        }
}

checkcode <- function(num, env = globalenv()) {
        cachedir <- cache()
        srcfile <- checkSourceFile()

        meta <- read.dcf(metafile(srcfile))
        forceEval <- as.logical(as.numeric(meta[, "forceEval"]))
        exprList <- parse(srcfile)

        if(missing(num))
                num <- seq_len(nrow(meta))
        tempout <- tempfile()
        on.exit(file.remove(tempout))

        for(i in num) {
                expr <- exprList[i]

                if(forceEval[i]) {
                        message("evaluating expression ", i)
                        eval(expr, env)
                        next
                }
                message("checking expression ", i)
                checkenv <- new.env(parent = emptyenv())
                loadcache(i, checkenv)

                status <- tryCatch({
                        capture.output({
                                eval(expr, env)
                        }, file = tempout)
                }, condition = function(cond) {
                        message("- problem evaluating expression, FAILED")
                        msg <- conditionMessage(cond)

                        if(length(msg) > 0) {
                                prntmsg <- gettextf("%s: %s", class(cond)[1],
                                                    msg)
                                prntmsg <- paste(strwrap(prntmsg, prefix = "- "),
                                                 collapse = "\n")
                                vmessage(prntmsg)
                        }
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
                        compare_objects(obj, env, checkenv)
                }
        }
}


forceDownload <- function(objectList, env) {
        for(obj in objectList) {
                get(obj, env, inherits = FALSE)
        }
}

################################################################################
## Verify objects against their hashes

checkobjects <- function(num) {
        cachedir <- cache()
        srcfile <- checkSourceFile()
        meta <- read.dcf(metafile(srcfile))

        if(missing(num))
                num <- seq_len(nrow(meta))
        check <- vector("list", length = length(num))

        for(i in num) {
                objects <- strsplit(meta[i, "objects"], ";", fixed = TRUE)[[1]]

                if(length(objects) == 0)
                        next
                vmessage("checking expression ", i)

                filename <- file.path(dbdir(cachedir), meta[i, "exprID"])

                if(!file.exists(filename))
                        transferCacheFile(filename, cachedir)
                testenv <- new.env(parent = emptyenv())

                con <- gzfile(filename, "rb")                
                status <- tryCatch({
                        cacheLazyLoad(filename, testenv)
                        index <- unserialize(con)
                        hashVector <- unserialize(con)
                        TRUE
                }, error = function(cond) {
                        vmessage("- problem checking objects")
                        cond
                }, finally = {
                        if(isCloseable(con))
                                close(con)
                })
                if(inherits(status, "condition")) {
                        check[[i]] <- logical(length(objects))
                        next
                }
                valid <- sapply(objects, function(objname) {
                        obj <- get(objname, testenv, inherits = FALSE)
                        stored_hash <- as.character(hashVector[objname])
                        v <- identical(stored_hash, hash(obj))

                        msg <- gettextf("%s object '%s' %s",
                                        ifelse(v, "+", "-"), objname,
                                        ifelse(v, "OK", "not verified"))
                        vmessage(msg)
                        v
                })
                names(valid) <- objects
                check[[i]] <- valid
        }
        use <- !sapply(check, is.null)
        check <- check[use]

        message(sprintf("%d of %d objects verified", sum(unlist(check)),
                        length(unlist(check))))
        invisible(check[use])
}
