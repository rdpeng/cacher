################################################################################
## Code for verifying/checking an analysis

checkobjects <- function(obj, env, checkenv) {
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

################################################################################
## Verify objects against their hashes

verifyObject <- function(num) {
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
		filename <- file.path(cachedir, dbdir(cachedir),
				      meta[i, "exprID"])
		testenv <- new.env(parent = emptyenv())
		cacheLazyLoad(filename, testenv)

		con <- gzfile(filename, "rb")
		tryCatch({
			index <- unserialize(con)
			hash <- unserialize(con)
		}, finally = {
			if(isOpen(con))
				close(con)
		})
		valid <- sapply(seq_along(objects), function(j) {
			obj <- get(objects[j], testenv)
			identical(hash[objects[j]], hash(obj))
		})
		names(valid) <- objects
		check[[i]] <- valid
	}
	use <- !sapply(check, is.null)
	check[use]
}
