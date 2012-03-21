################################################################################
## Tools for exploring the cache

cache <- function() {
        getConfig("cachedir")
}

setcache <- function(dir) {
        setConfig("cachedir", dir)
        invisible(dir)
}

deletecache <- function(cachedir = NULL) {
        if(is.null(cachedir))
                cachedir <- cache()
        if(!is.character(cachedir) || is.null(cachedir))
                stop("cache directory not found")
        unlink(cachedir, recursive = TRUE)
        setConfig("srcfile", NULL)
}

sourcefile <- function(srcfile = NULL) {
        cachedir <- cache()

        ## Get it
        if(is.null(srcfile)) {
                sf <- getConfig("srcfile")
                rv <- ifelse(is.null(sf), sf, basename(sf))
                return(rv)
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


truncateLine <- function(line, width = getOption("width")) {
        if(nchar(line) > width && width > 20)
                line <- paste(substr(line, 1, width - 3), "...", sep = "")
        line
}

showExpressions <- function(num, srcref, full = FALSE) {
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
                line <- paste(indent, expr, sep = "  ")

                if(!full)
                        line <- truncateLine(line[1])
                writeLines(line, con)
        }
        close(con)
        on.exit()
        file.show(tfile)
}

code <- function(num = NULL, full = FALSE) {
        srcfile <- checkSourceFile()
        exprList <- parse(srcfile)

        if(is.null(num))
                num <- seq_len(length(exprList))
        if(!full) {
                expr.print <- sapply(exprList, function(x) {
                        deparse(x, width.cutoff = getConfig("exprDeparseWidth"))[1]
                })
        }
        else {
                srcref <- attr(exprList, "srcref")
                sf <- srcfile(srcfile)
                expr.print <- lapply(seq_along(srcref), function(i) {
                        n <- srcref[[i]]
                        getSrcLines(sf, n[1], n[3])
                })
        }
        showExpressions(num, expr.print, full)
        invisible(exprList[num])
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
        
        if(!is.null(srcfile) && file.exists(srcfile))
                srcfile
        else {
                available.files <- showfiles()

                if(length(available.files) == 1) {
                        sourcefile(available.files)
                        getConfig("srcfile")
                }
                else if(length(available.files) > 1) {
                        stop("set source file with 'sourcefile'; ",
                             "available files are\n\t",
                             paste(available.files, collapse = ", "))
                }
                else
                        stop("no source files available")
        }
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

################################################################################
## Use CodeDepends stuff

readDoc <- function() {
        if(!require(CodeDepends))
                stop("need 'CodeDepends' package to graph code")
        srcfile <- checkSourceFile()
        doc <- readScript(srcfile, type = "R")
        doc
}

graphcode <- function(num, ...) {
        if(!require(Rgraphviz))
                stop("need 'Rgraphviz' package to graph code")
        doc <- readDoc()

        if(missing(num))
                num <- seq_along(doc)
        gr <- makeVariableGraph(frags = doc[num])
        plot(gr, ...)
}

## Show code that leads to an object

objectcode <- function(name, num, show = TRUE) {
        doc <- readDoc()

        if(missing(num))
                num <- seq_along(doc)
        info <- as(doc[num], "ScriptInfo")
        idxlist <- lapply(name, function(n) {
                getDependsThread(n, info)
        })
        idx <- sort.int(unique(unlist(idxlist)))

        if(!length(idx))
                return(invisible(NULL))
        if(show)
                code(idx, full = TRUE)
        invisible(idx)
}

## Evaluate code leading to an object

evalobject <- function(name, num, env = parent.frame(), ...) {
        idx <- objectcode(name, num, show = FALSE)
        runcode(idx, env, ...)
}

loadobject <- function(name, num, env = parent.frame()) {
        idx <- objectcode(name, num, show = FALSE)
        i <- max(idx)
        vmessage("loading cache for expression ", i)
        loadcache(i, env)
}
