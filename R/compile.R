################################################################################
## Copyright (C) 2007, Roger D. Peng <rpeng@jhsph.edu>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA
###############################################################################

logMessage <- function(...) {
        args <- list(...)
        msg <- paste(paste(args, collapse = ""), "\n", sep = "")
        out <- getConfig("logfile")
        cat(msg, file = out, append = TRUE)
}

cacherPlotHook <- function() {
        logMessage("  'plot.new' called; need to force evaluation")
        setConfig("new.plot", TRUE)
}

cacherGridHook <- function() {
        logMessage("  'grid.newpage' called; need to force evaluation")
        setConfig("new.plot", TRUE)
}

setHookFunctions <- function() {
        setConfig("oldPlotHook", getHook("plot.new"))
        setConfig("oldGridHook", getHook("grid.newpage"))
        setHook("plot.new", cacherPlotHook, "append")
        setHook("grid.newpage", cacherGridHook, "append")
}

unsetHookFunctions <- function() {
        setHook("plot.new", getConfig("oldPlotHook"), "replace")
        setHook("grid.newpage", getConfig("oldGridHook"), "replace")
}

vmessage <- function(..., domain = NULL, appendLF = TRUE) {
        verb <- getConfig("verbose")

        if(!is.null(verb) && verb)
                message(..., domain = domain, appendLF = appendLF)
}

################################################################################

setConfig <- function(name, value) {
        allowed <- c("srcfile", "cachedir", "oldPlotHook", "oldGridHook",
                     "new.plot", "metadata", "logfile", "expr.tag",
                     "new.files", "new.objects", "skipcode", "verbose",
                     "exprDeparseWidth", "history", "archive", "uploadscript")
        if(!(name %in% allowed))
                stop(gettextf("invalid config option '%s'", name))
        assign(name, value, .config, inherits = FALSE)
}

getConfig <- function(name) {
        tryCatch({
                get(name, .config, inherits = FALSE)
        }, error = function(e) {
                NULL
        })
}

.config <- new.env(parent = emptyenv())

.history <- new.env(parent = emptyenv())

################################################################################

mkdirs <- function(cachedir) {
        dir.create(cachedir, showWarnings = FALSE)
        dir.create(dbdir(cachedir), showWarnings = FALSE, recursive = TRUE)
        dir.create(srcdir(cachedir), showWarnings = FALSE, recursive = TRUE)
        dir.create(logdir(cachedir), showWarnings = FALSE, recursive = TRUE)
        dir.create(metadir(cachedir), showWarnings = FALSE, recursive = TRUE)
        dir.create(tagdir(cachedir), showWarnings = FALSE, recursive = TRUE)
}

tagdir <- function(cachedir) file.path(cachedir, "tag")
metadir <- function(cachedir) file.path(cachedir, "meta")
logdir <- function(cachedir) file.path(cachedir, "log")
dbdir <- function(cachedir) file.path(cachedir, "db")
srcdir <- function(cachedir) file.path(cachedir, "src")

createLogFile <- function(cachedir, logfile, srcfile) {
        if(is.null(logfile)) {
                logfile <- file.path(logdir(cachedir),
                                     paste(basename(srcfile), "log", sep="."))
                file.create(logfile)
        }
        else if(is.na(logfile))
                logfile <- stderr()
        else
                file.create(logfile)
        setConfig("logfile", logfile)
}

identicalFiles <- function(x, y) {
        ## Are the contents of the two files the same?
        checksum <- c(hashFile(x), hashFile(y))
        identical(checksum[1], checksum[2])
}

copyFileToCache <- function(srcfile, cachedir) {
        dest <- file.path(srcdir(cachedir), basename(srcfile))
        
        if(file.exists(dest) && identicalFiles(srcfile, dest))
                return(dest)
        i <- 0
        dest0 <- dest
        
        while(file.exists(dest)) 
                dest <- paste(dest0, i + 1, sep = ".")
        status <- file.copy(srcfile, dest)

        if(!status)
                warning("unable to copy source file to cache")
        dest
}

################################################################################

cc <- function(expr, cachedir = ".cache", srcfile = NULL, ...) {
        if(!identical(globalenv(), parent.frame())) 
                stop("'cc' must be called from the global environment")
        subexpr <- substitute(expr)
        exprtext <- deparse(subexpr, width.cutoff = 60)

        if(is.null(srcfile))
                srcfile <- file.path(tempdir(), hash(exprtext))
        writeLines(exprtext, srcfile)
        cacher(srcfile, cachedir, ...)
}

cacher <- function(srcfile, cachedir = ".cache", logfile = NULL) {
        exprList <- parse(srcfile, srcfile = NULL)

        if(!file.exists(cachedir)) {
                vmessage(gettextf("creating cache directory '%s'", cachedir))
                mkdirs(cachedir)
        }
        file.create(metafile(srcfile))
        setConfig("metadata", metafile(srcfile))

        createLogFile(cachedir, logfile, srcfile)
        setHookFunctions()
        on.exit(unsetHookFunctions())

        setConfig("cachedir", cachedir)
        setConfig("new.plot", FALSE)

        ## Copy to cache for later use
        srcfile.cache <- copyFileToCache(srcfile, cachedir)
        sourcefile(srcfile.cache)
        updateSrcFileList(srcfile.cache)

        initForceEvalList()

        for(i in seq_along(exprList)) {
                expr <- exprList[i]
                exprStr <- abbreviateExpr(expr)
                msg <- sprintf("%d: %s", i, exprStr)
                logMessage(msg)

                exprFile <- exprFileName(expr, exprList[seq_len(i - 1)])
                runExpression(expr, exprFile)
                writeMetadata(expr, srcfile, exprFile)
        }
        updateDBFileList()
        writeLocaleInfo()
}

################################################################################

writeLocaleInfo <- function() {
        locale <- Sys.getlocale()
        writeLines(locale, file.path(cache(), "locale"))
}

updateDBFileList <- function() {
        cachedir <- cache()
        dbfilelist <- file.path(cachedir, "dbfiles")

        oldlist <- if(!file.exists(dbfilelist))
                character(0)
        else
                readLines(dbfilelist)
        dbfiles <- unique(c(oldlist, dir(dbdir(cachedir))))
        writeLines(dbfiles, dbfilelist)
}

updateSrcFileList <- function(srcfile) {
        cachedir <- cache()
        srcfilelist <- file.path(cachedir, "srcfiles")

        oldlist <- if(!file.exists(srcfilelist))
                character(0)
        else
                readLines(srcfilelist)
        newlist <- unique(c(oldlist, basename(srcfile)))
        writeLines(newlist, srcfilelist)
}

abbreviateExpr <- function(expr) {
        exprWidth <- getConfig("exprDeparseWidth")
        deparse(expr[[1]], width = exprWidth)[1]
}

writeMetadata <- function(expr, srcfile, exprFile) {
        entry <- data.frame(srcfile = srcfile,
                            expr = abbreviateExpr(expr),
                            objects = paste(getConfig("new.objects"),collapse=";"),
                            files = paste(getConfig("new.files"), collapse = ";"),
                            exprID = basename(exprFile),
                            exprHash = hash(expr),
                            forceEval = as.integer(checkForceEvalList(expr)))
        ## cat("writeMetadata:", as.character(entry$exprID), "\n")
        mfile <- getConfig("metadata")
        write.dcf(entry, file = mfile, width = 5000, append = TRUE)
        cat("\n", file = mfile, append = TRUE)  ## Needed for R >= 2.6.0
        invisible(entry)
}

isCached <- function(exprFile) {
        file.exists(exprFile)
}

checkNewPlot <- function() {
        if(newplot <- getConfig("new.plot"))
                setConfig("new.plot", FALSE)
        newplot
}

runExpression <- function(expr, exprFile) {
        ## 'expr' is a single expression, so something like 'a <- 1'
        if(checkForceEvalList(expr)) {
                logMessage("  force expression evaluation")
                setConfig("new.objects", "")

                out <- withVisible({
                        eval(expr, globalenv(), baseenv())
                })
                if(out$visible)
                        print(out$value)
                return(NULL)
        }
        forceEval <- FALSE

        if(!isCached(exprFile)) {
                logMessage("  eval expr and cache")
                keys <- evalAndCache(expr, exprFile)

                new.plot <- checkNewPlot()
                forceEval <- (length(keys) == 0 || new.plot)

                if(forceEval && !checkForceEvalList(expr)) {
                        logMessage("  expression has side effect: ", hash(expr))
                        updateForceEvalList(expr)
                }
        }
        else {
                logMessage("  -- loading expr from cache")
                keys <- cacheLazyLoad(exprFile, globalenv())
        }
        setConfig("new.objects", keys)
        invisible(keys)
}

exprFileName <- function(expr, history) {
        file.path(dbdir(cache()), hashExpr(expr, history))
}

hashFile <- function(filename) {
        if(length(filename) != 1)
                stop("only pass a single file name to 'hashFile'")
        digest(filename, algo = "sha1", file = TRUE)
}

hash <- function(object) {
        digest(object, algo = "sha1")
}

hashExpr <- function(expr, history) {
        srcfile <- sourcefile()
        expr <- deparse(expr, width.cutoff = 60)
        history <- deparse(history, width.cutoff = 60)
        obj <- list(expr, history, srcfile)
        hash(obj)
}

################################################################################

copy2env <- function(keys, fromEnv, toEnv) {
        for(key in keys) {
                assign(key, get(key, fromEnv, inherits = FALSE), toEnv)
        }
}

## Take an environment and return a copy.  Not an exact copy because
## we don't get all keys (not sure why, but for some reason I remember
## that getting all the keys caused problems.

copyEnv <- function(from) {
        env <- new.env(parent = parent.env(from))
        keys <- ls(from, all.names = FALSE)

        for(key in keys) {
                obj <- get(key, from, inherits = FALSE)
                assign(key, obj, env, inherits = FALSE)
        }
        env
}

## Check for new symbols in 'e2' that are not in 'e1'

isNewOrModified <- function(symbolnames, e1, e2) {
        sapply(symbolnames, function(s) {
                in1 <- exists(s, e1, inherits = FALSE)
                in2 <- exists(s, e2, inherits = FALSE)
                is.new <- !in1 && in2
                is.deleted <- in1 && !in2

                if((!in1 && !in2) || is.deleted)
                        FALSE
                else if(is.new)
                        TRUE
                else
                        !identical(get(s, e1, inherits = FALSE),
                                   get(s, e2, inherits = FALSE))
        })
}

## If 'source()' was used (or a similar function like 'set.seed'),
## there may be new symbols in the global environment.  Check for new
## symbols and return a character vector of symbol names.

checkNewSymbols <- function(e1, e2) {
        if(identical(e1, e2))
                return(character(0))
        specials <- c(".Random.seed")

        ## Don't check for names beginning with '.' for now
        allsym <- unique(c(ls(e1), ls(e2), specials))

        use <- isNewOrModified(allsym, e1, e2)
        allsym[use]
}

## Take an expression, evaluate it in a local environment and dump the
## results to a database.  Associate the names of the dumped objects
## with a digest of the expression.  Return a character vector of keys
## that were dumped

## I *think* the 'copyEnv' stuff is okay w.r.t efficiency because the
## objects in 'global1' and 'global2' are never modified and therefore
## do not end up using extra memory.  I don't think the copyEnv stuff
## works with promises.

evalAndCache <- function(expr, exprFile) {
        ## Evaluation environment
        env <- new.env(parent = globalenv())

        ## Make a copy of the global env before evaluating 'expr';
        ## then check 'before' and 'after' to see if anything new has
        ## been created.        
        before <- copyEnv(globalenv())
        out <- withVisible({
                eval(expr, env, globalenv())
        })
        if(out$visible)
                print(out$value)
        after <- copyEnv(globalenv())

        ## Functions like 'source' and 'set.seed' alter the global
        ## environment, so check for new symbols after evaluation.        
        new.global <- checkNewSymbols(before, after)
        copy2env(new.global, globalenv(), env)

        ## Get newly assigned object names
        keys <- ls(env, all.names = TRUE)
        keys <- checkNonCacheable(keys, env)

        status <- tryCatch({
                saveWithIndex(keys, exprFile, env)
        }, interrupt = function(cond) {
                file.remove(exprFile)
                cond
        })
        if(inherits(status, "condition"))
                stop(status)
        if(length(keys) > 0)
                copy2env(keys, env, globalenv())
        keys
}

## This is a catch-all function to check for objects that we do not
## want to cache.  Currently, connections are the only things we do
## not cache.  If a non-cacheable object is found, then we actually
## cannot cache the entire expression and the expression needs to be
## evaluated each time.

checkNonCacheable <- function(keys, env) {
        for(k in keys) {
                x <- get(k, env, inherits = FALSE)

                if(inherits(x, "connection"))
                        return(character(0))
        }
        keys
}

################################################################################
## Handling expressions with side effects

forceEvalListFile <- function() {
        file.path(cache(), ".ForceEvalList")
}

updateForceEvalList <- function(expr) {
        con <- file(forceEvalListFile(), "a")
        on.exit(close(con))

        writeLines(hash(expr), con)
}

initForceEvalList <- function() {
        file <- forceEvalListFile()

        ## This is probably not necessary....
        if(!file.exists(file))
                file.create(file)
        invisible(file)
}

checkForceEvalList <- function(expr) {
        exprList <- readLines(forceEvalListFile())
        hash(expr) %in% exprList
}


