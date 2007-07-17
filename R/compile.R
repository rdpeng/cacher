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

library(digest)

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

getFileList <- function() {
        fileList <- suppressWarnings({
                n <- dir(recursive = TRUE, full.names = TRUE, all.files = TRUE)
                normalizePath(n)
        })
        cachedir <- normalizePath(getConfig("cachedir"))
        exclude <- grep(cachedir, fileList, fixed = TRUE)
        fileList[-exclude]
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

setConfig <- function(name, value) {
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

################################################################################

cacher <- cc <- function(file, cachedir = ".cache", logfile = NA,
                         window.size = Inf) {
        exprList <- parse(file, srcfile = NULL)
        
        dir.create(cachedir, showWarnings = FALSE)
        metadata <- file.path(cachedir, paste(file, "meta", sep = "."))
        file.create(metadata)
        
        if(is.na(logfile)) {
                logfile <- file.path(cachedir, paste(file,"log",sep="."))
                file.create(logfile)
        }
        else if(is.null(logfile))
                logfile <- stderr()
        else
                file.create(logfile)
        setHookFunctions()
        on.exit(unsetHookFunctions())

        setConfig("cachedir", cachedir)
        setConfig("metadata", metadata)
        setConfig("new.plot", FALSE)
        setConfig("logfile", logfile)
        setConfig("file", file)
        setConfig("fileList", getFileList())

        initForceEvalList()

        for(i in seq_along(exprList)) {
                expr <- exprList[i]
                exprStr <- deparse(expr[[1]],width=getConfig("exprDeparseWidth"))[1]
                msg <- sprintf("%d: %s", i, exprStr)
                logMessage(msg)
                window.idx <- seq.int(max(0, i - window.size), i - 1)
                setConfig("history", exprList[window.idx])

                runExpression(expr)
                writeMetadata(expr)
        }
}

################################################################################

writeMetadata <- function(expr) {
        exprWidth <- getConfig("exprDeparseWidth")
        
        entry <- data.frame(codefile = getConfig("file"),
                            expr = deparse(expr[[1]], width = exprWidth)[1],
                            objects = paste(getConfig("new.objects"),collapse=";"),
                            files = paste(getConfig("new.files"), collapse = ";"),
                            exprID = hashExpr(expr, getConfig("history")),
                            exprHash = hash(expr),
                            forceEval = as.integer(checkForceEvalList(expr)),
                            time = Sys.time())
        write.dcf(entry, file = getConfig("metadata"), append = TRUE, width = 5000)
        invisible(entry)
}

isCached <- function(exprFile) {
        file.exists(exprFile)
}

checkNewFiles <- function() {
        current <- getFileList()
        newfiles <- setdiff(current, getConfig("fileList"))
        files.new <- length(newfiles) > 0

        if(files.new) {
                logMessage("  expression created file(s) ",
                           paste(newfiles, collapse = ", "))
                setConfig("fileList", c(getConfig("fileList"), newfiles))
                setConfig("new.files", newfiles)
        }
        else
                setConfig("new.files", "")
        files.new
}

checkNewPlot <- function() {
        if(newplot <- getConfig("new.plot")) 
                setConfig("new.plot", FALSE)
        newplot
}

runExpression <- function(expr) {
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
        exprFile <- exprFileName(expr)
        
        if(!isCached(exprFile)) {
                logMessage("  eval expr and cache")
                keys <- evalAndCache(expr, exprFile)
                
                newfiles <- checkNewFiles()
                newplot <- checkNewPlot()

                forceEval <- (length(keys) == 0 || newplot || newfiles)
                
                if(forceEval && !checkForceEvalList(expr)) {
                        logMessage("  expression has side effect: ", hash(expr))
                        updateForceEvalList(expr)
                }
        }
        logMessage("  -- loading expr from cache")
        objects <- cacheLazyLoad(exprFile, globalenv())
        setConfig("new.objects", objects)
        invisible(objects)
}

exprFileName <- function(expr) {
        file.path(getConfig("cachedir"), hashExpr(expr, getConfig("history")))
}

hash <- function(object) {
        digest(object, algo = "sha1")
}

hashExpr <- function(expr, history) {
        obj <- list(expr, history)
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

## If 'source()' was used, there may be new symbols in the global
## environment, unless 'source(local = TRUE)' was used.  Also applies
## for 'set.seed()'.
        
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
        env <- new.env(parent = globalenv())
        before <- copyEnv(globalenv())
        out <- withVisible({
                eval(expr, env, globalenv())
        })
        if(out$visible)
                print(out$value)
        after <- copyEnv(globalenv())

        ## Functions like 'source' and 'set.seed' alter the global
        ## environment, so check after evaluation
        new.global <- checkNewSymbols(before, after)
        copy2env(new.global, globalenv(), env)

        ## Get newly assigned object names
        keys <- ls(env, all.names = TRUE)

        saveWithIndex(keys, exprFile, env)
        keys
}

################################################################################
## Handling expressions with side effects

forceEvalListFile <- function() {
        file.path(getConfig("cachedir"), ".ForceEvalList")
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

################################################################################


makeMapFileName <- function(Rnwfile) {
        mapfile <- sub("\\.Rnw$", "\\.map", Rnwfile)

        ## Don't clobber
        if(identical(mapfile, Rnwfile))
                mapfile <- paste(Rnwfile, "map", sep = ".")
        mapfile
}

