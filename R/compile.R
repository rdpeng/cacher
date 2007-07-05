######################################################################
## Copyright (C) 2006, Roger D. Peng <rpeng@jhsph.edu>
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
#####################################################################

######################################################################
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
## do not end up using extra memory.

evalAndCache <- function(expr, exprFile, cache = TRUE) {
        env <- new.env(parent = globalenv())
        global1 <- copyEnv(globalenv())
        
        eval(expr, env)

        global2 <- copyEnv(globalenv())

        ## Functions like 'source' and 'set.seed' alter the global
        ## environment, so check after evaluation
        new.global <- checkNewSymbols(global1, global2)
        copy2env(new.global, globalenv(), env)

        ## Get newly assigned object names
        keys <- ls(env, all.names = TRUE)

        if(length(keys) == 0 && !checkForceEvalList(expr)) {
                ## message("expression has side effect: ", digest(expr))
                updateForceEvalList(expr)
        }
        if(cache) 
                saveWithIndex(keys, exprFile, env)
        env
}

exprFileName <- function(cachedir, options, exprDigest) {
        chunkdir <- makeChunkDirName(cachedir, options)
        file.path(chunkdir, exprDigest)
}

makeChunkDirName <- function(cachedir, options) {
        file.path(cachedir, paste(options$label, options$chunkDigest,
                                  sep = "_"))
}

################################################################################
## Handling expressions with side effects

sideEffectListFile <- function() {
        file.path(getCacheDir(), ".ForceEvalList")
}

updateForceEvalList <- function(expr) {
        exprDigest <- digest(expr)
        con <- file(sideEffectListFile(), "a")
        on.exit(close(con))
        
        writeLines(exprDigest, con)
}

initForceEvalList <- function() {
        file <- sideEffectListFile()

        ## This is probably not necessary....
        if(!file.exists(file))
                file.create(file)
        invisible(file)
}

checkForceEvalList <- function(expr) {
        exprDigest <- digest(expr)
        exprList <- readLines(sideEffectListFile())
        exprDigest %in% exprList
}

################################################################################
## The major modification is here: Rather than evaluate expressions
## and leave them in the global environment, we evaluate them in a
## local environment (that has globalenv() as the parent) and then
## store the assignments in a database.  If an expression does not
## give rise to new R objects, then nothing is saved.

cacheSweaveEvalWithOpt <- function (expr, options) {
        ## 'expr' is a single expression, so something like 'a <- 1'
        res <- NULL
        
        if(!options$eval)
                return(res)
        if(options$cache && !checkForceEvalList(expr)) {
                cachedir <- getCacheDir()
                chunkdir <- makeChunkDirName(cachedir, options)

                if(!file.exists(chunkdir))
                        dir.create(chunkdir, recursive = TRUE)
                exprDigest <- digest(expr, algo = "md5")
                exprFile <- exprFileName(cachedir, options, exprDigest)

                ## If the current expression is not cached, then
                ## evaluate the expression and dump the resulting
                ## objects to the database.
                res <- if(!file.exists(exprFile)) {
                        try({
                                withVisible({
                                        evalAndCache(expr, exprFile)
                                })
                        }, silent = TRUE)
                }
                else  
                        NULL  ## load from cache

                ## (If there was an error then just return the
                ## condition object and let Sweave deal with it.)
                if(inherits(res, "try-error"))
                        return(res)
                
                lazyLoad(exprFile, globalenv())
        }
        else {
                res <- try({
                        withVisible({
                                eval(expr, globalenv(), baseenv())
                        })
                }, silent = TRUE)
                if(inherits(res, "try-error"))
                        return(res)
                ## copy2env(ls(env, all.names = TRUE), env, globalenv())
        }
        if(!is.null(res) && (options$print | (options$term & res$visible)))
                print(res$value)
        res
}


makeMapFileName <- function(Rnwfile) {
        mapfile <- sub("\\.Rnw$", "\\.map", Rnwfile)

        ## Don't clobber
        if(identical(mapfile, Rnwfile))
                mapfile <- paste(Rnwfile, "map", sep = ".")
        mapfile
}

writeChunkMetadata <- function(object, chunk, options) {
        chunkprefix <- utils::RweaveChunkPrefix(options)
        chunkexps <- parse(text = chunk)
        chunkDigest <- digest(chunkexps, algo = "md5")
        options$chunkDigest <- chunkDigest
        
        ## If there's a data map file then write the chunk name and the
        ## directory of the chunk database to the map file (in DCF format)
        dbName <- if(options$cache)
                makeChunkDirName(getCacheDir(), options)
        else
                ""
        ## Capture figure filenames; default to PDF, otherwise use EPS.
        ## Filenames are <chunkprefix>.<extension>, which could change in
        ## the future depending on Sweave implementation details
        figname <- ""
        if(options$fig && options$eval) {
                figname <- if(options$pdf)
                        paste(chunkprefix, "pdf", sep = ".")
                else if(options$eps)
                        paste(chunkprefix, "eps", sep = ".")
                else
                        ""
        }
        ## Write out map file entry
        mapFile <- object[["mapFile"]]
        mapEntry <- data.frame(chunk = options$label,
                               chunkprefix = chunkprefix,
                               chunkDigest = chunkDigest,
                               fig = figname,
                               cacheDB = dbName,
                               time = Sys.time())
        write.dcf(mapEntry, file = mapFile, append = TRUE, width = 2000)
        options
}
































