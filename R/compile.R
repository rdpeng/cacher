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
################################################################################

library(digest)

cacher <- function(file, cachedir = ".cache") {
        if(!file.exists(cachedir))
                dir.create(cachedir)
        metadata <- file.path(cachedir, ".exprMetaData")
        file.create(metadata)
        initForceEvalList(config)
        
        config <- list(cachedir = cachedir,
                       metadata = metadata)
        exprList <- parse(file, srcfile = NULL)


        for(i in seq_along(exprList)) {
                cat(i, " ")
                expr <- exprList[i]
                config$history <- exprList[seq_len(i - 1)]
                runExpression(expr, config)

                writeMetadata(expr, config)
        }
}
cc <- cacher

################################################################################

writeMetadata <- function(expr, config) {
        entry <- data.frame(exprID = hashExpr(expr, config$history),
                            forceEval = as.integer(checkForceEvalList(expr, config)),
                            time = Sys.time())
        write.dcf(entry, file = config$metadata, append = TRUE, width = 5000)
        invisible(entry)
}

isCached <- function(exprFile) {
        file.exists(exprFile)
}
        
runExpression <- function (expr, config) {
        ## 'expr' is a single expression, so something like 'a <- 1'
        if(!checkForceEvalList(expr, config)) {
                exprFile <- exprFileName(expr, config)

                if(!isCached(exprFile)) {
                        message("eval expr and cache")
                        evalAndCache(expr, exprFile, config)
                }
                message("--loading expr from cache")
                cacheLazyLoad(exprFile, globalenv())
        }
        else {
                message("force evaluating expression")
                eval(expr, globalenv(), baseenv())
        }
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
## do not end up using extra memory.

evalAndCache <- function(expr, exprFile, config) {
        env <- new.env(parent = globalenv())
        before <- copyEnv(globalenv())
        eval(expr, env, globalenv())
        after <- copyEnv(globalenv())

        ## Functions like 'source' and 'set.seed' alter the global
        ## environment, so check after evaluation
        new.global <- checkNewSymbols(before, after)
        copy2env(new.global, globalenv(), env)

        ## Get newly assigned object names
        keys <- ls(env, all.names = TRUE)

        if(length(keys) == 0 && !checkForceEvalList(expr, config)) {
                message("expression has side effect: ", hash(expr))
                updateForceEvalList(expr, config)
        }
        saveWithIndex(keys, exprFile, env)
        env
}

exprFileName <- function(expr, config) {
        file.path(config$cachedir, hashExpr(expr, config$history))
}

################################################################################
## Handling expressions with side effects

forceEvalListFile <- function(config) {
        file.path(config$cachedir, ".ForceEvalList")
}

updateForceEvalList <- function(expr, config) {
        con <- file(forceEvalListFile(config), "a")
        on.exit(close(con))
        
        writeLines(hash(expr), con)
}

initForceEvalList <- function(config) {
        file <- forceEvalListFile(config)

        ## This is probably not necessary....
        if(!file.exists(file))
                file.create(file)
        invisible(file)
}

checkForceEvalList <- function(expr, config) {
        exprList <- readLines(forceEvalListFile(config))
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

writeChunkMetadata <- function(object, chunk, options) {
        chunkprefix <- utils::RweaveChunkPrefix(options)
        chunkexps <- parse(text = chunk)
        chunkDigest <- hash(chunkexps)
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
































