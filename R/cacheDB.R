## Simple database implementation for cacheSweave

saveWithIndex <- function(list = character(0), file, envir = parent.frame()) {
        con <- gzfile(file, "wb")
        on.exit(close(con))
        
        byteList <- lapply(list, function(symname) {
                x <- get(symname, envir, inherits = FALSE)

                if(is.environment(x))
                        warning(gettextf("saving of environments not supported"))
                list(key = symname,
                     bytes = serialize(x, connection = NULL))
        })
        writeIndex(byteList, con)
        writeOffset(con)
        writeData(byteList, con)
}

writeIndex <- function(byteList, con) {
        if(length(byteList) > 0) {
                lens <- sapply(byteList, function(x) length(x$bytes))
                index <- c(0, cumsum(lens)[-length(byteList)])
                names(index) <- sapply(byteList, "[[", "key")
                
        }
        else
                index <- integer(0)
        serialize(index, con)
}

readOffset <- function(con) {
        integerLen <- unserialize(con)
        offset.raw <- unserialize(con)
        integerLen + integerLen + offset.raw
}

writeOffset <- function(con) {
        offset <- seek(con)
        integerLen <- length(serialize(as.integer(1), NULL))
        serialize(as.integer(integerLen), con)
        serialize(as.integer(offset), con)
}

writeData <- function(byteList, con) {
        for(entry in byteList) {
                writeBin(entry$bytes, con)
        }
}

isEmptyIndex <- function(idx) {
        isTRUE(length(idx) == 0)
}

cacheLazyLoad <- function(file, envir = parent.frame()) {
        cachedir <- getConfig("cachedir")

        if(!file.exists(file)) {
                origin <- readLines(file.path(cachedir, "ORIGIN"))
                dbfile <- file.path(dbdir(origin), basename(file))
        }
        else
                dbfile <- file
        dbcon <- gzcon(file(dbfile, "rb"))
        tryCatch({
                index <- unserialize(dbcon)
                offset <- readOffset(dbcon)
        }, finally = {
                if(isOpen(dbcon))
                        close(dbcon)
        })
        if(isEmptyIndex(index))
                return(character(0))
        wrap <- function(x, pos, env) {
                force(x)
                force(pos)

                delayedAssign(x, {
                        if(!file.exists(file))
                                transferCacheFile(file, cachedir)
                        con <- gzfile(file, "rb")
                        tryCatch({
                                seek(con, pos + offset)
                                unserialize(con)
                        }, finally = {
                                close(con)
                        })
                }, eval.env = environment(), assign.env = env)
        }
        keys <- names(index)

        if(is.null(keys))
                stop("problem with lazy-load database index")        
        for(i in seq_along(index)) {
                wrap(keys[i], index[i], envir)
        }
        invisible(keys)
}

getIndex <- function(file) {
        con <- gzfile(file, "rb")
        on.exit(close(con))

        index <- unserialize(con)
        index
}
