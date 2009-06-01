## Simple database implementation for cacheSweave

saveWithIndex <- function(list = character(0), file, envir = parent.frame()) {
        con <- gzfile(file, "wb")
        on.exit(close(con))

        byteList <- lapply(list, function(symname) {
                x <- get(symname, envir, inherits = FALSE)

                if(is.environment(x))
                        warning(gettextf("saving of environments not supported"))
                list(key = symname,
                     bytes = serialize(x, connection = NULL),
                     hash = hash(x))
        })
        writeIndex(byteList, con)
        writeHash(byteList, con)
        writeData(byteList, con)
}

writeHash <- function(byteList, con) {
        hashlist <- sapply(byteList, "[[", "hash")
        names(hashlist) <- sapply(byteList, "[[", "key")
        serialize(hashlist, con)
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

writeData <- function(byteList, con) {
        for(entry in byteList) {
                writeBin(entry$bytes, con)
        }
}

isEmptyIndex <- function(idx) {
        isTRUE(length(idx) == 0)
}

validConnection <- function(con) {
        tryCatch({
                summary(con)
                TRUE
        }, error = function(err) {
                FALSE
        })
}

isCloseable <- function(con) {
        validConnection(con) && isOpen(con)
}

cacheLazyLoad <- function(file, envir = parent.frame()) {
        cachedir <- cache()

        if(file.exists(file))
                dbfile <- file
        else if(isClone()) {
                ## file doesn't exist, but can be transferred from remote
                origin <- readLines(file.path(cachedir, "origin"))
                dbfile <- file.path(dbdir(origin), basename(file))
        }
        else
                stop(gettextf("unable to lazy-load file '%s'", file))

        ## This might be read over the network
        dbcon <- gzcon(file(dbfile, "rb"))
        index <- tryCatch({
                unserialize(dbcon)
        }, finally = {
                if(isCloseable(dbcon))
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
                                index <- unserialize(con)
                                hash <- unserialize(con)
                                offset <- seek(con)
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
