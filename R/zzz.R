.onLoad <- function(lib, pkg) {
        setConfig("exprDeparseWidth", 30)
        setConfig("cachedir", ".cache")
        setConfig("archive", "http://www.biostat.jhsph.edu/rr")
        setConfig("verbose", TRUE)
}

