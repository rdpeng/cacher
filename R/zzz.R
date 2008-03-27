.onLoad <- function(lib, pkg) {
        setConfig("exprDeparseWidth", 30)
        setConfig("cachedir", ".cache")
        setConfig("archive", "http://penguin.biostat.jhsph.edu")
        setConfig("uploadscript", "http://penguin.biostat.jhsph.edu/cgi-bin/cpkg-upload.pl")
        setConfig("verbose", FALSE)
}

