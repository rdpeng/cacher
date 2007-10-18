.onLoad <- function(lib, pkg) {
	setConfig("exprDeparseWidth", 30)
	setConfig("cachedir", ".cache")
	setConfig("archive", "http://www.biostat.jhsph.edu/rr")
	setConfig("uploadscript", "http://10.253.164.24/cgi-bin/cpkg-upload.pl")
	setConfig("verbose", FALSE)
}

