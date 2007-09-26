\name{pkgupload}
\alias{pkgupload}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pkgupload(pkgname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkgname}{ ~~Describe \code{pkgname} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (pkgname) 
{
    if (!require(RCurl)) 
        stop("'RCurl' required for uploading cache packages")
    message(gettextf("uploading cache package '\%s'", pkgname))
    fileInfo <- fileUpload(normalizePath(pkgname))
    out <- postForm("http://10.253.164.24/cgi-bin/cpkg-upload.pl", 
        cpkgfile = fileInfo)
    invisible(out)
  }
}
\keyword{utilities}
