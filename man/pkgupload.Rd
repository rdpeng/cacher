\name{pkgupload}
\alias{pkgupload}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Upload cache packages}
\description{
  Tool for uploading a cache package to a remote archive
}
\usage{
pkgupload(pkgname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkgname}{character, file name for cache package}
}
\details{
  If one has created a cache package with the \code{package} function,
  the package can be uploaded to a remote archive using the
  \code{pkgupload} function.  The \code{postForm} function from the
  \code{RCurl} package is required for this to work.
}
\value{
  The (HTML) response from the webserver is returned as a character
  vector.
}
\seealso{\code{\link{package}}}
\keyword{utilities}
