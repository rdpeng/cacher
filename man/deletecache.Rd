\name{deletecache}
\alias{deletecache}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Delete the cache}
\description{
  This function clears the entire cache by deleting the cache directory
}
\usage{
deletecache(cachedir = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cachedir}{the path to the cache directory}
}
\details{
  If \code{cachedir} is \code{NULL}, then the cache directory returned
  by \code{cache} is removed.
}
\value{
  The result of \code{unlink} is returned.
}
\keyword{utilities}
