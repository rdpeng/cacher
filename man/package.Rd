\name{package}
\alias{package}
\alias{cachepackage}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a cache package}
\description{
  Create a cache package for distribution
}
\usage{
package(cachedir)
cachepackage(cachedir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cachedir}{character, path to cache directory from which the
    cache package should be made}
}
\details{
  The \code{zip} utility is needed in order for this function to
  operate.  The cache directory is zipped up and an archive is created.
  This file can be distributed to others or uploaded to an archive
  website.
  
  If an archive file of the same already exists, it will be overwritten,
  with a warning issued.
}
\note{
  The \code{package} function is deprecated in favor of the renamed
  \code{cachepackage} function.
}
\value{
  The name of the archive file created.
}
\keyword{utilities}
