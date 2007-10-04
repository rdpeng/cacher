\name{loadcache}
\alias{loadcache}
\title{Load objects from cache}
\description{
  Load R objects corresponding to a given expression from the cache
}
\usage{
loadcache(num, env = parent.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{expression sequence number}
  \item{env}{environment into which objects should be loaded}
}
\details{
  Objects associated with an expression can be lazy-loaded into the environment
  \code{env} using \code{loadcache}.  One can obtain a list of which
  objects are associated with which expressions using the
  \code{showobjects} function.  If \code{loadcache} is used to load
  objects from a remote cache, then the corresponding database files
  will be downloaded on the object's first access.

  Expression sequence numbers can be obtained by calling \code{code}.
}
\value{
  A character vector containing the names of the objects loaded (invisibly).
}
\seealso{\code{\link{code}}, \code{\link{showobjects}}}
\keyword{utilities}
