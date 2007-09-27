\name{showobjects}
\alias{showobjects}

\title{Show objects for an expression}
\description{
  Show what objects are cached for a given expression
}
\usage{
showobjects(num)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{expression sequence number}
}
\details{
  Given an expression sequence number, \code{showobjects} shows what
  objects were created (and hence cached) by that expression.  These
  objects can subsequently be loaded into the workspace with
  \code{loadcache}.  If \code{num} is a sequence, then a single
  character vector is returned containing the union of the names of the
  objects cached.

  Expression sequence numbers can be obtained by calling \code{code}.
}
\value{
  A character vector containing the names of the objects
}
\seealso{\code{\link{loadcache}}, \code{\link{code}}}
\keyword{utilities}
