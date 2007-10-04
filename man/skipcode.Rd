\name{skipcode}
\alias{skipcode}

\title{Skip evaluation of an expression}
\description{
  Specify R expressions whose evaluation should be skipped
}
\usage{
skipcode(num, append = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{expression sequence number, or \code{NULL}}
  \item{append}{logical, should we add to the list of expressions to be
    skipped?}
}
\details{
  \code{skipcode} can be used to force certain expressions to be skipped
  from evaluation when using the \code{runcode} function (for example, if
  certain external resources are not available).  There is a globally
  maintained list of expressions that will be skipped for a given source
  file.

  If \code{num} is \code{NULL}, then the list of skipped expressions is
  cleared.
}
\value{
  Nothing useful is returned; the global list of expressions to be
  skipped is modified.
}
\seealso{\code{\link{code}}}
\keyword{utilities}
