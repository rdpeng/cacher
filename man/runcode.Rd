\name{runcode}
\alias{runcode}
\alias{checkcode}

\title{Evaluate cached R expressions}
\description{
  Evaluate and check R expressions in a cached analysis
}
\usage{
runcode(num, env = parent.frame(), forceAll = FALSE)
checkcode(num, env = parent.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{expression sequence number}
  \item{env}{environment in which expressions should be evaluated}
  \item{forceAll}{logical, should we load cached expressions when
    possible or evaluate all expressions?}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  Neither \code{runcode} nor \code{checkcode} return anything useful.
}
\seealso{\code{\link{code}}, \code{\link{showcode}}}
\keyword{utilities}% __ONLY ONE__ keyword per line
