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
  \item{num}{expression sequence number}
  \item{append}{logical, should we add to the list of expressions to be
    skipped?}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  Nothing useful is returned; the global list of expressions to be
  skipped is modified.
}
\seealso{\code{\link{code}}}
\keyword{utilities}
