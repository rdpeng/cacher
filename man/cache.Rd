\name{cache}
\alias{cache}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Return cache directory}
\description{
  Return the path to the current cache directory
}
\usage{
cache()
}
\value{
  A string indicating the path to the current cache directory.
}
\examples{
setConfig("cachedir", "mycachedir")
cache()
}
\keyword{utilities}
