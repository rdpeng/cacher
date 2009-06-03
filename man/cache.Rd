\name{cache}
\alias{cache}
\alias{setcache}

\title{Set/Return cache directory}
\description{
  Set/Return the path to the current cache directory
}
\usage{
cache()
setcache(dir)
}
\arguments{
  \item{dir}{directory name}
}
\value{
  A string indicating the path to the current cache directory.
}
\examples{
setcache("mycachedir")
cache()
}
\keyword{utilities}
