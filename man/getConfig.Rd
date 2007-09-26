\name{getConfig}
\alias{getConfig}
\alias{setConfig}

\title{Get and set configuration options}
\description{
  Get and set configuration options
}
\usage{
getConfig(name)
setConfig(name, value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{character, name of the option}
  \item{value}{option setting}
}
\details{
  There are a number of options that \code{cacher} checks but some of
  the important ones for users are:
  \item{srcfile}{the source file currently being cached or explored}
  \item{cachedir}{the cache directory}
  \item{archive}{URL location of the archive from which cache packages
    should be downloaded}
}
\value{
  \code{getConfig} returns the value of the given option or \code{NULL}
  if the option is not valid; \code{setConfig} returns nothing.
}
\seealso{\code{\link{sourcefile}}, \code{\link{cache}}}
\keyword{utilities}
