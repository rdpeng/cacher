\name{cacher}
\alias{cacher}
\alias{cc}

\title{Cache statistical analyses}
\description{
  Caching statistical analyses and R expressions
}
\usage{
cacher(srcfile, cachedir = ".cache", logfile = NULL)
cc(expr, cachedir = ".cache", srcfile = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{srcfile}{character, source file to be cached}
  \item{cachedir}{character, path to cache directory}
  \item{logfile}{character, path to file for log messages}
  \item{expr}{an R expression}
  \item{...}{other arguments passed to \code{cacher}}
}
\details{
  \code{cacher} caches the results of evaluating the R expressions in
  the source file \code{srcfile}.  Contents are cached to the directory
  indicated by \code{cachedir} and by default, log messages are written
  to a log file in the cache directory.

  The \code{cc} function caches individual expressions by writing these
  expressions to a temporary file and calling \code{cacher} on that
  file.  The name of the temporary file can be set by using the
  \code{srcfile} argument.  Otherwise, a random name is chosen.
}
\value{
  Nothing useful is returned.
}
\author{Roger D. Peng \email{rpeng@jhsph.edu}}
\examples{
\dontrun{
sfile <- system.file("examples", "sample.R", package = "cacher")
cacher(sfile)
}
}
\keyword{utilities}
