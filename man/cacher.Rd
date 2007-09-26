\name{cacher}
\alias{cacher}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cacher(srcfile, cachedir = ".cache", logfile = NULL)
cc(expr, cachedir = ".cache", srcfile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{srcfile}{character, source file to be cached}
  \item{cachedir}{character, path to cache directory}
  \item{logfile}{character, path to file for log messages}
  \item{expr}{an R expression}
}
\details{
  \code{cacher} caches the results of evaluating the R expressions in
  the source file \code{srcfile}.  Contents are cached to the directory
  indicated by \code{cachedir} and by default, log messages are written
  to a log file in the cache directory.
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
