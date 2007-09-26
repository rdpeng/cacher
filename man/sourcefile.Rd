\name{sourcefile}
\alias{sourcefile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get/set source file}
\description{
  Get or set the current source file for analysis
}
\usage{
sourcefile(srcfile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{srcfile}{character string indicating the source file for
    examination, or \code{NULL}}
}
\details{
  When \code{srcfile} is \code{NULL}, \code{sourcefile} returns a
  character vector of the currently active source file to be examined.
  This value affects other functions such as \code{code},
  \code{runcode}, etc.  If no source file has been previously set, then
  \code{sourcefile} returns \code{NULL}.

  If \code{srcfile} is a character string, then the currently active
  source file is set to whatever value of \code{srcfile} is given.
}
\value{
  A character string indicating the currently active source file or
  \code{NULL} if none has been set.
}
\keyword{utilities}
