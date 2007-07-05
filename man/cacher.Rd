\name{cacher}
\alias{cacher}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cacher(file, cachedir = ".cache")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{ ~~Describe \code{file} here~~ }
  \item{cachedir}{ ~~Describe \code{cachedir} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(file, cachedir = ".cache") {
        if(!file.exists(cachedir))
                dir.create(cachedir)
        if(!file.exists(metadata <- file.path(cachedir, ".exprMetaData")))
                file.create(metadata)
        config <- list(cachedir = cachedir,
                       metadata = metadata)
        exprList <- parse(file, srcfile = NULL)

        initForceEvalList(config)

        for(i in seq_along(exprList)) {
                cat(i, " ")
                expr <- exprList[i]
                config$history <- exprList[seq_len(i - 1)]
                runExpression(expr, config)

                writeMetadata(expr, config)
        }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
