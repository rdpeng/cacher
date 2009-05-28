\name{objectcode}
\Rdversion{1.1}
\alias{objectcode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Show dependent R expressions for an object
}
\description{
  Given the name of an R object or set of R objects, show the
  R expressions on which those objects depend and return the indices for
  those R expressions in the document.
}
\usage{
objectcode(name, show = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
    character vector of names of variables
  }
  %%     ~~Describe \code{name} here~~
  \item{show}{
    Should the expressions be printed to the screen?
  }
}
\details{
  This function depends on the \pkg{CodeDepends} package for computing
  the dependencies. If it is not installed, an error will be thrown.
}
\value{
  A vector of indices indicating the expression sequence numbers for the
  R expressions.
}
\author{
  Roger D. Peng \email{rpeng@jhsph.edu}
}
\keyword{utilities}
