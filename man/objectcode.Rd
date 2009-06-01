\name{objectcode}
\Rdversion{1.1}
\alias{objectcode}
\alias{evalobject}
\alias{loadobject}

\title{
  Show dependent R expressions for an object
}
\description{
  Given the name of an R object or set of R objects, show the
  R expressions on which those objects depend and return the indices for
  those R expressions in the document. Alternatively, evaluate the
  sequence of R expressions that give rise to an R object.
}
\usage{
objectcode(name, num, show = TRUE)
evalobject(name, num, env = parent.frame(), ...)
loadobject(name, num, env = parent.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
    character vector of names of variables
  }
  \item{num}{
    a vector of expression sequence numbers
  }
  \item{show}{
    Should the expressions be printed to the screen?
  }
  \item{env}{environment in which evaluation should occur}
  \item{...}{other arguments passed to \code{runcode}}
}
\details{
  These functions depend on the \pkg{CodeDepends} package for computing
  the dependencies. If it is not installed, an error will be thrown.

  The \code{loadobject} function loads the cached value of the
  expression in which the object is found. It is possible that the
  cached value of the expression will contain more objects than just the
  named object, in which case all objects will be loaded.
}
\value{
  \code{objectcode} returns a vector of indices indicating the
  expression sequence numbers for the R expressions. \code{evalobject}
  returns nothing but evaluates R expressions in the specified environment.
}
\author{
  Roger D. Peng \email{rpeng@jhsph.edu}
}
\keyword{utilities}
