\name{graphcode}
\Rdversion{1.1}
\alias{graphcode}

\title{
  Plot graph of variable relationships
}
\description{
  Create a plot of a directed graph showing the relationships between
  the variables in code file or script.
}
\usage{
graphcode(num, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{
    a vector of expression sequence numbers
  }
  \item{\dots}{
    other arguments passed to the \code{plot} method
  }
}
\details{
  This function depends on the \pkg{CodeDepends} (for computing
  dependencies) and \pkg{Rgraphviz} (for plotting)
  packages in order to work. If they are not installed, an error will
  thrown.
}
\value{
  Nothing useful is returned.
}
\author{
  Roger D. Peng \email{rpeng@jhsph.edu}
}

\keyword{utilities}
