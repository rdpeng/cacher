\name{checkobjects}
\alias{checkobjects}

\title{Verify cached objects}
\description{
  Verify the integrity of objects cached in the database
}
\usage{
checkobjects(num)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{code expression sequence number}
}
\details{
  If \code{num} is missing, then all of the objects from all of the
  expressions are checked.  Otherwise, \code{checkobjects} only checks
  objects in the expressions specifiedy by \code{num}.
}
\value{
  A list of length equal to the number of code expressions specified.
  Each element of the list is a logical vector indicating which objects
  fo that expression were verified.  In the output, \code{TRUE}
  indicates that the object was verified and \code{FALSE} indicates
  otherwise. 
}
\keyword{utilities}
