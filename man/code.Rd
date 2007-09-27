\name{code}
\alias{code}
\alias{showcode}
\title{Show code expressions}
\description{
  Show code expressions corresponding to a given source file
}
\usage{
code(num = NULL, full = FALSE)
showcode()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{expression sequence number}
  \item{full}{logical, should the full expression be shown or a one-line
  abbreviated version?}
}
\details{
  These functions can be used to show the expressions for a given source
  file.  By default, \code{code} shows all expressions in a file in a
  one-line abbreviated form along with their expression sequence
  numbers.  To see each expression in its entirety, the argument
  \code{full = TRUE} must be set.  If any expressions have been marked
  to be skipped by \code{skipcode}, those expressions will be annotated
  with an asterisk.

  \code{showcode} shows the original source file in the pager, which can
  be useful if one is interested in seeing any comments.
}
\value{
  Nothing useful is returned.
}
\keyword{utilities}
