\name{runcode}
\alias{runcode}
\alias{checkcode}

\title{Evaluate cached R expressions}
\description{
  Evaluate and check R expressions in a cached analysis
}
\usage{
runcode(num, env = parent.frame(), forceAll = FALSE)
checkcode(num, env = globalenv())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{expression sequence number}
  \item{env}{environment in which expressions should be evaluated}
  \item{forceAll}{logical, should we load cached expressions when
    possible or evaluate all expressions?}
}
\details{
  \code{checkcode} will evaluate each R expression in the source file on
  the user's local machine and compare any resulting outputs to the
  corresponding objects stored in the cache.  If the outputs do not match (in
  the sense that \code{all.equal} returns something other than
  \code{TRUE}), then a message is printed indicating the failure to
  verify the output and the messages from \code{all.equal} are provided
  to the user.  Otherwise, \code{checkcode} will print \code{OK} for
  that expression and move to the next expression.
}
\note{
  Objects involving character values (such as factors) may be subject to
  collating rules that are specific to the user's local environment.
  Therefore, if objects are recreated on a user's local machine and
  compared with the same object in the cache (which was presumably
  created on a different machine), \code{checkcode} may report a
  verification failure because of the change in locale.
}
\value{
  Neither \code{runcode} nor \code{checkcode} return anything useful.
}
\seealso{\code{\link{code}}, \code{\link{showcode}}}
\keyword{utilities}% __ONLY ONE__ keyword per line
