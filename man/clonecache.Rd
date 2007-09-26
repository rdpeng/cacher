\name{clonecache}
\alias{clonecache}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Clone a remote cache}
\description{
  Clone a remote cache or download a remote cache package
}
\usage{
clonecache(origin, cachedir = ".cache", all.files = FALSE, id = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{origin}{character, URL of remote cache}
  \item{cachedir}{character, path to local cache directory in which
    remote cache will be cloned}
  \item{all.files}{logical, should all database files be downloaded?}
  \item{id}{character, identifier for a cache packages stored in a remote
    archive}
}
\details{
  If \code{clonecache} is given a URL via its \code{origin} argument,
  then that cache is cloned and (by default) all of the metadata from
  the remote cache is copied to the local directory indicated by
  \code{cachedir}.  If \code{all.files = TRUE} then the database files
  are also downloaded.

  If \code{id} is non-\code{NULL}, then the \code{origin} argument is
  ignored and the package corresponding to the ID is downloaded from a
  remote archive (as set determined by \code{getConfig("archive")}.
}
\value{
  Nothing is returned; a cache directory is created and metadata are
  downloaded to it.
}
\keyword{utilities}
