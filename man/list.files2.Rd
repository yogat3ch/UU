% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/universal_useful.R
\name{list.files2}
\alias{list.files2}
\title{List full file paths with the file name as the name}
\usage{
list.files2(path = ".", full.names = TRUE, ...)
}
\arguments{
\item{path}{\code{(character)} dir path to files}

\item{full.names}{a logical value.  If \code{TRUE}, the directory
    path is prepended to the file names to give a relative file path.
    If \code{FALSE}, the file names (rather than paths) are returned.}

\item{...}{
  Arguments passed on to \code{\link[base:list.files]{base::list.files}}
  \describe{
    \item{\code{pattern}}{an optional \link[base]{regular expression}.  Only file names
    which match the regular expression will be returned.}
    \item{\code{all.files}}{a logical value.  If \code{FALSE}, only the
    names of visible files are returned (following Unix-style visibility,
    that is files whose name does not start with a dot).  If \code{TRUE},
    all file names will be returned.}
    \item{\code{recursive}}{logical.  Should the listing recurse into directories?}
    \item{\code{ignore.case}}{logical.  Should pattern-matching be case-insensitive?}
    \item{\code{include.dirs}}{logical.  Should subdirectory names be included in
    recursive listings?  (They always are in non-recursive ones).}
    \item{\code{no..}}{logical.  Should both \code{"."} and \code{".."} be excluded
    also from non-recursive listings?}
  }}
}
\value{
\code{(named character)}
}
\description{
List full file paths with the file name as the name
}
\examples{
list.files2("~")
}
