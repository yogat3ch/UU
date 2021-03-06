% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/universal_useful.R
\name{ext}
\alias{ext}
\title{Extract the file extensions from a filepath}
\usage{
ext(path, strip = FALSE, new_ext)
}
\arguments{
\item{path}{\code{(character)} path}

\item{strip}{\code{(logical)} Whether to strip the extension from the path to return the bare file name}

\item{new_ext}{\code{chr} New extension for the filename}
}
\value{
\code{(character)} with the extensions
}
\description{
Given a path, extract the file extension
}
