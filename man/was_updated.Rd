% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_file_io.R
\name{was_updated}
\alias{was_updated}
\title{Was a file updated in the last hour?}
\usage{
was_updated(file, alert = TRUE)
}
\arguments{
\item{file}{\code{chr} file path}

\item{alert}{\code{alert} Whether to give a console alert that the file was updated.}
}
\value{
\code{lgl} whether the file was updated
}
\description{
Was a file updated in the last hour?
}
\examples{
x <- UU::mkpath(tempfile(), mkfile = TRUE)
was_updated(x)
file.remove(x)
}
