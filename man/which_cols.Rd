% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/universal_useful.R
\name{which_cols}
\alias{which_cols}
\title{Translate DT Column names to numeric indices using regex matching}
\usage{
which_cols(x, .data)
}
\arguments{
\item{x}{\code{chr/num} Column numbers or names}
}
\value{
\code{num} Numeric representation of column
}
\description{
Translate DT Column names to numeric indices using regex matching
}
\examples{
names(mtcars)
which_cols("c", mtcars)

}
