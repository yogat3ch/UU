% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/universal_useful.R
\name{size}
\alias{size}
\title{Digital storage size conversion
See \link[utils]{object.size}}
\usage{
size(
  x,
  in_unit = c("b", "Kb", "Mb", "Gb", "Tb", "Pb", "Eb", "Zb", "Yb")[1],
  out_unit = "b",
  standard = c("IEC", "SI")[1]
)
}
\arguments{
\item{x}{\code{(numeric)}}

\item{in_unit}{\code{(character)} units of x}

\item{out_unit}{\code{(character)} units of output number}

\item{standard}{\code{(character)}}
}
\value{
\code{(numeric)}
}
\description{
Digital storage size conversion
See \link[utils]{object.size}
}
\examples{
size(50, "mb")
size(50, "gb")
size(50, "gb", "mb")
}
