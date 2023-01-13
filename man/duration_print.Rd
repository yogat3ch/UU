% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_time_helpers.R
\name{duration_print}
\alias{duration_print}
\title{Translate a duration into the human-legible estimation as a character}
\usage{
duration_print(duration)
}
\arguments{
\item{duration}{\code{Duration}}
}
\value{
\code{chr}
}
\description{
Translate a duration into the human-legible estimation as a character
}
\examples{
duration_print(lubridate::period(6223))
}