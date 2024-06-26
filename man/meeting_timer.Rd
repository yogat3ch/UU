% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_date_time.R
\name{meeting_timer}
\alias{meeting_timer}
\title{Partition a meeting into evenly distributed sections based on how much time is left between when intros end and the end of the meeting}
\usage{
meeting_timer(
  start_hms = "17:40:00",
  t4q = lubridate::minutes(10),
  m_length = lubridate::minutes(75),
  sections = 4,
  intros_end = lubridate::now()
)
}
\arguments{
\item{start_hms}{\code{chr} Meeting start time in 24-hour H:M:S Time format. This is HMS since it's easier to supply this argument in advance of the meeting.}

\item{t4q}{\code{Duration} how long to save for questions/closing remarks at the end}

\item{m_length}{\code{Duration} total length of the meeting}

\item{sections}{\code{num} Number of sections to distribute time}

\item{intros_end}{\code{Datetime} If you know when intros should end in advance, supply it, otherwise this function is intended to to be run when they end for a dynamic calculation of sections}
}
\value{
\code{list} With:
\itemize{
\item{\code{each_section}}{ time allotted to each section}
\item{\code{Section X}}{ The end time for each section}
\item{\code{end}}{ The end time}
}
}
\description{
Partition a meeting into evenly distributed sections based on how much time is left between when intros end and the end of the meeting
}
\examples{
# If the meeting started 10 minutes ago and the intros just ended
meeting_start_time <- (lubridate::now() - lubridate::minutes(10)) |> as.character() |> stringr::str_extract("\\\\d{2}:\\\\d{2}:\\\\d{2}")
meeting_timer(meeting_start_time)
}
