% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_shiny.R
\name{nm_to_id}
\alias{nm_to_id}
\title{Make a randomly formatted name into snakecase id}
\usage{
nm_to_id(x)
}
\arguments{
\item{x}{\code{chr}}
}
\value{
\code{chr} as a snakecase id
}
\description{
Make a randomly formatted name into snakecase id
}
\examples{
nm_to_id("This convoluted name")
}
\seealso{
Other shiny: 
\code{\link{as_js}()},
\code{\link{glue_js}()},
\code{\link{path_strip_shiny}()},
\code{\link{path_strip_to}()},
\code{\link{read_js}()},
\code{\link{shiny_error_recover}()},
\code{\link{strip_html}()}
}
\concept{shiny}
