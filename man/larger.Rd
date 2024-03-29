% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_conditionals.R
\name{larger}
\alias{larger}
\title{Which is larger}
\usage{
larger(x, y, compare_length = TRUE)
}
\arguments{
\item{x}{\code{num/obj}}

\item{y}{\code{num/obj}}

\item{compare_length}{\code{lgl} whether the lengths rather than the values should be compared}
}
\value{
\code{num} the index of the larger, or 0 if equal
}
\description{
Which is larger
}
\examples{
larger(1,2)
larger(1,2, compare_length = FALSE)
larger(letters[1:3],letters[1:2])
}
\seealso{
Other conditionals: 
\code{\link{is_error}()},
\code{\link{is_legit}()},
\code{\link{is_project}()},
\code{\link{most}()},
\code{\link{same}()},
\code{\link{zchar}()}
}
\concept{conditionals}
