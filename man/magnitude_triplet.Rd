% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_rounding.R
\name{magnitude_triplet}
\alias{magnitude_triplet}
\title{Compute the order of magnitude triplet ie thousand, million, trillion}
\usage{
magnitude_triplet(x)
}
\arguments{
\item{x}{\code{num}}
}
\value{
\code{num}
}
\description{
Compute the order of magnitude triplet ie thousand, million, trillion
}
\examples{
magnitude_triplet(10^(1:10))
}
\seealso{
Other rounding: 
\code{\link{magnitude_order}()},
\code{\link{num2str}()},
\code{\link{num2str_vec}()},
\code{\link{num_chr_suffi}},
\code{\link{round_to}()},
\code{\link{size}()},
\code{\link{unit_find}()},
\code{\link{unit_modify}()},
\code{\link{unit_modify_vec}()},
\code{\link{unit_string}()}
}
\concept{rounding}
