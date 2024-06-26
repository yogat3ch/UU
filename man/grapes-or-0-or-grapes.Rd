% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infixes.R
\name{\%|0|\%}
\alias{\%|0|\%}
\title{Replace a 0 length value}
\usage{
lhs \%|0|\% rhs
}
\arguments{
\item{lhs}{\code{(expression)} to try}

\item{rhs}{\code{(expression)} to replace if expression fails}
}
\value{
results from lhs if length > 1 otherwise rhs
}
\description{
If the lhs is length 0, replace with rhs
}
\seealso{
Other infixes: 
\code{\link{\%allin\%}()},
\code{\link{\%nin\%}()},
\code{\link{\%|\%}()},
\code{\link{\%|legit|\%}()},
\code{\link{\%|try|\%}()},
\code{\link{\%|zchar|\%}()}
}
\concept{infixes}
