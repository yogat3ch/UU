% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infixes.R
\name{\%|try|\%}
\alias{\%|try|\%}
\title{Try an expression}
\usage{
lhs \%|try|\% rhs
}
\arguments{
\item{lhs}{\code{(expression)} to try}

\item{rhs}{\code{(expression)} to replace if expression fails}
}
\value{
results from lhs on success results from rhs on fail
}
\description{
Calls the expression (LHS) & if it fails return RHS
}
\seealso{
Other infixes: 
\code{\link{\%allin\%}()},
\code{\link{\%nin\%}()},
\code{\link{\%|0|\%}()},
\code{\link{\%|\%}()},
\code{\link{\%|legit|\%}()},
\code{\link{\%|zchar|\%}()}
}
\concept{infixes}
