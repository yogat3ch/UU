% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_environments_namespaces.R
\name{get_global}
\alias{get_global}
\title{Get an object from the global environment}
\usage{
get_global(global = "active")
}
\value{
\code{obj}
}
\description{
Get an object from the global environment
}
\examples{
get_global(".Last.value")
}
\seealso{
Other namespaces: 
\code{\link{assign_global}()},
\code{\link{assign_in_ns}()},
\code{\link{get_from_ns}()},
\code{\link{pkg_ns}()},
\code{\link{unload_namespaces}()}
}
\concept{namespaces}
