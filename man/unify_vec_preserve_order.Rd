% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_vectors.R
\name{unify_vec_preserve_order}
\alias{unify_vec_preserve_order}
\title{Unify two vectors preserving the order of \code{x}}
\usage{
unify_vec_preserve_order(x, y)
}
\arguments{
\item{x}{\code{vec} to preserve the order of}

\item{y}{\code{vec} to vector of values to include in the output (unordered)}
}
\description{
Unify two vectors preserving the order of \code{x}
}
\examples{
unify_vec_preserve_order(letters[c(5, 3)], letters[c(4:10,3)])
unify_vec_preserve_order(letters[1:5], letters[c(4:10)])
unify_vec_preserve_order(NULL, letters[c(4:10)])
unify_vec_preserve_order(letters, NULL)
}
\seealso{
Other vectors: 
\code{\link{len_unique}()},
\code{\link{names_values_switch}()},
\code{\link{rle_df}()},
\code{\link{rle_groups}()},
\code{\link{rle_seq}()},
\code{\link{sort_by_names}()},
\code{\link{true_names}()},
\code{\link{unique_with_names}()},
\code{\link{vlookup}()},
\code{\link{vlookup_from_ref}()},
\code{\link{zchar_remove}()}
}
\concept{vectors}
