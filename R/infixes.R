#' @title Try an expression
#' @name try-infix
#' @description Calls the expression (LHS) & if it fails return RHS
#' @param lhs \code{(expression)} to try
#' @param rhs \code{(expression)} to replace if expression fails
#'
#' @return results from lhs on success results from rhs on fail
#' @export

`%|try|%` <- function(lhs, rhs) {
  tryCatch(rlang::eval_tidy(rlang::enquo(lhs)), error = rlang::as_function(~{rlang::eval_tidy(rlang::enquo(rhs))}))
}

#' @title Replace a 0 length value
#' @name zero-length-infix
#' @description If the lhs is length 0, replace with rhs
#' @param lhs \code{(expression)} to try
#' @param rhs \code{(expression)} to replace if expression fails
#'
#' @return results from lhs if length > 1 otherwise rhs
#' @export

`%|0|%` <- function(lhs, rhs) {
  if (rlang::is_empty(lhs))
    rhs
  else
    lhs
}

#' If legit lhs, else rhs
#' @inheritParams try-infix
#' @name legit-infix
#' @seealso is_legit
#' @return If legit lhs else rhs
#' @export
#' @examples
#' (100 / NA) %|legit|% 4
#' list(a = 5)$a$value %|legit|% 4
`%|legit|%` <- Vectorize(function(lhs, rhs) {
  if (UU::is_legit(try(lhs, silent = TRUE))) {
    lhs
  } else {
    rhs
  }
}, vectorize.args = "lhs", SIMPLIFY = TRUE)

#' Replace zero-length character strings with right hand side
#'
#' @param lhs \code{chr}
#' @param rhs \code{chr}
#'
#' @return \code{chr}
#' @export
#'
#' @examples
#' c("a" , "", "c", "") %|nzchar|% "b"
`%|nzchar|%` <- Vectorize(function(lhs, rhs) {
  if (nzchar(lhs))
    lhs
  else
    rhs
}, vectorize.args = "lhs", SIMPLIFY = TRUE)
