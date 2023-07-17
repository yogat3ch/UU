#' @title Try an expression
#' @description Calls the expression (LHS) & if it fails return RHS
#' @param lhs \code{(expression)} to try
#' @param rhs \code{(expression)} to replace if expression fails
#' @name %|try|%
#' @return results from lhs on success results from rhs on fail
#' @export

`%|try|%` <- function(lhs, rhs) {
  tryCatch(rlang::eval_tidy(rlang::enquo(lhs)), error = rlang::as_function(~{rlang::eval_tidy(rlang::enquo(rhs))}))
}

#' @title Replace a 0 length value
#' @description If the lhs is length 0, replace with rhs
#' @param lhs \code{(expression)} to try
#' @param rhs \code{(expression)} to replace if expression fails
#' @name %|0|%
#' @return results from lhs if length > 1 otherwise rhs
#' @export

`%|0|%` <- function(lhs, rhs) {
  if (rlang::is_empty(lhs))
    rhs
  else
    lhs
}

#' If legit lhs, else rhs
#' @inheritParams %|try|%
#' @name %|legit|%
#' @seealso is_legit
#' @return If legit lhs else rhs
#' @export
#' @examples
#' (100 / NA) %|legit|% 4
#' list(a = 5)$a %|legit|% 4
#' list(a = 5)$b %|legit|% 4
`%|legit|%` <-function(lhs, rhs) {
  if (is_legit(try(lhs, silent = TRUE))) {
    lhs
  } else {
    rhs
  }
}

#' Replace zero-length character strings with right hand side
#' @name %|zchar|%
#' @param lhs \code{chr}
#' @param rhs \code{chr}
#'
#' @return \code{chr}
#' @export
#'
#' @examples
#' c("a" , "", "c", "") %|zchar|% "b"
`%|zchar|%` <- Vectorize(function(lhs, rhs) {
  if (nzchar(lhs))
    lhs
  else
    rhs
}, vectorize.args = "lhs", SIMPLIFY = TRUE, USE.NAMES = FALSE)


#' Are lhs values absent from set on rhs?
#' @name %nin%
#' @seealso is_legit
#' @author Think.fr
#' @export
#' @examples
#' 1 %nin% 1:10
`%nin%` <- Negate(`%in%`)

#' Are all lhs values in rhs?
#' @name %allin%
#' @export
#' @examples
#' 2:5 %allin% 1:10
#' 5:11 %allin% 1:10
`%allin%` <- function(lhs, rhs) {
  isTRUE(all(lhs %in% rhs))
}

#' Is value non-null?
#' @author Think.fr
#' @seealso is_legit
#' @export
#' @examples
#' nonull(NULL)
nonull <- Negate(is.null)

#' Is value non-NA?
#' @seealso is_legit
#' @author Think.fr
#' @export
#' @examples
#' not_na(c("a", NA))
not_na <- Negate(is.na)
