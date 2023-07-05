#' Is object an error
#'
#' @param x \code{obj}
#'
#' @return \code{lgl}
#' @export
#' @family conditionals
#' @examples
#' is_error(try(stop()))
is_error <- function(x) {
  inherits(x, c("error", "try-error"))
}

#' @title Is object legit?
#' @description Is object non-null, non-empty, non-NA, and not a try-error?
#' @param x \code{(object)}
#' @return \code{(logical)}
#' @family conditionals
#' @export

is_legit <- function(x) {
  !(all(is.null(x)) || rlang::is_empty(x) || all(suppressWarnings(is.na(x))) || inherits(x, c("try-error", "error")))
}


#' Is zero-length character?
#'
#' @param x \code{chr}
#'
#' @return \code{lgl}
#' @export
#' @family conditionals
#' @examples
#' zchar("")
#' zchar(" ")
zchar <- Negate(nzchar)


#' Are the values in each object the same?
#' @description
#' The primary difference from \code{\link[base]{identical}} & \code{\link[base]{all.equal}} is that objects are sorted by name so order doesn't matter. Set `sort_by_names = FALSE` to sort by values.
#'
#' @param x \code{obj}
#' @param y \code{obj}
#' @inheritDotParams all.equal
#' @return \code{lgl}
#' @export
#' @family conditionals
#' @examples
#' same(list(x = 1, y = 2), list(y = 2, x = 1))
same <- function(x, y, sort_by_names = TRUE, ...) {
  nms = list(x = !is.null(names(x)),
             y = !is.null(names(y)))
  if (sort_by_names && all(nms$x, nms$y)) {
    stopifnot(`x must be named` = nms$x)
    stopifnot(`y must be named` = nms$y)
    x <- x[order(names(x))]
    y <- y[order(names(y))]
  } else {
    x <- sort(x)
    y <- sort(y)
  }
  isTRUE(all.equal(x, y, ...))
}

#' Which is larger
#'
#' @param x \code{num/obj}
#' @param y \code{num/obj}
#' @param compare_length \code{lgl} whether the lengths rather than the values should be compared
#'
#' @return \code{num} the index of the larger, or 0 if equal
#' @export
#' @family conditionals
#' @examples
#' larger(1,2)
#' larger(1,2, compare_length = FALSE)
#' larger(letters[1:3],letters[1:2])
larger <- function(x, y, compare_length = TRUE) {
  v <- list(x, y)
  if (compare_length)
    v <- purrr::map(v, length)
  if (v[[1]] == v[[2]]) {
    0
  } else {
    which.max(purrr::list_flatten(v))
  }
}

#' Are most values TRUE
#' @description IF more than half the values are TRUE, returns TRUE
#' @param x \code{lgl}
#'
#' @return \code{lgl}
#' @export
#' @family conditionals
#' @examples
#' most(c(TRUE,TRUE,FALSE))
#' most(c(TRUE,FALSE,FALSE))
most <- function(x) {
  (sum(x, na.rm = TRUE) / length(na.omit(x))) > .5
}


#' Is Session in a Project?
#'
#' @return \code{lgl}
#' @export
#' @family conditionals
#' @examples
#' is_project()
is_project <- function() {
  desc <- utils::packageDescription("rstudioapi")
  if (is_legit(desc) && rstudioapi::isAvailable())
    is_legit(rstudioapi::getActiveProject())
  else
    FALSE
}


