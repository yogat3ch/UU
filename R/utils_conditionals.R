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
#'
#' @param x \code{(object)} to check for legitimacy
#' @param is.null \code{lgl} Check for non-null
#' @param is_empty \code{lgl} Check for non-empty
#' @param is.na \code{lgl} Check for non-NA
#' @param not_error \code{lgl} Check for non-error
#'
#' @return \code{(logical)}
#' @family conditionals
#' @export

is_legit <- function(x, is.null = TRUE, is_empty = TRUE, is.na = TRUE, not_error = TRUE) {
  .checks <- c(is.null, is_empty, is.na, not_error)
  if (any(!.checks)) {
    checks <- rlang::exprs(
      is.null  = legit_null(x),
      is_empty = legit_empty(x),
      # If all arguments are empty, all(is_na) will be TRUE. To disambiguate an empty object from one that has all objects as NA, we need to simultaneously check if the object is non-empty
      is.na = legit_na(x),
      not_error = legit_error(x)
    )[.checks]
    checks <- purrr::reduce(checks, .f = \(.x, .y, ...) {
      rlang::call2(`||`, .x, .y)
    })
    !rlang::eval_bare(checks)
  } else {
    !(legit_null(x) || legit_empty(x) || legit_na(x) || legit_error(x))
  }

}

legit_null <- function(x) {
  all(is.null(x))
}
legit_empty <- function(x) {
  rlang::is_empty(x)
}
legit_na <- function(x) {
  all(suppressWarnings(is.na(x))) & {if (is.data.frame(x)) nrow(x) != 0 else !rlang::is_empty(x)}
}
legit_error <- function(x) {
  is_error(x)
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
zchar <- \(x) isTRUE(Negate(nzchar)(x))

#' Are the values in each object the same?
#' @description
#' The primary difference from \code{\link[base]{identical}} & \code{\link[base]{all.equal}} is that objects are sorted by name so order doesn't matter. Set `sort_by_names = FALSE` to sort by values instead of names for atomic vectors: eg `c(1,2,3)` is equivalent to `c(3,2,1)`. Turn off sorting altogether with `no_sort`.
#' @inheritParams base::all.equal
#' @param x \code{obj}
#' @param y \code{obj}
#' @param sort_by_names \code{lgl} Sort both target/current by their names (so the order is not taken into account when comparing). Sorting by names is non-recursive, only the top level of the list is sorted.
#' @param no_sort \code{lgl} Turn off the default behavior that sorts atomic vectors before comparing. This will compare `current` & `target` as is, equivalent to `isTRUE(all.equal(target,current, ...))`.
#' @inheritDotParams base::all.equal
#' @return \code{lgl}
#' @export
#' @family conditionals
#' @examples
#' same(list(x = 1, y = 2), list(y = 2, x = 1))
same <- function(target = x, current = y, sort_by_names = TRUE, x = target, y = current, no_sort = FALSE, ...) {
  if (!no_sort) {
    nms = list(x = !is.null(names(target)),
               y = !is.null(names(current)))

    if (sort_by_names && all(nms$x, nms$y)) {
      stopifnot(`target must be named` = nms$x)
      stopifnot(`current must be named` = nms$y)
      target <- target[order(names(target))]
      current <- current[order(names(current))]
    } else {
      if (rlang::is_atomic(target))
        target <- sort(target)
      if (rlang::is_atomic(current))
        current <- sort(current)
    }
  }

  isTRUE(all.equal(target, current, ...))
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
