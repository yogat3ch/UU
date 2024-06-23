#' The length of unique values in a vector
#'
#' @param x \code{vctr}
#'
#' @return \code{dbl}
#' @export
#' @family vectors
#' @examples
#' len_unique(c(1,2,1))
len_unique <- function(x) {
  length(unique(x))
}

#' Sort a vector or list by it's name (or self if no names)
#'
#' @param x \code{obj} to sort
#' @param by_names \code{lgl} whether to sort by names
#' @param sort_by_values \code{lgl} whether to sort by values in the absence of names. If FALSE and `x` has no names, an error will be thrown.
#' @family vectors
#' @return \code{obj} sorted
#' @export
#' @examples
#' sort_by_names(c(b = "b", c = "a"))
#' sort_by_names(c(b = "b", c = "a"), by_names = FALSE)
sort_by_names <- function(x, by_names = TRUE, sort_by_values = TRUE) {
  no_names <- is.null(names(x))
  y <- if (by_names) {
    if (no_names) {
      msg <- "{.code x} has no names"
      if (sort_by_values)
        gmsg(paste0(msg,", sorting by values."))
      else
        gbort(msg)
    }
    names(x) %||% x
  } else
    x
  stopifnot(`x must be a list` = !is.list(y))
  x[order(y)]
}


#' Unique a vector, preserving the names of the first original entries
#'
#' @param x \code{vec} with names
#'
#' @return \code{vec} of the same type
#' @export
#' @family vectors
#' @examples
#' unique_with_names(c(N = "n", b = "b", A = "n"))
unique_with_names <- function(x) {
  x[!duplicated(x)]
}

#' Unify two vectors preserving the order of `x`
#' @param x \code{vec} to preserve the order of
#' @param y \code{vec} to vector of values to include in the output (unordered)
#' @family vectors
#' @export
#' @examples
#' unify_vec_preserve_order(letters[c(5, 3)], letters[c(4:10,3)])
#' unify_vec_preserve_order(letters[1:5], letters[c(4:10)])
#' unify_vec_preserve_order(NULL, letters[c(4:10)])
#' unify_vec_preserve_order(letters, NULL)
unify_vec_preserve_order <- function(x, y) {
  out <- if (!any(na.rm = TRUE, x %in% y) || identical(x, y)) {
    y
  } else if (isTRUE(y %allin% x)) {
    intersect(x, y)
  } else {
    # intersect preserves the order of the first argument
    to_preserve <- intersect(x, y)
    new <- union(y, to_preserve)
    lout <- length(new)
    new_i <- seq_along(new)
    prev_order <- match(to_preserve, x)
    new[prev_order] <- to_preserve
    new_vals <- setdiff(y, x)
    new_order <- setdiff(new_i, prev_order)
    if (!rlang::is_empty(new_vals))
      new[new_order] <- new_vals
    new
  }
  return(out)
}

#' Switch the names and the values of a vector
#'
#' @param x \code{named object}
#'
#' @return \code{obj}
#' @export
#' @family vectors
#' @examples
#' names_values_switch(c(a = 1, b = 2))
names_values_switch <- function(x) {
  rlang::set_names(names(x), x)
}

#' Vlookup replace using a lookup column and reference table
#'
#' @param base \code{vector} of starting values. Replacements will be made in this vector before it is returned.
#' @param lookup_col \code{vector} of values with same length as `base` that will be matched to `lookup_ref` to deteremine the replacement indices
#' @param lookup_ref \code{vector} of reference values, which `lookup_col` will be matched to in order to determine replacement values.
#' @param value_col \code{vector} of replacement values with same length as `lookup_ref`
#'
#' @return \code{vector}
#' @export
#' @family vectors
#' @examples
#' ref <- tibble::tibble(lookup = letters[1:5], value = 1:5)
#' original <- tibble::tibble(lookup = letters[1:20], base = runif(20, min = 6, max = 20))
#' dplyr::mutate(original, base = vlookup_from_ref(base, lookup, ref$lookup, ref$value))
vlookup_from_ref <- function(
    base,
    lookup_col,
    lookup_ref,
    value_col
) {
  col_ref_idx <- match(lookup_col, lookup_ref)
  replacements <- value_col[na.omit(col_ref_idx)]
  col_base_idx <- !is.na(col_ref_idx)
  if (any(col_base_idx))
    base[which(col_base_idx)] <- replacements
  return(base)
}


# ----------------------- Mon Apr 08 16:49:54 2019 ------------------------#
#' @title rle_df - create a run-length-encoding data.frame
#' @description
#' Given an \code{\link[base]{rle}} this function will return a data.frame of starts, ends, and indexes thereof of the run lengths.
#' Credit: \url{https://stackoverflow.com/questions/43875716/find-start-and-end-positions-indices-of-runs-consecutive-values}
#' @param x \code{(vector)} An object for which to run an `rle`
#' @return \item{(data.frame)}{ with length, values, start and end indices.}
#' @family vectors
#' @examples
#' rle_df(sample(c(TRUE,FALSE), replace = TRUE, 100))
#' @export

rle_df <- function(x) {
  input_rle <- rle(x)
  .out <- unclass(input_rle)
  .out <- dplyr::select(dplyr::mutate(tibble::as_tibble(.out),
                                      end = cumsum(lengths),
                                      start = c(1, dplyr::lag(end)[-1] + 1)),
                        c(1,2,4,3))
  return(.out)
}

#' Create a sequence from the start to the end for a given value from an `rle_df` for indexing
#'
#' @param rle_df \code{(tbl)} See `rle_df`
#' @param value \code{(any)} Value to filter for in the `values` column. Require the values in the value column to be unique.
#' @family vectors
#' @return \code{(dbl)}
#' @export
#'
#' @examples
#' rle_seq(rle_df(rep(letters[1:3], each = 3)), "c")
rle_seq <- function(rle_df, value) {
  r <- dplyr::filter(rle_df, values == value)
  seq(r$start, r$end)
}

#' Create an RLE Grouping from a logical vector
#'
#' @param x \code{lgl} vector
#'
#' @return \code{list}
#' @export
#' @family vectors
#' @examples
#' rle_groups(sample(c(TRUE, FALSE), 20, replace = TRUE))
rle_groups <- function(x) {
  rle_df(x) |>
    dplyr::filter(values) |>
    apply(1, \(.x) {.x["start"]:.x["end"]})
}


#' Return the names of all TRUE items in a logical vector
#'
#' @param x \code{lgl} with names
#'
#' @return \code{chr} of the names of true values
#' @export
#' @family vectors
#' @examples
#' true_names(c(a = TRUE, b = FALSE))
#' true_names(c(a = FALSE, b = FALSE))
true_names <- function(x) {
  stopifnot(`x must have names` = !is.null(names(x)),
            `x must be a logical, atomic vector` = rlang::is_atomic(x) && rlang::is_logical(x))
  names(x)[x]
}

#' Remove zero length strings (or string with all spaces)
#'
#' @param x \code{chr}
#'
#' @return \code{chr}
#' @export
#' @family vectors
#' @examples
#' zchar_remove(c("", "  ", "a"))
zchar_remove <- function(x) {
  .x <- trimws(x)
  .x[nzchar(.x)]
}
