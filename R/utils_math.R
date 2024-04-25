#' Get even numbers
#' @param x \code{int}
#' @export
#' @examples
#' evens(1:10)
#' @family math
evens <- function(x) subset(x, x %% 2 == 0)

#' Get odd numbers
#' @param x \code{int}
#' @export
#' @examples
#' odds(1:10)
#' @family math
odds <- function(x) subset(x, x %% 2 == 1)

#' Simple interpolate between two numbers
#'
#' @param start \code{num} first number
#' @param end \code{num} last number
#' @param n \code{int} total numbers in output vector
#'
#' @return \code{num} vector with length equivalent to n
#' @export
#' @family math
#' @examples
#' interpolate(0, 6, 6)
interpolate = function(start, end, n) {
  out <- approx(x = c(start, end), n = n)$y
  return(out)
}

# Statistics ----
# Wed Apr  3 04:52:51 2024

#' @title Statistical mode
#' @description Return the most frequenctly occuring item in a dataset
#' @family statistics
#' @param x \code{(vector)}
#' @return \code{(vector)}
#' @export

smode <- function(x) {
  .u <- unique(x)
  tab <- tabulate(match(x, .u))
  .u[tab == max(tab)]
}

#' An alternative to \link[base]{max} that preserves names
#'
#' @param x \code{vec}
#'
#' @return \code{atomic} returns the largest element in the vector
#' @export
#' @family statistics
#' @examples
#' max2(c(a = 1, b = 2))
max2 <- function(x) {
  x[which.max(x) %|0|% 1]
}


#' Convert a math comparator to it's inverse
#'
#' @param x \code{chr/fun} Math comparator, one of `r paste0(comparison_inverse_key$key, collapse = ', ')`
#'
#' @return \code{chr/fun} the inverse comparator as the same class as `x`
#' @export
#' @family math
#' @examples
#' comparison_inverse(">=")
comparison_inverse <- function(x) {
  as_fun <- is.function(x)
  s <- if (as_fun)
    stringr::str_extract(rlang::expr_deparse(x), "[><=]+")
  else
    x

  inverse <- comparison_inverse_key[which(comparison_inverse_key$key == s),]$inverse
  if (as_fun)
    inverse <- getFromNamespace(inverse, "base")
  return(inverse)
}


#' Math comparison comparator to plain english key
#' @family math
#' @export
comparison_key <- c(
  "less than" = "<",
  "greater than" = ">",
  "more than" = ">",
  "less than or equal to" = "<=",
  "more than or equal to" = ">=",
  "greater than or equal to" = ">=",
  "equal to" = "="
)


#' Math comparison comparator inverse key
#' @family math
#' @export
comparison_inverse_key <- tibble::tibble(key = comparison_key[c(1:2,4,5)], inverse = c(">=", "<=", ">", "<"))

#' Convert inequality statements between character, mathematic, symbol and function representations
#'
#' @param x \code{chr} vector or inequality statements
#' @param outtype \code{chr} class of the outype
#' \itemize{
#'   \item{\code{"chr"}} A character
#'   \item{\code{"str"}} A character
#'   \item{\code{"sym"}} A symbol/name
#'   \item{\code{"name"}} A symbol/name
#'   \item{\code{"fun"}} A function
#' }
#'
#' @return \code{chr/name/fun} depending on the requested `outtype`
#' @export
#' @family math
#' @seealso comparison_key
#' @examples
#' str_comparison(">")
#' str_comparison("less than or equal to", "fun")
#' str_comparison("greater than or equal to", "sym")
str_comparison <- function(x, outtype = "chr") {

  out <- purrr::map_chr(x, ~{
    .switches <- if (.x %in% names(comparison_key))
      comparison_key
    else
      rlang::set_names(names(comparison_key), comparison_key)
    rlang::exec(switch, trimws(x),
                !!!.switches)
  })
  if (all(out %in% comparison_key) && !outtype %in% c("chr","str"))
    out <- switch(outtype,
                  name = ,
                  sym = rlang::syms(out),
                  fun = purrr::map(out, getFromNamespace, ns = "base"))

  return(out)
}
