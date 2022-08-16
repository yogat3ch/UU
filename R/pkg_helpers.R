#' Is working directory a packagE?
#'
#' @return \code{lgl}
#' @export
#' @examples is_package()

is_package <- function () {
  UU::`%|try|%`(nzchar(pkgload::pkg_path()), FALSE)
}

