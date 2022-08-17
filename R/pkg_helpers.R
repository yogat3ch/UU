#' Is working directory a package?
#'
#' @return \code{lgl}
#' @export
#' @examples
#' is_package()

is_package <- function () {
  pkg_path() %||% FALSE
}

pkg_path <- function() {
  pkgload::pkg_path() %|try|% NULL
}

pkg_name <- function() {
  pkgload::pkg_name() %|try|% NULL
}

#' Is package in development or installed
#'
#' @param pkg_nm \code{chr} name of package
#'
#' @return \code{lgl}
#' @export
#'

is_package_dev <- function(pkg_nm = pkg_name()) {
  pkgload::is_dev_package(pkg_nm)
}

