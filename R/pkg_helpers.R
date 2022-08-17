#' Is working directory a package?
#'
#' @return \code{lgl}
#' @export
#' @examples
#' is_package()

is_package <- function () {
  nzchar(pkg_path())
}

pkg_path <- function() {
  pkgload::pkg_path() %|try|% ""
}

#' Is package in development or installed
#'
#' @param pkg_nm \code{chr} name of package
#'
#' @return \code{lgl}
#' @export
#'

is_package_dev <- function(pkg_nm = pkgload::pkg_name() %|try|% NULL) {
  (pkg_nm %in% pkgload:::dev_packages()) %|0|% FALSE
}

