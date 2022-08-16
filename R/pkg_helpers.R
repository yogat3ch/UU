#' Is working directory a packagE?
#'
#' @return \code{lgl}
#' @export
#' @examples
#' is_package()

is_package <- function () {
  nzchar(pkgload::pkg_path()) %|try|% FALSE
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

