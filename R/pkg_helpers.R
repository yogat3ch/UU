#' Is working directory a package?
#'
#' @return \code{lgl}
#' @export
#' @family package dev
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
#' @family package dev
#' @return \code{lgl}
#' @export
#'

is_package_dev <- function(pkg_nm = pkg_name()) {
  pkgload::is_dev_package(pkg_nm)
}

#' @title Get the names of all exported functions in a package
#'
#' @param x \code{(character)} Package name
#' @param all.names \code{(logical)} Include names that begin with characters `.` `_` etc
#'
#' @return \code{(character)}
#' @export
#' @family package dev
#' @examples
#' get_package_fns("dplyr")
get_package_fns <- function(x, all.names = FALSE, pattern, negate = FALSE) {
  nms <- ls(getNamespace(x), all.names=all.names)
  if (!missing(pattern))
    nms<- stringr::str_subset(nms, pattern, negate)
  nms
}
