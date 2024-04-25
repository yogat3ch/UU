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

#' Get a function from a package, abort if package not installed.
#'
#' @param pkg \code{chr} package
#' @param fn \code{fn} function name
#'
#' @return \code{fun}
#' @export
#' @family package dev
#' @examples
#' need_pkg("utils", "recover")
need_pkg <- function(pkg, fn) {
  cmd <- cli::code_highlight(glue::glue("install.packages('{pkg}')"), code_theme = 'Twilight')
  getFromNamespace(fn, ns = pkg) %|try|% gbort(c("{fn} requires {pkg}. Use {cmd} first."))
}

#' Split a list of packages separated by commas
#'
#' @param x \code{chr} of packages, often the output from `renv`
#'
#' @return \code{chr} vector with each package as a character
#' @export
#' @family package dev
#' @examples
#' string_split_comma(c("DT, USAboundaries, USAboundariesData, cicerone, config",
#' "ggplot2, golem, googlesheets4, htmlwidgets, leaflet",
#' "paletteer, plotly, rmapshaper, scales, sf, shiny, shinyBS",
#' "shinyWidgets, shinyauthr, shinycssloaders, shinydashboard",
#' "shinyjs, shinyvalidate, tidyr") )
pkg_chr_split_comma <- function(x) {
  dput(unlist(lapply(x, \(.x) {
    stringr::str_split(.x, "\\,\\s{1,}")
  })))
}
