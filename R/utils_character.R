#' Remove zero length strings (or string with all spaces)
#'
#' @param x \code{chr}
#'
#' @return \code{chr}
#' @export
#' @family character
#' @examples
#' zchar_remove(c("", "  ", "a"))
zchar_remove <- function(x) {
  .x <- trimws(x)
  .x[nzchar(.x)]
}


#' Split a list of packages separated by commas
#'
#' @param x \code{chr} of packages, often the output from `renv`
#'
#' @return \code{chr} vector with each package as a character
#' @export
#'
#' @examples
#' string_split_comma(c("DT, USAboundaries, USAboundariesData, cicerone, config",
#' "ggplot2, golem, googlesheets4, htmlwidgets, leaflet",
#' "paletteer, plotly, rmapshaper, scales, sf, shiny, shinyBS",
#' "shinyWidgets, shinyauthr, shinycssloaders, shinydashboard",
#' "shinyjs, shinyvalidate, tidyr") )
string_split_comma <- function(x) {
  dput(unlist(lapply(x, \(.x) {
    stringr::str_split(.x, "\\,\\s{1,}")
  })))
}
