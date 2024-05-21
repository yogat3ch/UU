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


#' Changes installed package version relationships to `==` from the `usethis` default of `>=`
#'
#' @param pkg \code{chr} Name of package
#' @param write_to_desc \code{lgl} Whether to write the packages with updated
#'
#' @return \code{None} Either prints to the console or modifies the DESCRIPTION in place
#' @export
#'

installed_to_description <- function(pkg, write_to_desc = TRUE) {
  pd <- packageDescription(pkg)
  out <- stringr::str_split(pd$Imports, "\n")[[1]] |>
    purrr::map_chr(\(.x) {
      v <- stringr::str_extract(.x, "\\d+\\.(?:\\d+\\.){1,2}\\d*")[[1]]
      p <- stringr::str_extract(.x, "^[A-Za-z0-9\\.]+")[[1]]
      if (!is.na(v)) {
        pv <- packageDescription(p)
        .x <- stringr::str_replace(.x, v, pv$Version)
      }
      .x
    })
  if (write_to_desc) {
    desc_path <- file.path("DESCRIPTION")
    keep.white <- c("Authors@R", "Imports", "Remotes", "Suggests")
    if (file.exists(desc_path)) {
      desc <- read.dcf(desc_path, keep.white = keep.white)
      import_col <- grep("Imports", colnames(desc))
      replacement <- paste0("\n\t", glue::glue_collapse(out, sep = "\n\t"))
      if (length(import_col) == 0) {
        desc <- cbind(desc, gsub("Imports: ", "\n\t",
                                 replacement))
        colnames(desc)[ncol(desc)] <- "Imports"
      }
      else {
        desc[, "Imports"] <- gsub("Imports: ", "\n\t",
                                  replacement)
      }
      write.dcf(desc, file = desc_path, keep.white = keep.white)
    }
  }
}
