#' Go to a specified folder
#' @param path \code{chr} to go to. Each method has a preset default based on the function name, but any method can be used to go to any folder specified.
#' @include utils_file_io.R
#' @family file navigation
#' @export
folder <- NULL

#' Go to a specified file
#' @description A List object with convenience functions that open the named file in RStudio. An `R` named sublist of all files in the _R_ folder if such a folder exists
#' @param path \code{chr} file to go to. Each method has a preset default based on the function name, but any path can be used to go to any file specified.
#' @family file navigation
#' @export
file <- NULL

#' All the files that can be navigated to with `file` if they exist
#' @family file navigation
#' @export
.file <- c(
  custom_scss = "inst/app/www/css/custom.scss",
  theme = "R/mod_theme.R",
  ui = "R/app_ui.R",
  server = "R/app_server.R",
  Rprofile = ".Rprofile",
  Renviron = ".Renviron",
  renv_settings = "renv/settings.dcf",
  desc = "DESCRIPTION",
  news = "NEWS.md",
  Readme.Rmd = "README.Rmd",
  Readme.md = "README.md",
  gitignore = ".gitignore"
)


