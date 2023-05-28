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
.file <-
  c(
    app_server = "R/app_server.R",
    app_ui = "R/app_ui.R",
    custom_scss = "inst/app/www/css/custom.scss",
    DESC = "DESCRIPTION",
    gitignore = ".gitignore",
    mod_sidebar = "R/mod_sidebar.R",
    mod_theme = "R/mod_theme.R",
    news = "NEWS.md",
    Readme.md = "README.md",
    Readme.Rmd = "README.Rmd",
    renv_settings = "renv/settings.dcf",
    renv_settings = "renv/settings.json",
    Renviron = ".Renviron",
    Rprofile = ".Rprofile"
  )


