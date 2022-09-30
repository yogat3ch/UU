#' Go to a specified folder in the Virga Labs golem project
#' @param path \code{chr} to go to. Each method has a preset default based on the function name, but any method can be used to go to any folder specified.
#' @include utils_file_io.R
#' @export
folder <- NULL

#' Go to a specified file in the Virga Labs golem project
#' @param path \code{chr} file to go to. Each method has a preset default based on the function name, but any method can be used to go to any file specified.
#' @export
file <- NULL

#' All the files that can be navigated to with `file` if they exist
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

#' Move all js files to js folder
#' @export
move_js_to_folder <- function(files = UU::list.files2("inst/app/www", pattern = "\\.js$", include.dirs = FALSE), folder = dirs$js()) {
  if (!UU::is_legit(files)) {
    UU::gwarn("No files to move.")
  } else {
    purrr::walk(files, ~fs::file_move(.x, fs::path(folder, basename(.x))))
    if (all(purrr::map_lgl(files, ~file.exists(dirs$js(basename(.x))))))
      cli::cli_alert_success("Files moved to {.path {folder}}: {cli::col_br_blue(paste0(basename(files), collapse = ', '))}")
  }
}

