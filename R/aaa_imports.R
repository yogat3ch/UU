#' @importFrom rlang `%|%` `%||%`
#' @export
rlang::`%||%`
#' @export
rlang::`%|%`

#' Add a function to reimports
#'
#' @param pkg \code{chr} package name
#' @param fun \code{chr} function name
#'
#' @return \code{msg}
#' @export
use_reimport <- function(pkg, fun) {
  imports <- list.files2("R", pattern = "imports") %|0|% UU::gbort("No file in {.path R/} with imports in the name.")
  l <- readLines(imports)
  fn <- glue::glue("{pkg}::{fun}")
  if (!any(stringr::str_detect(l, stringr::fixed(as.character(fn))))) {
    glue::glue("
    #' @importFrom {pkg} {fun}
    #' @export
    {fn}
    ") |>
      write(imports, append = TRUE)
    l <- readLines(imports)
    if (any(stringr::str_detect(l, stringr::fixed(as.character(fn)))))
      cli::cli_alert_success("{.code {fn}} added to {.path {imports}}.")
  } else {
    gwarn("{fn} is already imported.")
  }

}
