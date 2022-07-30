#' Create a JS string with glue insertions
#' glue `.open = !@` & `.close = @#`
#' @param js \code{chr} JS code to \link[glue]{glue}
#' @param e \code{env} calling environment
#'
#' @return \code{chr}
#' @export
#'
#' @examples
#' glue_js("$(document).ready(() => {let x = *{tolower(FALSE)}*)")
glue_js <- function(js, e = rlang::caller_env(), .open = "*{", .close = "}*") {
  if (file.exists(js))
    .js <- glue::glue_collapse(readLines(js))
  else
    .js <- js
  glue::glue(.open = .open, .close = .close, .js, .envir = e)
}

#' Toggle \link[utils]{recover} on error when obtuse shiny errors are encountered
#' @export


shiny_error_recover <- function() {
  if (!identical(getOption("shiny.error"), utils::recover))
    options(shiny.error = utils::recover)
  else
    options(shiny.error = NULL)
}
