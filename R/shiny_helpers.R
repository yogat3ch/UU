
#' Preserve a string as JS/HTML (prevent translation of characters)
#'
#' @param x \code{chr}
#'
#' @return \code{chr, HTML, JS_EVAL}
#' @export
#'

as_js <- function(x) {
  structure(
    x,
    html = TRUE,
    noWS = TRUE,
    class = c("html", "character", "JS_EVAL")
  )
}

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
  .js <- if (length(js) == 1 && file.exists(js))
    readLines(js)
  else
    js
  as_js(glue::glue(.open = .open, .close = .close, glue::glue_collapse(.js), .envir = e))
}

#' Toggle \link[utils]{recover} on error when obtuse shiny errors are encountered
#' @export


shiny_error_recover <- function() {
  if (!identical(getOption("shiny.error"), utils::recover))
    options(shiny.error = utils::recover)
  else
    options(shiny.error = NULL)
}

#' Read Javascript file
#'
#' @param filename \code{chr}
#'
#' @return \code{chr}
#' @export
#'

read_js <- function(filename) {
  as_js(glue::glue_collapse(readLines(filename), sep = "\n"))
}
