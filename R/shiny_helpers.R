
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
#' @param as_chr \code{lgl} Whether to use \code{\link[UU]{as_js}} on the output `FALSE` or \link[base]{as.character} `TRUE`. **Default FALSE**
#' @param e \code{env} calling environment
#' @inheritParams glue::glue
#' @return \code{chr}
#' @export
#'
#' @examples
#' glue_js("$(document).ready(() => {let x = *{tolower(FALSE)}*)")
glue_js <- function(js, as_chr = FALSE, e = rlang::caller_env(), .open = "*{", .close = "}*") {
  .js <- if (length(js) == 1 && file.exists(js))
    readLines(js)
  else
    js
  out <- glue::glue(.open = .open, .close = .close, glue::glue_collapse(.js, sep = "\n"), .envir = e)
  if (as_chr)
    out <- as.character(out)
  else
    out <- as_js(out)
  return(out)
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

#' Make a randomly formatted name into snakecase id
#'
#' @param x \code{chr}
#'
#' @return \code{chr} as a snakecase id
#' @export
#'
#' @examples
#' nm_to_id("This convoluted name")
nm_to_id <- function(x) {
  paste0(stringr::str_extract_all(tolower(x), "[[:alnum:]]+")[[1]], collapse = "_")
}
