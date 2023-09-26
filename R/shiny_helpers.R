
#' Preserve a string as JS/HTML (prevent translation of characters)
#'
#' @param x \code{chr}
#' @family shiny
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
#' @return \code{chr}
#' @export
#' @family shiny
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
#' @family shiny

shiny_error_recover <- function() {
  if (!identical(getOption("shiny.error"), utils::recover)) {
    UU::assign_in_ns(getOption("shiny.error"), nm = ".shiny.error", ns_env = .GlobalEnv)
    options(shiny.error = utils::recover)
    cli::cli_inform("option 'shiny.error' set to `utils::recover`")
  } else {
    error_val <- UU::get_from_ns(".shiny.error")
    options(shiny.error = error_val)
    cli::cli_inform("option 'shiny.error' restored to previous value")
  }

}

#' Read Javascript file
#'
#' @param filename \code{chr}
#'
#' @return \code{chr}
#' @export
#' @family shiny

read_js <- function(filename) {
  as_js(glue::glue_collapse(readLines(filename), sep = "\n"))
}

#' Make a randomly formatted name into snakecase id
#'
#' @param x \code{chr}
#'
#' @return \code{chr} as a snakecase id
#' @export
#' @family shiny
#' @examples
#' nm_to_id("This convoluted name")
nm_to_id <- function(x) {
  paste0(stringr::str_extract_all(tolower(x), "[[:alnum:]]+")[[1]], collapse = "_")
}

#' Remove all HTML tags from a character vector
#'
#' @param x \code{chr}
#'
#' @return \code{chr} without HTML tags
#' @export
#'
#' @examples
#' strip_html("The <strong>fox</strong> ran")
strip_html <- function(x) {
  gsub("<.*?>", "", x)
}
