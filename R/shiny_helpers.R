
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

#' Increment an in-place counter
#'
#' @param x \code{num/reactiveVal/reactiveValues} Any numeric to be incremented
#' @param e \code{env} Calling environment
#'
#' @return \code{none} Increments the counter in the parent environment (modifies in place)
#' @export
#'
#' @examples
#' x <- 1
#' increment(x)
#' x
increment <- function(x, e = rlang::caller_env()) {
  rv <- rlang::enexpr(x)
  if (inherits(x, "reactiveVal")) {
    i <- (x() %||% 0) + 1
    eval(rlang::call2(rlang::expr(!!rv), rlang::expr(!!i)), envir = e)
  } else {
    # Works for reactiveValues and other non reactives
    eval(rlang::call2(`<-`, rlang::expr(!!rv), rlang::expr((!!rv %||% 0) + 1)), envir = e)
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
#' @family shiny
#' @return \code{chr} without HTML tags
#' @export
#'
#' @examples
#' strip_html("The <strong>fox</strong> ran")
strip_html <- function(x) {
  gsub("<.*?>", "", x)
}

#' Character code conversion table
.character_codes <- tibble::tribble(
  ~Char, ~Numericcode, ~Namedcode,   ~Description,
   "\"",      "&#34;",   "&quot;", "double quote",
    "'",      "&#39;",   "&apos;", "single quote",
    "<",      "&#60;",     "&lt;",    "less than",
    ">",      "&#62;",     "&gt;", "greater than",
    "&",      "&#38;",    "&amp;",    "ampersand"
)


#' Replace HTML Character codes with their character equivalent
#' @description
#' See `?.character_codes` for conversion table. **Note** that this will not translate Ampersand if converting from Namedcode to character because it will translate the Namedcodes themselves.
#'
#' @param x \code{chr} string(s) in which character codes should be replaced
#' @param to_character \code{lgl} Change character codes to character, if `FALSE` character symbols are changed to character codes
#'
#' @return \code{chr} with substitutions
#' @export
#'
#' @examples
#' character_codes("5 < 10")
#' character_codes("5 &lt; 10")
#' character_codes("5 &lt; 10", to_character = FALSE)
character_codes <- function(x, to_character = TRUE) {
  action <- c(from = "Namedcode", to = "Char")[if (to_character) 1:2 else 2:1]
  # Don't do the ampersand if translating from Namedcodes, because it translates all of the appearances of ampersand in the namedcode output
  if (!to_character)
    .character_codes <- dplyr::filter(.character_codes, Char != "&")
  purrr::pwalk(.character_codes, \(...) {
    .x <- list(...)
    x <<- stringr::str_replace_all(x, stringr::fixed(.x[[action[1]]]), .x[[action[2]]])
  })
  return(x)
}
