#' Create a JS string with glue insertions
#' glue `.open = !@` & `.close = @#`
#' @param js \code{chr} JS code to \link[glue]{glue}
#' @param e \code{env} calling environment
#'
#' @return \code{chr}
#' @export
#'
#' @examples
#' gluejs("$(document).ready(() => {let x = *{tolower(F)}*)")
glue_js <- function(js, e = rlang::caller_env(), .open = "*{", .close = "}*") {
  if (file.exists(js))
    .js <- glue::glue_collapse(readLines(js))
  else
    .js <- js
  glue::glue(.open = .open, .close = .close, .js, .envir = e)
}
