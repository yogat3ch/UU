#' Create a JS string with glue insertions
#' glue `.open = !@` & `.close = @#`
#' @param js \code{chr} JS code to \link[glue]{glue}
#' @param e \code{env} calling environment
#'
#' @return \code{chr}
#' @export
#'
#' @examples
#' gluejs("$(document).ready(() => {let x = !@tolower(F)@#})")
gluejs <- function(js, e = rlang::caller_env()) {
  glue::glue(.open = "!@", .close = "@#", js, .envir = e)
}
