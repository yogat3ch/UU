
#' Custom error message
#' @description Throw \link[rlang]{abort} with \link[cli]{format_error}
#' @param message \code{(chr)} The message to display, formatted as a bulleted list. The first element is displayed as an alert bullet prefixed with ! by default. Elements named "*", "i", and "x" are formatted as regular, info, and cross bullets respectively. See \link[rlang]{topic-condition-formatting} for more about bulleted messaging.
#' @param class \code{(class)} Subclass of the condition
#' @param trace \code{(trace)} A `trace` object created by \link[rlang]{trace_back}
#' @param parent \code{(cond)} Supply `parent` when you rethrow an error from a condition handler
#' @param e \code{(environment)} calling environment. Passed to `glue` for making the message
#' @family condition signaling
#' @export

gbort <- function (
    message = NULL,
    class = NULL,
    ...,
    trace = rlang::trace_back(bottom = 2),
    parent = NULL,
    e = rlang::caller_env()
) {
  rlang::abort(cli::format_error(message, .envir = e), class = class, ..., trace = trace, call = dplyr::last(trace)$call[[1]], parent = parent)
}

#' Custom warning message
#' @description Throw \link[cli]{cli_alert_warning} with \link[cli]{format_warning}
#' @inheritParams gbort
#' @inheritParams rlang::warn
#' @family condition signaling
#' @export

gwarn <- function (
    message = NULL,
    body = NULL,
    footer = NULL,
    .frequency = c("always", "regularly", "once"),
    e = rlang::caller_env()
) {
  rlang::warn(cli::format_warning(message, .envir = e) , use_cli_format = TRUE, .frequency = .frequency, .frequency_id = "UU", body = body, footer = footer)
}

#' Custom info message
#' @description Provide info with \link[rlang]{inform}
#' @inheritParams gbort
#' @inheritParams rlang::inform
#' @family condition signaling
#' @export

ginfo <- function (
    message = NULL,
    body = NULL,
    footer = NULL,
    .frequency = c("always", "regularly", "once"),
    e = rlang::caller_env()
) {
  rlang::inform(cli::format_message(message, .envir = e) , use_cli_format = TRUE, .frequency = .frequency, .frequency_id = "UU", body = body, footer = footer)
}

#' Custom message
#' Message using \link[cli]{format_message} & \link[cli]{cat_line}
#' @inheritParams cli::format_message
#' @family condition signaling
#' @export

gmsg <- function (
    msg,
    e = rlang::caller_env()
) {
  cli::cli_text(cli::format_message(msg, .envir = e))
}


#' Writes a trace back as a json for error logging
#' Useful for remote error logging on deployed shiny apps, such as via Sentry
#' @param e \code{error} Error condition object, optional
#' @param file \code{chr} path to file to be written.
#' @param tb \code{rlang_trace} Trace back
#'
#' @return \code{None} called for side effect of writing to file
#' @export
#' @family condition signaling
#' @examples
#' trace_back_json()
trace_back_json <- function(e = NULL, file = glue::glue("{lubridate::format_ISO8601(Sys.time())}.json"), tb = rlang::trace_back(bottom = 1)) {
  out <- list(
    trace_back = dplyr::mutate(tibble::as_tibble(tb), call = Vectorize(\(.x)glue::glue_collapse(sep = "\n", rlang::expr_deparse(.x)))(call))
  )
  if (!is.null(e))
    out$error = e
  jsonlite::write_json(out, file)
}
