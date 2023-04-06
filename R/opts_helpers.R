#' Check option value. **Use interactively only!**
#' @seealso write_opts
#' Code is dynamically generated when package is loaded. These will not auto generate properly in a deployed shiny app. See \link{write_opts} for a non-interactive alternative.
#' @param default if the specified option is not set in the options list, this value is returned. This facilitates retrieving an option and checking whether it is set and setting it separately if not.
#' @return \code{lgl}
#' @export
opts <- NULL


opts_helpers <- function(.Rprofile = ".Rprofile", .env = rlang::ns_env("UU")) {
  if (file.exists(.Rprofile) && is.null(get0("opts", envir = .env, inherits = FALSE))) {
    rprofile <- parse(.Rprofile)
    calls <- purrr::keep(rprofile, ~utils::head(as.character(.x[[1]]), 1) == "options")
    if (is_legit(calls)) {
      .opts <- rlang::call_args(calls[[1]])
      opts <- .opts |>
        purrr::imap(~{
          if (.y == "use_reprex")
            body <- rlang::expr({
              if (!interactive()) {
                out <- FALSE
              } else {
                out <- getOption("use_reprex", FALSE)
              }
              out
            })
          else
            body <- rlang::expr(getOption(!!.y, default = default))
          rlang::new_function(
            args = rlang::pairlist2(default = rlang::expr(!!.x)),
            body = body)
        })
      toggle <- .opts |>
        purrr::imap(~{

          body <- rlang::expr({
            if (missing(set)) {
              .val <- getOption(!!.y, default = !!.x)
              is_lgl <- is.logical(.val)
              if (!is_lgl) {
                UU::gwarn(!!glue::glue("{.y} is not logical and cannot be toggled. Use `set` to set value."))
                set <- .val
              } else
                set <- !.val
            }
            rlang::exec(options, rlang::list2(!!.y := set))
            cli::cli_inform(!!paste0(.y," set to {set}"))
          })
          rlang::new_function(
            args = rlang::pairlist2(set = ),
            body = body)
        })
      assign("opts", opts, .env)
      assign("toggle", toggle, .env)
      rp <- .Rprofile
      cli::cli_inform("{.pkg UU}: {.code opts} & {.code toggle} functions were generated for the following options in {.path {rp}}: {cli::col_br_blue(paste0(names(.opts), collapse = ', '))}", .frequency = "once", use_cli_format = TRUE, .frequency_id = "opts")
    }
  }
}

#' Write all the option checking functions to a file
#' @param file \code{chr} Default _R/opts.R_
#'
#' @return \code{msg}
#' @export
#'
write_opts <- function(file = "R/utils_opts.R") {
  if (UU::is_legit(opts)) {
    dump("opts", file = "R/opts.R", envir = rlang::ns_env("UU"))
  }
  if (file.exists(file))
    cli::cli_alert_success("option checking functions written to {.path {file}}")
}


#' Toggle an option
#' @description Meant for interactive use only.
#' @param opt \code{chr} option name to toggle. See `.virga_opts` for standard Virga options
#' @param set \code{obj} value to set the option to (takes precedence over toggle)
#'
#' @return \code{msg} to inform user of what the option is set to
#' @export
toggle <- NULL

#' Run expressions only when option `use_debug = TRUE`
#'
#' @param ... \code{expr} to evaluate
#'
#' @export

if_debug <- function(...) {
  if (opts$use_debug() %|try|% FALSE) {
    q <- rlang::enquos(...)
    purrr::walk(q, rlang::eval_tidy)
  }

}
