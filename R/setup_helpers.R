
need_write <- function(creds, file_lines, overwrite = FALSE, rprofile = FALSE) {
  if (is.null(names(creds)))
    creds <- rlang::set_names(creds)
  creds[purrr::imap_lgl(creds, ~{
    cred_exists <- stringr::str_detect(file_lines, stringr::regex(paste0(ifelse(rprofile, "^\\s{1,}?", "^"),.y,ifelse(rprofile, "[\\s]*\\=", "[\\n\\s]*$"))))
    if (!any(cred_exists) || overwrite)
      TRUE
    else
      FALSE
  })]
}


#' Write named keypairs to an _.Renviron_ / _.Rprofile_ file
#' @description Writes key pairs to _.Renviron_ / _.Rprofile_ and adds .Renviron to _.gitignore_ if not already there.
#' @param ... named keys to write
#' @inheritParams usethis::edit_r_environ
#' @param overwrite \code{(lgl)} should an existing key pair be overwritten. **Default: `FALSE`**
#' @param proj_dir \code{(chr)} project directory to write credentials to
#' @param rprofile \code{(lgl)} whether to write the keypairs to a \link[base]{options} call in a _.Rprofile_ file instead.
#' @return success message if a value is written
#' @export
#'

creds_to_renviron <- function(..., scope = c("user", "project")[1], overwrite = FALSE, proj_dir = ".", rprofile = FALSE) {
  .scope <- UU::match_letters(scope, "user", "project")
  .fp <- purrr::when(
    rprofile,
    isTRUE(.) ~ list(
      user = Sys.getenv("R_PROFILE_USER", "~/.Rprofile"),
      project = file.path(proj_dir, ".Rprofile")
    ),
    ~ list(
      user = Sys.getenv("R_ENVIRON_USER", "~/.Renviron"),
      project = file.path(proj_dir, ".Renviron")
    )
  )
  fp <- rlang::exec(switch,.scope,
               !!!.fp)

  UU::mkpath(fp, mkfile = TRUE, mkdir = FALSE)
  l <- readLines(fp)
  l <- l[nzchar(l)]
  creds <- rlang::dots_list(..., .named = TRUE)
  creds_to_write <- need_write(creds, l, overwrite, rprofile = rprofile)

  if (length(creds_to_write)) {
    if (rprofile)
      c2w <-  paste0("options(\n", paste0(paste0(names(creds_to_write), " = ", creds_to_write), collapse = ",\n") ,"\n)")
    else
      c2w <- paste0(names(creds_to_write), " = ","'",creds_to_write,"'")

    write(c2w, fp, append = TRUE)
    # Read the newly added vars/options
    purrr::when(rprofile,
                isTRUE(.) ~ source,
                ~ readRenviron)(fp)

    cli::cli_alert_success("{cli::col_green(paste0(names(creds_to_write), collapse = \", \"))} successfully written to {.path {fp}}")
  }
  if (scope == "project" && !rprofile)
    ignore_files(".Renviron", proj_dir)


}

#' Make key-pairs from a named character vector
#'
#' @param x \code{chr} named
#'
#' @return \code{chr}
#' @export
#'
#' @examples
#' key_pairs_text(Sys.getenv())
key_pairs_text <- function(x) {
  sprintf("%s = '%s'", names(x), x)
}

#' Find duplicates in key pairs
#'
#' @param x \code{chr/Dlist} Either character strings of keypairs or a Dlist returned by \link[base]{Sys.getenv}
#'
#' @return \code{lgl}
#' @export
#'
#' @examples
#' key_pairs_duplicated(Sys.getenv())
key_pairs_duplicated <- function(x, fromLast = TRUE) {
  .x <- x[nzchar(x)]
  if (!inherits(x, "Dlist") && is.null(names(x)))
    nms <- trimws(stringr::str_extract(.x, "^[^\\=]+"))
  else
    nms <- names(x)
  duplicated(nms, fromLast = fromLast)
}

#' Add lines to _.gitignore_
#'
#' @param lines \code{chr}
#' @param directory \code{(chr)} directory path of _.gitignore_ to be modified
#'
#' @return \code{informative messages}
#' @export

ignore_files <- function(lines, directory = ".") {
  fp <- file.path(directory, ".gitignore")
  UU::mkpath(fp, mkfile = TRUE)
  l <- readLines(fp)
  to_ignore <- need_write(lines, l)
  write(to_ignore, file = fp, append = TRUE)
  if (UU::is_legit(to_ignore))
    cli::cli_alert_info("{.val {paste0(lines, collapse = ',')}} added to {.path {fp}}")

}

#' Write expressions to the _.Rprofile_
#'
#' @param ... \code{exprs}
#' @param scope \code{chr} which _.Rprofile_ to write to
#'
#' @return \code{msg}
#' @export

write_to_rprofile <- function(..., scope = c("user", "project")[1]) {
  path <- usethis:::scoped_path_r(scope, ".Rprofile", envvar = "R_PROFILE_USER")
  full <- list(e = rlang::enexprs(...),
       l = parse(path))
  full <- purrr::map(full, ~{
    src <- purrr::map(.x, rlang::call_standardise)
    e_calls <- purrr::map_chr(src, ~rlang::expr_deparse(.x[[1]]))
    e_options_ind <- stringr::str_which(e_calls, "options")
    e_options_nms <- purrr::map(src[e_options_ind], rlang::call_args_names)
    e_options_vals <- purrr::map(src[e_options_ind], ~rlang::call_args(.x))
    list(exp = .x,
         calls = e_calls,
         options_ind = e_options_ind,
         options_nms = e_options_nms,
         options_vals = e_options_vals)
  })


  full <- purrr::map(full, ~{
    # if more than 1 option call in a single location
    if (length(.x$options_vals) > 1)
      .x$options_vals <- purrr::reduce(.x$options_vals, ~purrr::list_modify(.x, !!!.y))
    .x$options_vals <- purrr::flatten(.x$options_vals)
    .x
  })
  # Combine options calls
  full$combined <- list(options_vals = purrr::list_modify(full$l$options_vals, !!!full$e$options_vals))


  # combine any additional calls
  full$combined$exp <- rlang::parse_exprs(do.call(union, list(
    purrr::flatten_chr(purrr::map(full$e$exp[-full$e$options_ind], rlang::expr_deparse)),
    purrr::flatten_chr(purrr::map(full$l$exp[-full$l$options_ind], rlang::expr_deparse))
  )))

  full$combined$exp <- append(full$combined$exp, rlang::exec(base::call, "options", !!!full$combined$options_vals))
  full$combined$exp_txt <- do.call(c, purrr::map(full$combined$exp, rlang::expr_deparse))

  write(full$combined$exp_txt, path)
  cli::cli_alert_success("Lines written to {.path {path}}:\n{paste0(full$combined$exp_txt, collapse = '\n')}")

}


#' Write _R/aaa_reimports.R_ file
#' @param file \code{chr} path to file to write
#' @export

use_reimports <- function(file = "R/aaa_reimports.R") {
  write("#' @title Re-imports
#' @name Re-imports
#' @description Useful functions from other packages
#' @importFrom rlang `%||%` `%|%`
#' @importFrom UU `%|0|%` `%|try|%` `%|zchar|%` `%|legit|%`
NULL


#' @export
rlang::`%||%`

#' @export
rlang::`%|%`

#' @export
UU::`%|try|%`

#' @export
UU::`%|0|%`

#' @export
UU::`%|zchar|%`

#' @export
UU::`%|legit|%`

", file)
}
