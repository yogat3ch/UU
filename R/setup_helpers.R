
need_write <- function(creds, file_lines, overwrite = FALSE) {
  if (is.null(names(creds)))
    creds <- rlang::set_names(creds)
  creds[purrr::imap_lgl(creds, ~{
    cred_exists <- grepl(paste0("^",.y), file_lines)
    if (!any(cred_exists) || overwrite)
      TRUE
    else
      FALSE
  })]
}


#' Write named credentials to .Renviron file
#' @description Writes key pairs to .Renviron and adds .Renviron to _.gitignore_ if not already there.
#' @param ... named keys to write
#' @inheritParams usethis::edit_r_environ
#' @param overwrite \code{(lgl)} should an existing key be overwritten. **Default: `FALSE`**
#' @param proj_dir \code{(chr)} project directory to write credentials to
#'
#' @return success message if a value is written
#' @export
#'

creds_to_renviron <- function(..., scope = c("user", "project")[1], overwrite = FALSE, proj_dir = ".") {
  .scope <- UU::match_letters(scope, "user", "project")
  fp <- switch(.scope,
               user = Sys.getenv("R_ENVIRON_USER", "~/.Renviron"),
               project = file.path(proj_dir, ".Renviron"))
  UU::mkpath(fp, mkfile = TRUE)
  l <- readLines(fp)
  l <- l[nzchar(l)]
  creds <- rlang::dots_list(..., .named = TRUE)
  creds_to_write <- need_write(creds, l, overwrite)

  if (length(creds_to_write)) {
    write(paste0(names(creds_to_write), " = '", creds_to_write,"'"), fp, append = TRUE)
    readRenviron(fp)
    cli::cli_alert_success("{cli::col_green(paste0(names(creds_to_write), collapse = \", \"))} successfully written to {.path {fp}}")
  }
  if (scope == "project")
    ignore_files(".Renviron", proj_dir)


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