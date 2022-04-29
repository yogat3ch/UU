
need_write <- function(creds, file_lines, overwrite = FALSE, rprofile = FALSE) {
  if (is.null(names(creds)))
    creds <- rlang::set_names(creds)
  creds[purrr::imap_lgl(creds, ~{
    cred_exists <- grepl(paste0(ifelse(rprofile, "", "^"),.y, "\\s?\\="), file_lines)
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
      c2w <- paste0(names(creds_to_write), " = ", creds_to_write) |>
        paste0(collapse = ",\n") |>
        {\(x) {paste0("options(\n",x,"\n)")}}()
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
