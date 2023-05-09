
need_write <-
  function(creds,
           file_lines,
           overwrite = FALSE,
           rprofile = FALSE) {
    if (is.null(names(creds)))
      creds <- rlang::set_names(creds)
    if (!overwrite) {
      cred_rgx <- purrr::imap_chr(creds, ~stringr::regex(paste0(
        ifelse(rprofile, "^\\s{1,}?", "^"),
        .y,
        "[\\s]*\\="
      )))
      cred_exists <- purrr::imap_lgl(cred_rgx, ~any(stringr::str_detect(file_lines, .x), na.rm = TRUE))
      needs_write <- !cred_exists
    } else {
      needs_write <- rep(TRUE, length(creds))
    }
    creds[needs_write]
  }


#' Write named keypairs to an _.Renviron_ / _.Rprofile_ file
#' @description Writes key pairs to _.Renviron_ / _.Rprofile_ and adds .Renviron to _.gitignore_ if not already there.
#' @param ... named keys to write
#' @param scope \code{chr} Edit globally for the current user, or locally for the current project
#' @param overwrite \code{(lgl)} should an existing key pair be overwritten. **Default: `FALSE`**
#' @param proj_dir \code{(chr)} project directory to write credentials to
#' @param rprofile \code{(lgl)} whether to write the keypairs to a \link[base]{options} call in a _.Rprofile_ file instead.
#' @return success message if a value is written
#' @family project setup
#' @export
#'

creds_to_renviron <- function(..., scope = c("user", "project")[1], overwrite = FALSE, proj_dir = ".", rprofile = FALSE) {
  .scope <- match_letters(scope, "user", "project")
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

  mkpath(fp, mkfile = TRUE, mkdir = FALSE)
  creds <- rlang::dots_list(..., .named = TRUE)
  l <- readLines(fp)
  l <- l[nzchar(l)]
  creds_to_write <- need_write(creds, l, overwrite, rprofile = rprofile)

  if (length(creds_to_write)) {
    if (rprofile)
      c2w <-  paste0("options(\n", paste0(paste0(names(creds_to_write), " = ", creds_to_write), collapse = ",\n") ,"\n)")
    else
      c2w <- key_pairs_text(creds_to_write)

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
#' @usage key_pairs_text(Sys.getenv())
#' @export
#' @family project setup
#'


key_pairs_text <- function(x) {
  sprintf("%s = '%s'", names(x), x)
}

#' Find duplicates in key pairs
#'
#' @param x \code{chr/Dlist} Either character strings of keypairs or a Dlist returned by \link[base]{Sys.getenv}
#' @usage key_pairs_duplicated(Sys.getenv())
#' @return \code{lgl}
#' @export
#' @family project setup
#'
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
#' @param directory \code{(chr)} directory path to `ignore_file`
#' @param ignore_file \code{chr} filename holding ignores to be modified. Default _.gitignore_
#' @return \code{informative messages}
#' @family project setup
#' @export

ignore_files <- function(lines, directory = ".", ignore_file = ".gitignore") {
  fp <- file.path(directory, ignore_file)
  mkpath(fp, mkfile = TRUE)
  usethis::write_union(fp, lines = lines)
}

#' Write expressions to the _.Rprofile_
#'
#' @param ... \code{exprs}
#' @param scope \code{chr} which _.Rprofile_ to write to
#' @family project setup
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


#' Write _R/aaa_reimports.R_ file with all current infix operators
#' @description
#' All infix operators available: `r cli::pluralize("{stringr::str_subset(ls(rlang::pkg_env('UU')), '^%')}")`
#' @inheritParams base::file.copy
#' @family project setup
#' @param file \code{chr} path to file to write
#' @export

use_UU_reimports <- function(file = "R/aaa_reimports.R", overwrite = FALSE) {
  if (!file.exists(file)) {
    file.copy(
      system.file("extdata/reimports.R", package = "UU"),
      file,
      overwrite = overwrite
    )
    cli::cli_alert_success("{.path {file}} written successfully. Document the package to use.")
  } else {
    cli::cli_alert_danger("{.path {file}} already exists. Set `overwrite = TRUE` to overwrite.")
  }
}

#' Add a function to reimports
#'
#' @param pkg \code{chr} package name
#' @param fun \code{chr} function name
#' @family project setup
#' @return \code{msg}
#' @export
use_reimport <- function(pkg, fun) {
  imports <- list.files2("R", pattern = "imports") %|0|% gbort("No file in {.path R/} with imports in the name.")
  l <- readLines(imports)
  fn <- glue::glue("{pkg}::{fun}")
  if (!any(stringr::str_detect(l, stringr::fixed(as.character(fn))))) {
    glue::glue("\n#' @importFrom {pkg} {fun}\n#' @export\n{fn}
    ") |>
      write(imports, append = TRUE)
    l <- readLines(imports)
    if (any(stringr::str_detect(l, stringr::fixed(as.character(fn)))))
      cli::cli_alert_success("{.code {fn}} added to {.path {imports}}.")
  } else {
    gwarn("{fn} is already imported.")
  }

}


#' Install a package
#' @family project setup
#' @param pkg \code{chr} package names **Required**
#' @param remote \code{chr} github remote
#' @param ... \code{args} passed on to \link[base]{install.packages} if no `remote` supplied or \link[remotes]{install_github} if `remote` supplied
#' @param to_desc \code{lgl} Add the package dependency to the _DESCRIPTION_ file?
#' @param snapshot \code{lgl} Run \code{renv::\link[renv]{snapshot}}
#' @inherit remotes::install_github return
#' @inheritDotParams remotes::install_github
#' @return Installs the package, add the version to the _DESCRIPTION_ file, and \link[renv]{snapshot}s the package to the _renv.lock_ file if present.
#' @export
#'

install_remote <- function(pkg, remote, ..., to_desc = TRUE, snapshot = TRUE) {
  # If it's a package & user opts to write the description file, add as dependency
  .write_desc <- (is.character(pkgload::package_file()) %|try|% FALSE) && to_desc
  if (missing(remote)) {
    install.packages(pkg, ...)
    if (.write_desc)
      purrr::walk(pkg, ~usethis::use_package(.x, min_version = TRUE))
  } else {
    .remotes <- glue::glue("{remote}/{pkg}")
    remotes::install_github(.remotes, ...)
    if (.write_desc)
      purrr::walk2(pkg, .remotes, ~usethis::use_dev_package(.x, remote = .y))
  }
  if (file.exists("renv.lock") && snapshot)
    UU::need_pkg("renv","snapshot")(prompt = FALSE)
}



#' Create a table of functions and their uses
#'
#' @param package \code{chr} package name
#'
#' @return \code{shiny.tag}
#' @export
#' @family project setup

fun_docs_table <- function(package = pkgload::pkg_name()) {
  rds <- purrr::map(list.files2("man", recursive = FALSE, pattern = "Rd$"), \(.x) {
    doc <- tools::parse_Rd(.x)
    rlang::set_names(doc, vapply(
      doc,
      function(.x) { gsub("\\\\", "", attr(.x, 'Rd_tag')) },
      character(1)
    ))
  })
  purrr::map_dfr(rds, \(.x) {
    doc <- .x
    desc <- purrr::map_chr(rlang::set_names(c("name","concept", "title", "description")), \(.x) {
      browser(expr = .x == "family")
      glue::glue_collapse(as.character(unlist(doc[[.x]] %||% "")))
    })
    names(desc) <- stringr::str_to_title(names(desc))
    tibble::tibble_row(
      !!!desc
    )
  }) |>
    dplyr::arrange(Concept)
}
