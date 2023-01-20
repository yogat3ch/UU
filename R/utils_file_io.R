#' @title Extract the file extensions from a filepath
#' @description Given a path, extract the file extension
#' @param path \code{(character)} path
#' @param strip \code{(logical)} Whether to strip the extension from the path to return the bare file name
#' @param new_ext \code{chr} New extension for the filename
#' @return \code{(character)} with the extensions
#' @export

ext <- function(path, strip = FALSE, new_ext) {
  new <- !missing(new_ext)
  if (new)
    strip <- TRUE

  if (strip) {
    out <- fs::path_ext_remove(path)
  } else {
    out <- fs::path_ext(path)
  }
  if (new)
    out <- fs::path(out, ext = new_ext)
  out
}

#' Read a dependency from file
#'
#' @param filename \code{(chr)} path to the file
#' @param ... Passed on to read function. See \link[UU]{file_fn} for which function will be used based on the filename
#' @return file contents
#' @export

dep_read <- function(filename, ...) {
  if (file.exists(filename))
    file_fn(filename)(filename, ...)
  else
    gbort("{.path {filename}} not found.")
}

#' Write a dependency to file
#'
#' @param x \code{(object)} to be written
#' @inheritParams dep_read
#' @param ... Passed on to write function. See \link[UU]{file_fn} for which function will be used based on the filename
#' @return \code{(message)} indicating success
#' @export

dep_write <- function(x, filename, ...) {
  .x <- cli::code_highlight(rlang::expr_deparse(rlang::enexpr(x)), code_theme = "Twilight")
  if (UU::ext(filename) == "feather")
    UU::need_pkg("arrow", "write_feather")(x, filename, compression = "uncompressed")
  else
    UU::file_fn(filename, write = TRUE)(x, filename, ...)
  if (file.exists(filename) && file.info(filename)$mtime > (Sys.time() - lubridate::seconds(10)))
    cli::cli_alert_success("{.x} written to {.path {filename}} ")
  else
    UU::gwarn("Failed to write {.x} to {.path {filename}}")
}

#' Create a directory path pointing function
#'
#' @param base_dir \code{(chr)} the base directory to which the path should point
#'
#' @return \code{(fun)} with pointing function using \link[fs]{path}
#' @export
#'
#' @examples
#' dir_fn("data")("random_data", "file", ext = "txt")
dir_fn <- function(base_dir) {
  rlang::new_function(args = rlang::pairlist2(... =, ext = ""), body = rlang::expr(fs::path(!!base_dir, ..., ext = ext)))
}


#' Return a logical on an interval
#'
#' @param file \code{chr} filename in which to store the interval time
#' @param interval \code{period/duration} Default 1 week
#'
#' @return \code{lgl}
#' @export
#'
#' @examples
#' time_elapsed()
time_elapsed <- function(file = ".interval_timer.rds", interval = lubridate::weeks(1)) {
  if (file.exists(".gitignore"))
    usethis::use_git_ignore(".interval_timer.rds")
  if (file.exists(file)) {
    prev_time <- readRDS(file)
    first_run <- FALSE
  } else {
    prev_time <- Sys.time()
    saveRDS(prev_time, file)
    first_run <- TRUE
  }


  if (Sys.time() > prev_time + interval || first_run) {
    saveRDS(Sys.time(), file)
    TRUE
  } else
    FALSE
}

#' @title Return the appropriate function for reading the specified path/extension
#'
#' @param x \code{(character)} The extension name or the path to the file
#' @param write \code{(logical)} Return the writing function? **Default** `FALSE` to return the reading function
#' @return \code{(function)}
#' @export
#'
#' @examples
#' file_fn("csv")
#' file_fn("csv", write = TRUE)

file_fn <- function(x, write = FALSE) {
  purrr::when(
    x,
    grepl("csv$", ., ignore.case = TRUE) && write ~ need_pkg("readr", "write_csv"),
    grepl("feather$", ., ignore.case = TRUE) && write ~ need_pkg("arrow", "write_feather"),
    grepl("rds$", ., ignore.case = TRUE) && write ~ saveRDS,
    grepl("(?:rda$)|(?:rdata$)", ., ignore.case = TRUE) && write ~ save,
    grepl("csv$", ., ignore.case = TRUE) ~ need_pkg("readr", "read_csv"),
    grepl("feather$", ., ignore.case = TRUE)  ~ need_pkg("arrow", "read_feather"),
    grepl("rds$", ., ignore.case = TRUE) ~ readRDS,
    grepl(regex_or(c("rda", "rdata"), suf = "$"), ., ignore.case = TRUE) ~ load_obj,
    grepl("(?:png$)|(?:jpg$)|(?:jpeg$)", ., ignore.case = TRUE) && write ~ need_pkg("ggplot2", "ggsave"),
    grepl("(?:png$)|(?:jpg$)|(?:jpeg$)", ., ignore.case = TRUE) ~ need_pkg("magick", "img_read"),
    grepl(regex_or(c("xlsx", "xls", "xlsm"), suf = "$"), ., ignore.case = TRUE) && write ~ need_pkg("writexl", "write_xlsx"),
    grepl(regex_or(c("xlsx", "xls", "xlsm"), suf = "$"), ., ignore.case = TRUE) ~ need_pkg("readxl", "read_excel"),
    ~ readLines
  )

}

#' Path functions for commonly used directories
#' @param ... \code{(chr)} directory paths
#' @param mkpath \code{lgl} Whether to return a path regardless of whether the file/dir exists or not
#' @param ext \code{(chr)} file extension
#' @param mustWork \code{lgl} If `TRUE`, an error is given if there are no matching files.
#' @usage dirs$data()
#' @export
#' @examples dirs$data("mydata", ext = "csv")
dirs <- purrr::map(
  list(
    css = "inst/app/www/css",
    data = "data",
    dev = "dev",
    extdata = "inst/extdata",
    img = "inst/app/www/img",
    inst = "inst",
    js = "inst/app/www/js",
    R = "R",
    renv = "renv",
    tests = "tests/testthat",
    top = ".",
    vault = "inst/vault",
    www = "inst/app/www"
  ),
  ~ dir_fn(.x)
)

#' Write `dir` helper function that are robust to dev vs deployed package states
#'
#' @param outfile \code{chr} path to file to write. Default _R/utils_dir_fns.R_
#' @param overwrite \code{lgl} Whether to overwrite the existing file. Default `TRUE`
#' @param for_golem \code{lgl} Whether to use the `app_sys` function if package is a golem package
#' @return \code{msg} and a new file
#' @export

write_dir_fn <- function(outfile = "R/utils_dir_fns.R", overwrite = TRUE, for_golem = file.exists("R/app_ui.R")) {
  if (file.exists(outfile) && overwrite)
    file.remove(outfile)
  mkpath(outfile, mkfile = TRUE)

  pkg_nm <- pkg_name()
  app_sys <- function() {}
  fn <- purrr::when(for_golem, isTRUE(.) ~ list("app_sys", mustWork = rlang::expr(mustWork)), ~ list("path_package", .ns = "fs", package = pkg_nm))

  dirs <- purrr::map(dirs, ~{
    .exp <- rlang::expr({
      .path <- fs::path(!!.x(), ..., ext = ext)
      if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        !!rlang::exec(rlang::call2, !!!fn, rlang::expr(.path))
      } else
        .path
    })
    rlang::new_function(args = rlang::pairlist2(... =, ext = "", mkpath = FALSE, mustWork = FALSE), body = .exp)
  })


  suppressWarnings(dump("dirs", outfile))
  l <- readLines(outfile)
  write(c("#' directory path generation convenience functions",
          "#' @param ... \\code{(chr)} directory paths",
          "#' @param mkpath \\code{lgl} Whether to return a path regardless of whether the file/dir exists or not",
          "#' @param ext \\code{(chr)} file extension",
          "#' @param mustWork \\code{lgl} If `TRUE`, an error is given if there are no matching files.",
          "#' @usage dirs$data()",
          "#' @export",
          "#' @examples dirs$data(\"mydata\", ext = \"csv\")",
          l), file = outfile)


}

#' @title Construct a path
#' @description Given a path, construct it if it does not exist.
#' @param path \code{(character)} path
#' @param mkfile \code{(logical)} whether to make the file if it doesn't exist. IF `TRUE` and the path has an extension, both the directory and the file will be created
#' @param mkdir \code{(logical)} whether to make the directory if it doesn't exist. If `TRUE`, and the `path` does not have an extension, path will be created as a directory path.
#' @return \code{(informative messages)}
#' @export

mkpath <- function(path, mkfile = FALSE, mkdir = TRUE) {
  if (mkdir && !(!stringr::str_detect(path, "\\/") && mkfile)) {
    .dir <- if (nzchar(ext(path)) || mkfile) {
      dirname(path)
    } else {
      path
    }
    if (!dir.exists(.dir) && !file.exists(path)) {
      dir.create(.dir, recursive = TRUE)
      cli::cli_inform("Created dir: {.path {.dir}}")
    }

  }
  if (mkfile && !file.exists(path)) {
    file.create(path)
    cli::cli_inform("Created file: {.path {path}}.")
  }

}


#' @title List full file paths with the file name as the name
#'
#' @param path \code{(character)} dir path to files
#' @inheritDotParams base::list.files
#' @inheritParams base::list.files
#' @return \code{(named character)}
#' @export
#'
#' @examples
#' list.files2("~")
list.files2 <- function(path = ".", full.names =  TRUE, ...) {
  if (!fs::dir_exists(path))
    gbort("{.path {path}} does not exist.")
  x <- list.files(path, full.names = full.names, ...)
  rlang::set_names(x, ext(basename(x), strip = TRUE))
}

#' @title Provide the appropriate file extension for a given object
#' @param object to determine the appropriate function for writing to disk
#' @return \code{(character)}
#' @export
object_ext <- function(object) {
  purrr::when(object,
              inherits(., "data.frame") ~ ".feather",
              inherits(., "ggplot") ~ ".png",
              !inherits(., "data.frame") ~ ".rds")
}

#' @title Return the appropriate function for writing the supplied object to disk
#'
#' @param x \code{(object)}
#' @details
#' \itemize{
#'   \item{\code{data.frame/matrix}}{ \link[arrow]{write_feather}}
#'   \item{\code{ggplot}}{ \link[ggplot2]{ggsave}}
#'   \item{\code{anything else}}{ \link[base]{saveRDS}}
#' }
#' @return \code{(function)} See details for which function
#' @export
#'
#' @examples
#' object_fn(1:15)


object_fn <- function(x, filepath) {
  out <- purrr::when(
    x,
    inherits(., "data.frame") ~ need_pkg("arrow", "write_feather"),
    inherits(., "matrix") ~ function(x, path) {
      need_pkg("arrow", "write_feather")(tibble::as_tibble(x, .name_repair = "minimal"), path = path)
    },
    inherits(., "ggplot") ~ need_pkg("ggplot2", "ggsave"),
    !inherits(., "data.frame") ~ saveRDS
  )
  if (!missing(filepath)) {
    if (!identical(out, file_fn(filepath, write = TRUE)))
      stop(glue::glue("Mismatch between class of object `x` & it's `filepath` extension. Is this the right object?"))
  }
  out
}

#' @title Provide the appropriate file read/write function
#' @description Write an object to disk
#' @param x \code{(object)} to write to disk
#' @param filename \code{(chr)} without path to write to disk
#' @param path \code{(chr)} where file will be written
#' @param ... arguments passed on to methods. See `?object_fn`
#' @param verbose \code{(logical)} Whether to print saved messages. **Default** `TRUE`
#' @return Success message if file is written
#' @export

object_write <- function(x, filename, path, ..., verbose = TRUE) {
  if (missing(path))
    .path <- dirname(filename)
  else
    .path <- path

  if (!dir.exists(.path))
    mkpath(.path)

  # Create the full filename
  .ext <- object_ext(x)
  img <- stringr::str_detect(.ext, "png$")
  if (missing(filename))
    .fname <- rlang::expr_deparse(rlang::enexpr(x))
  else
    .fname <- basename(filename)


  fp <- file.path(.path, paste0(.fname, ifelse(is_filepath(filename), "", .ext)))

  # order the arguments to the saving function
  .dots <- rlang::dots_list(..., .named = TRUE)
  if (img)
    .dots <- purrr::list_modify(list(plot = x, filename = fp, device = "png", dpi = "screen"), !!!.dots)
  else
    .dots <- purrr::list_modify(list(x, fp), !!!.dots)

  # write the file based on it's type

  fn <- object_fn(x, fp)
  rlang::exec(fn, !!!.dots)

  if (file.exists(fp) && verbose)
    cli::cli_alert_success("Saved {.path {fp}}")
  else if (!file.exists(fp))
    stop(fp, " could not be written to disk.")
  fp
}
#' @title Gather last updated times for on-disk files
#' @description Check the last modified time files or paths
#' @param x \code{(chr)} file path to check last updated time
#' @param path \code{(lgl)} whether x is a path and all files should be checked
#' @inheritDotParams base::list.files
#' @return \code{(POSIXct)} Last modified time
#' @export
last_updated <- function(x, path = FALSE, ...) {
  if (!path) {
    .files <- x
  } else {
    .files <- list.files2(x, ...)
  }
  if (is_legit(.files))
    .files <- do.call(c, purrr::map(rlang::set_names(.files), purrr::possibly(~file.info(.x)$mtime, lubridate::NA_POSIXct_)))
  else
    gwarn("{cli::code_highlight('last_updated', code_theme = 'Twilight')}: No files detected")
  .files
}

#' Check if files need to be updated
#'
#' @inheritParams last_updated
#' @param threshold The threshold time. If files have last modified times less than this time, they will be marked as needing an update.
#'
#' @return \code{(tbl)} with columns:
#' \itemize{
#'   \item{\code{full_path}}{ The full path to the file(s)}
#'   \item{\code{basename}}{ The file(s) basename}
#'   \item{\code{last_updated}}{ The last updated time}
#'   \item{\code{threshold}}{ The threshold time for comparison}
#'   \item{\code{needs_update}}{ logical as to whether the file should be updated}
#' }
#' @export

needs_update <- function(x, path = FALSE, threshold = lubridate::floor_date(Sys.time(), "day")) {
  .files <- last_updated(x, path)
  tibble::tibble(full_path = names(.files),
                 basename = basename(full_path),
                 last_updated = .files,
                 threshold = threshold,
                 needs_update = (threshold > last_updated) %|% TRUE)
}

#' @title Is path a file path
#' @description Given a path, is it a filepath?
#' @param path \code{(character)} path
#' @return \code{(logical)}
#' @export
is_filepath <- function(path) {
  grepl("\\.\\w{1,}", basename(path))
}
