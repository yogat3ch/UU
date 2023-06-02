#' @title Extract the file extensions from a filepath
#' @description Given a path, extract the file extension
#' @param path \code{(character)} path
#' @param strip \code{(logical)} Whether to strip the extension from the path to return the bare file name
#' @param new_ext \code{chr} New extension for the filename
#' @return \code{(character)} with the extensions
#' @family file IO
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




#' @title Converts input to a specified type output
#' @description Given various inputs, provide a col_type specification in the format indicated by `outtype`
#' @param x \code{(vector/function)} One of:
#' \itemize{
#'   \item{column}{ \code{(any)}}
#'   \item{a type specification from HUD}{ \code{(character)}}
#'   \item{a readr `parse_*` function (See \link[readr]{parse_logical})}{ \code{(function)}}
#'   \item{a readr type specification (See \link[readr]{cols})}{ \code{(character)}}
#' }
#' @param outtype \code{(character)} One of:
#' \itemize{
#'   \item{\code{"chr"}}{ Returns the class as a readr abbreviation (See \link[readr]{cols})}
#'   \item{\code{"hud"}}{ \code{(character)} a type specification from HUD}
#'   \item{\code{"fun"}}{a readr `parse_*` function (See \link[readr]{parse_logical})}{ \code{(function)}}
#'   \item{\code{"typ"}}{ \code{(character)} The R data class}
#'   \item{\code{"col"}}{ \code{(character)} The \code{\link[readr]{collector}}}
#' }
#' @return See outtype
#' @family file IO
#' @export

col_types <- function(x, outtype = c("chr", "hud", "fun", "typ", "col")[1]) {

  hash <- tibble::tribble(~ typ, ~ hud, ~ fun, ~ chr, ~col,
                          "integer", "I", UU::need_pkg("readr", "parse_integer"), "i", rlang::expr(UU::need_pkg("readr", "col_integer")()),
                          "numeric", "I", UU::need_pkg("readr", "parse_number"), "n", rlang::expr(UU::need_pkg("readr", "col_number")()),
                          "character", "S", UU::need_pkg("readr", "parse_character"), "c", rlang::expr(UU::need_pkg("readr", "col_character")()),
                          "logical", "S", UU::need_pkg("readr", "parse_logical"), "l", rlang::expr(UU::need_pkg("readr", "col_logical")()),
                          "factor", "I", UU::need_pkg("readr", "parse_factor"), "f", rlang::expr(UU::need_pkg("readr", "col_factor")()),
                          "Date", "D", UU::need_pkg("readr", "parse_date"), "D", rlang::expr(UU::need_pkg("readr", "col_date")()),
                          "POSIXct", "T", UU::need_pkg("readr", "parse_datetime"), "T", rlang::expr(UU::need_pkg("readr", "col_datetime")()),
                          "POSIXt", "T", UU::need_pkg("readr", "parse_datetime"), "T", rlang::expr(UU::need_pkg("readr", "col_datetime")()),
                          "POSIXlt", "T", UU::need_pkg("readr", "parse_datetime"), "T", rlang::expr(UU::need_pkg("readr", "col_datetime")()),
                          "list", "", UU::need_pkg("readr", "guess_parser"), "?", rlang::expr(UU::need_pkg("readr", "col_guess")())
  )
  intype <- purrr::when(x,
                        all(. %in% hash$typ) ~ "typ",
                        all(. %in% hash$hud) ~ "hud",
                        is.function(.) ~ "fun",
                        all(. %in% hash$chr) ~ "chr",
                        ~ "col")


  type <- switch(intype,
                 col = hash$typ[hash$typ %in% class(x)[1]],
                 typ = hash$typ[hash$typ %in% x[1]],
                 hud = hash$typ[stringr::str_which(hash$hud, x)[1]],
                 fun = hash$typ[purrr::map_lgl(hash$fun, identical, y = x)],
                 chr = hash$typ[hash$chr %in% x])

  out <- unique(hash[[outtype]][hash$typ %in% type])
  if (outtype %in% c("fun", "col"))
    out <- out[[1]]
  out
}

#' Return the size of a package, or all packages in a folder
#'
#' @param packages \code{chr} of package names
#' @param path \code{chr} with package folders in it such as \code{\link[base]{.libPaths}}
#'
#' @return \code{chr/tbl} depending on whether packages or path is provided
#' @export
#' @family file IO
#' @examples
#' package_size()
package_size <- function(packages, path = .libPaths()[1]) {
  if (!missing(packages)) {
    purrr::map_vec(.ptype = character(), rlang::set_names(packages), \(.x) {
      system(paste("du -sh", system.file(package = .x), "| awk '{print $1}'"), intern =
               TRUE)
    })
  } else {
    tibble::tibble(dir_path = list.files2(path, full.names = TRUE),
                          dir_size = purrr::map_vec(.ptype = numeric(), dir_path, \(.x) {sum(unlist(fs::dir_map(.x, all = TRUE, file.size)))}),
                          dir_sizeMB = size_(dir_size, out_unit = "MB"),
                          pkg_name = basename(dir_path)) |>
      dplyr::arrange(dir_size) |>
      dplyr::select(pkg_name, dir_sizeMB, dir_size, dir_path)
  }
}

#' @title Make a file path name with underscores
#' @param \code{(character)} file path
#' @family file IO
#' @export

make_names <- function(x) {
  fs::path_sanitize(x)
}

#' Read a dependency from file
#'
#' @param filename \code{(chr)} path to the file
#' @param ... Passed on to read function. See \link[UU]{file_fn} for which function will be used based on the filename
#' @return file contents
#' @family file IO
#' @export

dep_read <- function(filename, ...) {
  if (file.exists(filename))
    file_fn(filename)(filename, ...)
  else
    gbort("{.path {filename}} not found.")
}

#' @rdname dep_read
#' @export
object_read <- dep_read

#' Write a dependency to file
#'
#' @param x \code{(object)} to be written
#' @inheritParams dep_read
#' @param ... Passed on to write function. See \link[UU]{file_fn} for which function will be used based on the filename
#' @return \code{(message)} indicating success
#' @family file IO
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
#' @family file IO
#' @examples
#' dir_fn("data")("random_data", "file", ext = "txt")
dir_fn <- function(base_dir) {
  rlang::new_function(args = rlang::pairlist2(... =, ext = ""), body = rlang::expr(fs::path(!!base_dir, ..., ext = ext)))
}


#' @title Return the appropriate function for reading the specified path/extension
#'
#' @param x \code{(character)} The extension name or the path to the file
#' @param write \code{(logical)} Return the writing function? **Default** `FALSE` to return the reading function
#' @return \code{(function)}
#' @export
#' @family file IO
#' @examples
#' file_fn("csv")
#' file_fn("csv", write = TRUE)

file_fn <- function(x, write = FALSE) {
  .ext <- ext(x)
  if (write) {
    switch(tolower(.ext),
           csv = need_pkg("readr", "write_csv"),
           feather = need_pkg("arrow", "write_feather"),
           rds = saveRDS,
           png =,
           jpg =,
           jpeg = need_pkg("ggplot2", "ggsave"),
           xlsx =,
           xls = ,
           xlsm = need_pkg("writexl", "write_xlsx")
           )
  } else {
    switch(tolower(.ext),
           csv = need_pkg("readr", "read_csv"),
           feather = need_pkg("arrow", "read_feather"),
           rds = saveRDS,
           png =,
           jpg =,
           jpeg = need_pkg("magick", "img_read"),
           xlsx =,
           xls = ,
           xlsm = need_pkg("readxl", "read_excel"),
           readLines
           )

  }

}

#' Write lines at a specific location in a file
#'
#' @param file \code{chr} path to file
#' @param ... \code{chr} lines to write
#' @param after \code{num/chr} either a line number of "end" to write it at the end
#' @family file IO
#' @return the resulting file
#' @export

write_lines <- function(file, ..., after = "end") {
  lines <- readLines(file)
  to_add <- unlist(rlang::dots_list(...))
  if (identical(after, "end"))
    after <- length(lines)
  lines <- append(lines, to_add, after = after)
  write(lines, file = file)
  cli::cat_line(lines)
}

#' Path functions for commonly used directories
#' @param ... \code{(chr)} directory paths
#' @param mkpath \code{lgl} Whether to return a path regardless of whether the file/dir exists or not
#' @param ext \code{(chr)} file extension
#' @param mustWork \code{lgl} If `TRUE`, an error is given if there are no matching files.
#' @usage dirs\$data()
#' @family file IO
#' @export
#' @examples dirs$data("mydata", ext = "csv", mkpath = TRUE)
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
#' @family file IO
#' @return \code{msg} and a new file
#' @export

write_dir_fn <- function(outfile = "R/utils_dir_fns.R", overwrite = TRUE, for_golem = file.exists("R/app_ui.R")) {
  if (file.exists(outfile) && overwrite)
    file.remove(outfile)
  mkpath(outfile, mkfile = TRUE)

  pkg_nm <- pkg_name()
  app_sys <- function() {}
  fn <- if (for_golem)
    list("app_sys", mustWork = rlang::expr(mustWork))
  else
    list("path_package", .ns = "fs", package = pkg_nm)

  dirs <- purrr::map(dirs, ~{
    .exp <- rlang::expr({
      .path <- fs::path(!!.x(), ..., ext = ext)
      out <- if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        if (!(!!for_golem) && mustWork)
          !!rlang::exec(rlang::call2, !!!fn, rlang::expr(.path))
        else
          .path
      } else
        .path
      return(out)
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
#' @family file IO
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
#' @family file IO
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
#' @family file IO
#' @examples
#' object_fn(1:15)


object_fn <- function(x, filepath) {
  pkgs <- rlang::set_names(c("arrow", "readr", "base"))
  i <- purrr::map_lgl(pkgs, \(.x) {
    require(.x, character.only = TRUE, quietly = TRUE)
  }) |>
    which()

  csv_write <- switch(names(pkgs)[min(i)],
         arrow = need_pkg("arrow", "write_feather"),
         readr = need_pkg("readr", "write_csv"),
         base = utils::write.csv)



  out <- purrr::when(
    x,
    inherits(., "data.frame") ~ csv_write,
    inherits(., "matrix") ~ function(x, path) {
      csv_write(tibble::as_tibble(x, .name_repair = "minimal"), path = path)
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
#' @family file IO
#' @export

object_write <- function(x, filename, path = ".", ..., verbose = TRUE) {

  if (!dir.exists(path))
    mkpath(path)

  .ext <- object_ext(x)
  img <- stringr::str_detect(.ext, "png$")
  if (missing(filename)) {
    # Create the full filename
    nm <- rlang::expr_deparse(rlang::enexpr(x))
    filename <- fs::path(nm, ext = .ext)
  }
  fp <- fs::path(path, filename)

  # order the arguments to the saving function
  .dots <- rlang::dots_list(..., .named = TRUE)
  if (img)
    .dots <- purrr::list_modify(list(plot = x, filename = fp, device = "png", dpi = "screen"), !!!.dots)
  else
    .dots <- purrr::list_modify(list(x, fp), !!!.dots)

  # write the file based on it's type

  fn <- switch(.ext,
         R = dump,
         object_fn(x, fp))
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
#' @family file IO
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
#' @family file IO
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
#' @family file IO
#' @export
is_filepath <- function(path) {
  grepl("\\.[a-zA-Z0-9]{1,}$", basename(path))
}

#' Move all files to a folder
#'
#' @param files \code{chr} of files to move
#' @param folder \code{chr} folder to move them to
#'
#' @family file IO
#' @export
move_files_to_folder <- function(files = UU::list.files2("inst/app/www", pattern = "\\.js$", include.dirs = FALSE), folder = dirs$js()) {
  if (!UU::is_legit(files)) {
    UU::gwarn("No files to move.")
  } else {
    purrr::walk(files, ~fs::file_move(.x, fs::path(folder, basename(.x))))
    if (all(purrr::map_lgl(files, ~file.exists(fs::path(folder, basename(.x))))))
      cli::cli_alert_success("Files moved to {.path {folder}}: {cli::col_br_blue(paste0(basename(files), collapse = ', '))}")
  }
}
