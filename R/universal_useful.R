#' @title Is object legit?
#' @description Is object non-null, non-empty, non-NA, and not a try-error?
#' @param x \code{(object)}
#' @return \code{(logical)}
#' @export

is_legit <- function(x) {
  !(is.null(x) || rlang::is_empty(x) || is.na(x) || inherits(x, "try-error"))
}

#' @title Is object an error class?
#' @description Is object of class `try-error`
#' @param x \code{(object)}
#' @return \code{(logical)}
#' @export

is_error <- function(x) {
  inherits(x, "try-error")
}

#' @title Statistical mode
#' @description Return the most frequenctly occuring item in a dataset
#' @param x \code{(vector)}
#' @return \code{(vector)}
#' @export

smode <- function(x) {
  .u <- unique(x)
  tab <- tabulate(match(x, .u))
  .u[tab == max(tab)]
}

#' @title Is path a file path
#' @description Given a path, is it a filepath?
#' @param path \code{(character)} path
#' @return \code{(logical)}
#' @export
is_filepath <- function(path) {
  grepl("\\.\\w{1,}", basename(path))
}

#' @title Extract the file extensions from a filepath
#' @description Given a path, extract the file extension
#' @param path \code{(character)} path
#' @return \code{(character)} with the extensions
#' @export

ext <- function(path) {
  out <- stringr::str_extract(path, "(?<=\\.)\\w+$")
  if (!is_legit(out))
    stop("Not a valid file path")
  out
}

#' @title Construct a path
#' @description Given a path, construct it if it does not exist.
#' @param path \code{(character)} path
#' @return \code{(logical)}
#' @export

mkpath <- function(path) {
  # Check to see if it's a file path and use just the directory path if so
  if (is_filepath(path))
    path <- dirname(path)

  if (!dir.exists(path)) {
    message(paste0(path, " does not exist. Creating..."))
    .path <- stringr::str_split(path, paste0("\\", .Platform$file.sep))[[1]]
    .wd <- stringr::str_split(getwd(), paste0("\\", .Platform$file.sep))[[1]]
    .path <- .path[!.path %in% .wd]
    purrr::walk(purrr::accumulate(.path, ~{
      .p <- paste0(.x,.Platform$file.sep,.y)
    }), ~{
      if (!dir.exists(.x)) dir.create(.x)
    })
  }
}
#' @title Provide the appropriate file read/write function
#' @description Return the appropriate read or write function given an object or `ext`ension as a character
#' @param object to determine the appropriate function for writing to disk
#' @param ext \code{(character)} file extension to determine the appropriate reading function
#' @return \code{(function)}
#' @export

file_io <- function(x, path, mkpath = TRUE) {
  if (mkpath && !missing(path))
    mkpath(path)

  if (is.character(x) && is_filepath(x) && missing(path)) {
    # if a filepath is passed as the first argument, load it based on the extension
    fn <- purrr::when(
      x,
      !file.exists(.) ~ stop(x, " not found"),
      length(.) > 1 ~ stop("Duplicate files found for ", x),
      grepl("csv$", ., ignore.case = TRUE) ~ readr::read_csv,
      grepl("feather$", ., ignore.case = TRUE) ~ feather::read_feather,
      grepl("rds$", ., ignore.case = TRUE) ~ readRDS
    )
    out <- fn(x)
  } else {
    # write the file based on it's type
    fn <- purrr::when(x,
                      inherits(., "data.frame") ~ feather::write_feather,
                      inherits(., "matrix") ~ function(x, path) {
                        feather::write_feather(tibble::as_tibble(x, .name_repair = "minimal"), path = path)
                      },
                      !inherits(., "data.frame") ~ saveRDS)
    fn(x, path)
  }

}

#' @title Make a file path name with underscores
#' @param \code({character}) file path
#' @export

make_names <- function(x) {
  stringr::str_replace_all(x, '[\\<|\\>|\\:\\"\\/\\|\\?\\*]', "_")
}

#' @title Provide the appropriate file extension for a given object
#' @param object to determine the appropriate function for writing to disk
#' @return \code{(character)}
#' @export
file_io_ext <- function(object) {
  purrr::when(object,
              inherits(., "data.frame") ~ ".feather",
              !inherits(., "data.frame") ~ ".rds")
}

#' @title Find an object by it's class
#' @param \code{(environment)} The environment to search
#' @param \code{(class)} The class to search for
#' @export

find_by_class <- function(class, e = rlang::caller_env()) {
  obj <- purrr::compact(purrr::map(ls(e), ~{
    out <- get0(.x, envir = e)
    purrr::when(out,
                inherits(., class) ~ .,
                ~ NULL)
  }))
  if (UU::is_legit(obj)) {
    if (length(obj) > 1)
      rlang::warn(paste0("More than one object with class: ", class,". Returning the first found."))
    out <- obj[[1]]
  } else {
    rlang::abort(paste0("Could not find object with class ",class,". Has it been instantiated?"))
  }
  out
}

#' @title Find the names in common
#' @description Given named objects, find the names in common
#' @param ... \code{(objects)}
#' @return \code{(character)} of the common names
#' @export
common_names <- function(...) {
  purrr::map(rlang::dots_list(..., .named = TRUE), names) |>
    {\(x) {do.call(c, x)}}()  |>
    table() |>
    {\(x) {names(x)[x == max(x)]}}()
}
