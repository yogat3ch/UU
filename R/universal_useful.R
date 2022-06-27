#' @title Unload namespaces prior to package install
#' @param ns \code{(chr)} namespaces to unload
#' @export

unload_namespaces <- function(ns, verbose = FALSE) {
  if (missing(ns))
    ns <- loadedNamespaces()
  .ns <- ns[!ns %in% c("rstudio", "stats", "graphics", "utils", "datasets", "methods",
                "base", "bit64", "tools")]
  purrr::walk(.ns, purrr::possibly(unloadNamespace, NA, quiet = TRUE))
  .ns <- loadedNamespaces()
  if (length(.ns) < length(ns) && verbose)
    cli::cli_alert_success("Unloaded: {cli::col_grey(paste0(ns[!ns %in% .ns], sep = ', '))}")
}

#' Is zero-length character?
#'
#' @param x \code{chr}
#'
#' @return \code{lgl}
#' @export
#'
#' @examples
#' zchar("")
zchar <- function(x) {
  !nzchar(x)
}

#' The length of unique values in a vector
#'
#' @param x \code{vctr}
#'
#' @return \code{dbl}
#' @export
#'
#' @examples
#' len_unique(c(1,2,1))
len_unique <- function(x) {
  length(unique(x))
}

#' Read Javascript file
#'
#' @param filename \code{chr}
#'
#' @return \code{chr}
#' @export
#'

read_js <- function(filename) {
  glue::glue_collapse(readLines(filename), sep = "\n")
}

#' @inherit plyr::match_df title params description
#' @param out \code{obj} Of class matching the desired output. **Default** `NULL` returns a `data.frame` with the matching row in `y`. `numeric()` will return the matching index in `y` & `logical()` will return a matching logical index
#' @seealso plyr::match_df
#' @return \code{tbl/dbl/lgl} Depending on
#' @export
match_df <- function(x, y, out = NULL, on = NULL, verbose = FALSE) {
  if (is.null(on)) {
    on <- intersect(names(x), names(y))
    if (verbose)
      message("Matching on: ", paste(on, collapse = ", "))
  }
  keys <- plyr::join.keys(x, y, on)
  key_out(x, keys, out)
}
#' Handle different output type requests for `match_df`
#' @export
key_out <- function(x, keys, out) {
  UseMethod("key_out", out)
}
#' @export
key_out.default <- function(x, keys, out) {
  x[keys$x %in% keys$y, , drop = FALSE]
}
#' @export
key_out.numeric <- function(x, keys, out) {
  keys$x
}
#' @export
key_out.logical <- function(x, keys, out) {
  keys$y %in% keys$x
}
#' @title Is object legit?
#' @description Is object non-null, non-empty, non-NA, and not a try-error?
#' @param x \code{(object)}
#' @return \code{(logical)}
#' @export

is_legit <- function(x) {
  !(all(is.null(x)) || rlang::is_empty(x) || all(suppressWarnings(is.na(x))) || inherits(x, c("try-error", "error")))
}


#' @title Is object an error class?
#' @description Is object of class `try-error`
#' @param x \code{(object)}
#' @return \code{(logical)}
#' @export

is_error <- function(x) {
  inherits(x, c("try-error", "error"))
}

#' Convert numeric value to a string abbreviation with K, M, B for Thousand, Million & Billion
#'
#' @param n \code{num}
#' @param sf \code{num} significant figures to round to
#' @return \code{chr}
#' @export
#'
#' @examples
#' num2str(10000)
num2str <- function(n, sf = 2) {
  divisors <- purrr::map_dbl(1:3 * 3, ~{
    n / 10 ^ .x
  })
  i <- which.max(which(divisors >= 1))
  if (is_legit(i))
    paste0(round(divisors[i], 2), c("K", "M", "B")[i])
  else
    as.character(round(n, sf))
}
num2str <- Vectorize(num2str)

#' What is the human-readable suffix for a number
#'
#' @param n \code{num/chr} Either a numeric or number as a string with suffix input.
#' @seealso num2str
#' @return \code{chr}
#' @export
#'
#' @examples
#' num_suf(30000)
num_suf <- function(n) {
  UseMethod("num_suf")
}
#' @export
num_suf.numeric <- function(n) {
  stringr::str_extract(num2str(n), "[[:alpha:]]+")
}
#' @export
num_suf.character <- function(n) {
  stringr::str_extract(n, "[[:alpha:]]+")
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

#' Custom error message
#' @description Throw \link[rlang]{abort} with \link[cli]{format_error}
#' @param message \code{(chr)} The message to display, formatted as a bulleted list. The first element is displayed as an alert bullet prefixed with ! by default. Elements named "*", "i", and "x" are formatted as regular, info, and cross bullets respectively. See \link[rlang]{topic-condition-formatting} for more about bulleted messaging.
#' @param class \code{(class)} Subclass of the condition
#' @param trace \code{(trace)} A `trace` object created by \link[rlang]{trace_back}
#' @param parent \code{(cond)} Supply `parent` when you rethrow an error from a condition handler
#' @param e \code{(environment)} calling environment. Passed to `glue` for making the message
#' @export

gbort <- function (
  message = NULL,
  class = NULL,
  ...,
  trace = rlang::trace_back(bottom = 1),
  parent = NULL,
  e = rlang::caller_env()
) {
  rlang::abort(cli::format_error(message, .envir = e), class = class, ..., trace = trace, parent = parent)
}

#' Custom warning message
#' @description Throw \link[rlang]{warn} with \link[cli]{format_warning}
#' @inheritParams gbort
#' @param .frequency \code{(chr)} How frequently should the warning or message be displayed? By default ("always") it is displayed at each time. If "regularly", it is displayed once every 8 hours. If "once", it is displayed once per session.
#' @export

gwarn <- function (
  message = NULL,
  class = NULL,
  ...,
  .frequency = c("always",
                 "regularly", "once"),
  e = rlang::caller_env()
) {
  rlang::warn(cli::format_warning(message, .envir = e), class = class, ..., .frequency = .frequency)
}

#' Custom message
#' Message using \link[cli]{format_message} & \link[cli]{cat_line}
#' @inheritParams cli::format_message
#' @export

gmsg <- function (
  msg,
  e = rlang::caller_env()
) {
  cli::cat_line(cli::format_message(msg, .envir = e))
}
#' @title Extract the file extensions from a filepath
#' @description Given a path, extract the file extension
#' @param path \code{(character)} path
#' @param strip \code{(logical)} Whether to strip the extension from the path to return the bare file name
#' @return \code{(character)} with the extensions
#' @export

ext <- function(path, strip = FALSE) {
  if (strip) {
    out <- fs::path_ext_remove(path)
  } else {
    out <- fs::path_ext(path)
  }
  out
}

#' @title Construct a path
#' @description Given a path, construct it if it does not exist.
#' @param path \code{(character)} path
#' @param mkfile \code{(logical)} whether to make the file if it doesn't exist. IF `TRUE` and the path has an extension, both the directory and the file will be created
#' @param mkdir \code{(logical)} whether to make the directory if it doesn't exist. If `TRUE`, and the `path` does not have an extension, path will be created as a directory path.
#' @return \code{(informative messages)}
#' @export

mkpath <- function(path, mkfile = FALSE, mkdir = TRUE) {
  if (mkdir) {
    .dir <- ifelse(nzchar(ext(path)), dirname(path), path)
    if (!dir.exists(.dir)) {
      dir.create(.dir, recursive = TRUE)
      cli::cli_inform("Created dir: {.path {.dir}}")
    }

  }
  if (mkfile && !file.exists(path)) {
    file.create(path)
    cli::cli_inform("Created file: {.path {path}}.")
  }

}

is_project <- function() {
  desc <- utils::packageDescription("rstudioapi")
  if (is_legit(desc) && rstudioapi::isAvailable())
    is_legit(rstudioapi::getActiveProject())
  else
    FALSE
}


#' Load project & user-level _.Renviron_ & _.Rprofile_
#' @export
startup <- function() {
  if (!getOption("UU_startup", FALSE)) {
    options(UU_startup = TRUE)
    list(.Rprofile = Sys.getenv("R_PROFILE" , ".Rprofile"),
         .Rprofile_user = Sys.getenv("R_PROFILE_USER", "~/.Rprofile"),
         .Renviron = Sys.getenv("R_ENVIRON", ".Renviron"),
         .Renviron_user = Sys.getenv("R_ENVIRON_USER", "~/.Renviron")) |>
      purrr::iwalk(~{
        if (file.exists(.x)) {
          gmsg("{.path {.x}} loaded.")
          rlang::exec(switch(.y,
                             .Rprofile = ,
                             .Rprofile_user = base::source,
                             .Renviron = ,
                             .Renviron_user = base::readRenviron), .x)
        }

      })
  }
  on.exit(unload_namespaces(c("UU", "purrr", "rlang")))
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

#' Get a function from a package, abort if package not installed.
#'
#' @param pkg \code{chr} package
#' @param fn \code{fn} function name
#'
#' @return \code{fun}
#' @export
#'
#' @examples
#' need_pkg("utils", "recover")
need_pkg <- function(pkg, fn) {
  cmd <- cli::code_highlight(glue::glue("install.packages('{pkg}')"), code_theme = 'Twilight')
  getFromNamespace(fn, ns = pkg) %|try|% gbort(c("{fn} requires {pkg}. Use {cmd} first."))
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


load_obj <- function(file) {
  e <- new.env()
  load(file, e)
  .nms <- ls(e, all.names = TRUE)
  if (length(.nms) == 1){
    out <- e[[.nms]]
  } else {
    out <- rlang::env_get_list(e, nms = .nms)
  }
  out
}
.size <- data.frame(
  stringsAsFactors = FALSE,
  check.names = FALSE,
  IEC = c(1, 1024^(1:8)),
  SI = c(1, 1000^(1:8)),
  type = c("b", "Kb", "Mb", "Gb", "Tb", "Pb", "Eb", "Zb", "Yb")
)
#' Digital storage size conversion
#' See \link[utils]{object.size}
#' @param x \code{(numeric)}
#' @param in_unit \code{(character)} units of x
#' @param out_unit \code{(character)} units of output number
#' @param standard \code{(character)}
#' @return \code{(numeric)}
#' @export
#'
#' @examples
#' size(50, "mb")
#' size(50, "gb")
#' size(50, "gb", "mb")

size <- function(x, in_unit = c("b", "Kb", "Mb", "Gb", "Tb", "Pb", "Eb", "Zb", "Yb")[1], out_unit = "b", standard = c("IEC", "SI")[1]) {
  .standard <- UU::match_letters(standard, "IEC", "SI", ignore.case = TRUE)
  .in_unit <- UU::match_letters(in_unit, .size$type, ignore.case = TRUE)
  .out_unit <- UU::match_letters(out_unit, .size$type, ignore.case = TRUE)
  (.size[grepl(paste0("^",.in_unit), .size$type, ignore.case = TRUE), .standard, drop = TRUE] * x) / .size[grepl(paste0("^",.out_unit), .size$type, ignore.case = TRUE), .standard, drop = TRUE]
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
#' object_fn(data.frame(a = 2, b = 3))


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
#' @return \code{(POSIXct)} Last modified time
#' @export
last_updated <- function(x, path = FALSE) {
  if (!path) {
    .files <- x
  } else {
    .files <- UU::list.files2(x)
  }
  if (is_legit(.files))
    .files <- do.call(c, purrr::map(rlang::set_names(.files), purrr::possibly(~file.info(.x)$mtime, lubridate::NA_POSIXct_)))
  else
    gwarn("{cli::code_highlight('UU::last_updated', code_theme = 'Twilight')}: No files detected")
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
  .files <- UU::last_updated(x, path)
  tibble::tibble(full_path = names(.files),
                 basename = basename(full_path),
                 last_updated = .files,
                 threshold = threshold,
                 needs_update = (threshold > last_updated) %|% TRUE)
}


#' @title Make a file path name with underscores
#' @param \code{(character)} file path
#' @export

make_names <- function(x) {
  fs::path_sanitize(x)
}

#' @title Find an object by it's class
#' @param \code{(environment)} The environment to search
#' @param \code{(class)} The class to search for
#' @export

find_by_class <- function(class, e = rlang::caller_env()) {
  obj <- purrr::compact(purrr::map(ls(e), purrr::possibly(~{
    out <- get0(.x, envir = e)
    purrr::when(out, inherits(., class) ~ ., ~NULL)
  }, NULL)))
  if (UU::is_legit(obj)) {
    if (length(obj) > 1)
      rlang::warn(paste0("More than one object with class: ", class,". Returning the first found."))
    out <- obj[[1]]
  } else {
    rlang::warn(paste0("Could not find object with class ",class,". Has it been instantiated?"))
  }
  out
}

#' @title Find the names in common
#' @description Given named objects, find the names in common
#' @param ... \code{(objects)}
#' @return \code{(character)} of the common names
#' @export

common_names <- function(...) {
  x <- table(do.call(c, purrr::map(rlang::dots_list(..., .named = TRUE), names)))
  names(x)[x == max(x)]
}

#' @title Match the first `n` letters to supplied arguments
#' @description Case insensitive matching of argument to possibilities provided in ellipsis.
#' @param x \code{(character)} to match on
#' @param ... \code{(character)} vectors to match against
#' @param n \code{(numeric)} how many characters of `x` to use in matching. Set to `NULL` to use all
#' @param multiple \code{(logical)} are multiple matches allowed? If `FALSE` (Default) only the first match is returned.
#' @inheritParams base::grep
#' @param capitalize \code{(logical)} whether to capitalize the result
#' @return \code{(character)} vector of matches
#' @export

match_letters <- function(x, ..., n = 1, multiple = FALSE, ignore.case = FALSE, capitalize = FALSE) {
  if (!is.character(x)) return(x)
  if (!is.null(n))
    x <- substr(x, 0, n)
  if (is.null(x)) {
    out <- x
  } else {
    out <- tryCatch(grep(ifelse(length(x) > 1, paste0("^",x, collapse = "|"), paste0("^" ,x)), unlist(rlang::dots_list(...), use.names = FALSE), perl = TRUE, value = TRUE, ignore.case = ignore.case),
                    error = function(e) {
                      message(paste0(e))
                    })
    if (!multiple)
      out <- out[1]

    if (capitalize && !is.null(out))
      out <- purrr::map_chr(out, ~purrr::when(nchar(.x) == 1,. ~ toupper(.x), ~ gsub("^(\\w)(\\w+)","\\U\\1\\L\\2", .x, perl = TRUE)))
  }
  out
}

class_coercion_fn <- function(.class) {
  switch(.class,
         numeric = ,
         character = ,
         logical = ,
         factor = ,
         integer = getFromNamespace(paste0("as.",.class), "base"),
         Date = lubridate::as_date,
         POSIXCt = lubridate::as_datetime
         )
}

#' Match the classes of one object to that of another object
#'
#' @param x \code{(object)} object to be matched
#' @param y \code{(object)} object to be coerced
#'
#' @return \code{y} with class types matching that of \code{x}
#' @export

map_class <- function(x, y) {
    purrr::map2(purrr::map(y, class), x, ~class_coercion_fn(.x)(.y))
}

#' @title Get the missing arguments from the function as character
#'
#' @param calling_function \code{(function)} see \link[rlang]{caller_fn} or \link[base]{sys.function}
#' @param corresponding_call \code{(call)} The call where the `calling_function` is called. See \link[rlang]{trace_back} or \link[base]{sys.call}
#' @param include_null \code{(logical)} Include args set to `NULL`?
#' @param exclude_defaults \code{(logical)} Exclude arguments wth defaults?
#'
#' @return \code{(character)}
#' @export
#'
#' @examples
#' a <- function(a, b = NULL, c = "d") {
#'   missing_args()
#' }
#' a()
missing_args <-
  function(calling_function = rlang::caller_fn(1),
           corresponding_call = sys.call(1),
           include_null = TRUE,
           exclude_defaults = TRUE)
  {
    all_args <- formals(calling_function)

    arg_names <- names(all_args)
    matched_call <- match.call(calling_function,
                               corresponding_call,
                               expand.dots = FALSE)

    passed_args <- names(as.list(matched_call)[-1])
    out <- setdiff(arg_names, passed_args)
    if (include_null)
      out <-
      c(out, setdiff(names(purrr::keep(
        all_args, ~ is.null(.x)
      )), passed_args))
    if (exclude_defaults)
      out <-
      setdiff(out, names(purrr::keep(
        all_args, ~ !is.null(.x) & !rlang::is_missing(.x)
      )))
    out
  }
#' @title Get the names of all exported functions in a package
#'
#' @param x \code{(character)} Package name
#' @param all.names \code{(logical)} Include names that begin with characters `.` `_` etc
#'
#' @return \code{(character)}
#' @export
#'
#' @examples
#' get_package_fns("dplyr")
get_package_fns <- function(x, all.names = FALSE, pattern, negate = FALSE) {
  nms <- ls(getNamespace(x), all.names=all.names)
  if (!missing(pattern))
    nms<- stringr::str_subset(nms, pattern, negate)
  nms
}



#' @title Retrieve the function name
#' @description Sometimes a function is passed down the call stack and it's name is unknown. This function finds the name without having to pass it down the call stack as an argument.
#' @param fn \code{(function)} for which to retrieve the name
#'
#' @return \code{(character)} of the functions name
#' @export

fn_name <- function(fn) {
  trimws(stringr::str_extract(readLines(utils::getSrcFilename(fn, full.names = T))[utils::getSrcLocation(fn)], ".*(?=\\<\\-)"))

}

#' Create a compound regex grouped statement
#'
#' @param x \code{(character)} regex strings
#' @param pre \code{(character)} regex tokens to precede each string group. IE `(?:[pre]x)` w/out the braces
#' @param suf \code{(character)} regex tokens to follow each string group. IE `(?:x[suf])` w/out the braces
#' @param type \code{(character)} `|`, `&` supported
#' @return \code{(character)} grouped regex statement
#' @export

regex_op <- function(x, type = "|", prefix = "", suffix = "") {
  paste0(paste0("(?",switch(type, `|` = ":", `&` = "=.*"),prefix, x,suffix,")"), collapse = switch(type, `|` = "|", `&` = ""))
}

#' Create a compound regex grouped OR statement
#'
#' @inheritParams regex_op
#' @return \code{(character)} grouped regex OR statement
#' @export

regex_or <- function(x, prefix = "", suffix = "") regex_op(x, prefix = prefix, suffix = suffix)


# ----------------------- Mon Apr 08 16:49:54 2019 ------------------------#
#' @title rle_df
#'
#' Given an \code{\link[base]{rle}} this function will return a data.frame of starts, ends, and indexes thereof of the run lengths.
#' Credit: \url{https://stackoverflow.com/questions/43875716/find-start-and-end-positions-indices-of-runs-consecutive-values}
#' @param x \code{(vector)} An object for which to run an `rle`
#' @return \item{(data.frame)}{ with length, values, start and end indices.}
#' @examples
#' rle_df(sample(c(TRUE,FALSE), replace = TRUE, 100))
#' @export

rle_df <- function(x) {
  input_rle <- rle(x)
  .out <- unclass(input_rle)
  .out <- dplyr::select(dplyr::mutate(tibble::as_tibble(.out),
                                      end = cumsum(lengths),
                                      start = c(1, dplyr::lag(end)[-1] + 1)),
                        c(1,2,4,3))
  return(.out)
}

#' Create a sequence from the start to the end for a given value from an `rle_df` for indexing
#'
#' @param rle_df \code{(tbl)} See `rle_df`
#' @param value \code{(any)} Value to filter for in the `values` column. Require the values in the value column to be unique.
#'
#' @return \code{(dbl)}
#' @export
#'
#' @examples
#' rle_seq(rle_df(rep(letters[1:3], each = 3)), "c")
rle_seq <- function(rle_df, value) {
  r <- dplyr::filter(rle_df, values == value)
  seq(r$start, r$end)
}

#' @title Detect possible duplicates of rows or columns after a join
#'
#' @param after \code{(data.frame)} after the join
#' @param before \code{(data.frame)} from before the join **Optional** but required for row comparison.
#' @param halt_fn \code{(function)} to notify, default \link[rlang]{warn}.
#' @seealso [rlang::abort()] [base::message()]

#' @export
#'
#' @examples
#' a = data.frame(a = c(1, 2, 3, 4, 3, 5), b = 1:6)
#' b = data.frame(a = c(1, 2, 3, 4, 5), c = letters[1:5])
#' after <- dplyr::left_join(a, b)
#' join_check(b, after, halt_fn = message)
join_check <- function(after, before, halt_fn = rlang::warn) {
  nm_a <- rlang::expr_deparse(rlang::enexpr(after))
  .msg <- character(0)
  if (!missing(before)) {
    nm_b <- rlang::expr_deparse(rlang::enexpr(before))
    nb <- nrow(before)
    na <- nrow(after)
    if (nb != na)
      .msg <- paste0(.msg,
             " - Row count discrepancies:\n",
             nm_b," - ", nb,"\n",
             nm_a," - ", na,"\n")
  }
  c_dupes <- stringr::str_detect(names(after), "\\.x$|\\.y$")
  if(any(c_dupes))
    paste0(.msg,
           " - Column duplicates:\n",
           paste0(names(after)[c_dupes], collapse = ", "))



  if (is_legit(.msg))
    halt_fn(paste0("Possible join issues detected!\n",.msg))
}
