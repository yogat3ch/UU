#' @title Is object legit?
#' @description Is object non-null, non-empty, non-NA, and not a try-error?
#' @param x \code{(object)}
#' @return \code{(logical)}
#' @export

is_legit <- function(x) {
  !(all(is.null(x)) || rlang::is_empty(x) || all(suppressWarnings(is.na(x))) || inherits(x, c("try-error", "error")))
}

#' @title Try an expression
#' @description Calls the expression (LHS) & if it fails return RHS
#' @param lhs \code{(expression)} to try
#' @param rhs \code{()}
#'
#' @return rhs
#' @export

`%|try|%` <- function(lhs, rhs) {
  tryCatch(eval(rlang::enexpr(lhs)), error = rlang::as_function(~{eval(rlang::enexpr(rhs))}))
}

#' @title Is object an error class?
#' @description Is object of class `try-error`
#' @param x \code{(object)}
#' @return \code{(logical)}
#' @export

is_error <- function(x) {
  inherits(x, c("try-error", "error"))
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
#' Throw \link[rlang]{abort} with \link[cli]{format_error}
#' @inheritParams rlang::abort
#' @param e \code{(environment)} calling environment. Passed to `glue` for making the message
#' @export

gbort <- function (
  message = NULL,
  class = NULL,
  ...,
  trace = rlang::trace_back(),
  parent = NULL,
  e = rlang::caller_env()
) {
  rlang::abort(cli::format_error(message, .envir = e), class, ..., trace, parent)
}

#' Custom warning message
#' Throw \link[rlang]{warn} with \link[cli]{format_warning}
#' @inheritParams rlang::warn
#' @inheritParams gbort
#' @export

gwarn <- function (
  message = NULL,
  class = NULL,
  ...,
  .frequency = c("always",
                 "regularly", "once"),
  .frequency_id = NULL,
  e = rlang::caller_env()
) {
  rlang::warn(cli::format_warning(message, .envir = e), class, ..., .frequency, .frequency_id)
}

#' Custom message
#' Message using \link[cli]{format_message}
#' @inheritParams rlang::warn
#' @inheritParams gbort
#' @export

gmessage <- function (
  message = NULL,
  class = NULL,
  ...,
  .frequency = c("always",
                 "regularly", "once"),
  .frequency_id = NULL,
  e = rlang::caller_env()
) {
  rlang::warn(cli::format_warning(message, .envir = e), class, ..., .frequency, .frequency_id)
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
#' @return \code{(logical)}
#' @export

mkpath <- function(path) {
  if (!dir.exists(path)) {
    # Check to see if it's a file path and use just the directory path if so
    if (is_filepath(path))
      path <- dirname(path)

    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
      cli::cli_inform("Created {.path {path}}")
    }
  }
}


#' @title List full file paths with the file name as the name
#'
#' @param path \code{(character)} dir path to files
#' @inheritDotParams base::list.files
#' @return \code{(named character)}
#' @export
#'
#' @examples
#' list.files2("~")
list.files2 <- function(path = ".", full.names =  TRUE, ...) {
  list.files(path, full.names = full.names, ...) |>
    {\(x) {rlang::set_names(x, ext(basename(x), strip = TRUE))}}()
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
    grepl("csv$", ., ignore.case = TRUE) && write ~ readr::write_csv,
    grepl("feather$", ., ignore.case = TRUE) && write ~ arrow::write_feather,
    grepl("rds$", ., ignore.case = TRUE) && write ~ saveRDS,
    grepl("(?:png$)|(?:jpg$)|(?:jpeg$)", ., ignore.case = TRUE) && write~ ggplot2::ggsave,
    grepl("(?:rda$)|(?:rdata$)", ., ignore.case = TRUE) && write ~ save,
    grepl("csv$", ., ignore.case = TRUE) ~ readr::read_csv,
    grepl("feather$", ., ignore.case = TRUE)  ~ arrow::read_feather,
    grepl("rds$", ., ignore.case = TRUE) ~ readRDS,
    grepl("(?:rda$)|(?:rdata$)", ., ignore.case = TRUE) ~ load_obj,
    grepl("(?:png$)|(?:jpg$)|(?:jpeg$)", ., ignore.case = TRUE) ~ purrr::when(
        UU::is_legit(utils::packageVersion("magick")),
        . ~ magick::image_read,
        ~ stop(x, " is an image and requires the magick package.")
      ),
    ~ readLines
  )

}

load_obj <- function(file) {
  e <- new.env()
  load(file, e)
  .nms <- ls(e, all.names = TRUE)
  if (length(.nms) == 1)
    out <- e[[.nms]]
  else {
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

#' @title Return the appropriate function for writing the supplied object to disk
#'
#' @param x \code{(object)}
#' @return \code{(function)}
#' @export
#'
#' @examples
#' object_fn(1:15)
#' object_fn(data.frame(a = 2, b = 3))


object_fn <- function(x, filepath) {
  out <- purrr::when(
    x,
    inherits(., "data.frame") ~ arrow::write_feather,
    inherits(., "matrix") ~ function(x, path) {
      arrow::write_feather(tibble::as_tibble(x, .name_repair = "minimal"), path = path)
    },
    inherits(., "ggplot") ~ ggplot2::ggsave,
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
#' @inheritParams ggplot2::ggsave
#' @param verbose \code{(logical)} Whether to print saved messages. **Default** `TRUE`
#' @inheritDotParams ggplot2::ggsave
#' @inheritDotParams base::saveRDS
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
  if (img)
    knitr::plot_crop(fp)
  if (file.exists(fp) && verbose)
    cli::cli_alert_success("Saved {.path {fp}}")
  else if (!file.exists(fp))
    stop(fp, " could not be written to disk.")
  fp
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
  purrr::map(rlang::dots_list(..., .named = TRUE), names) |>
    {\(x) {do.call(c, x)}}()  |>
    table() |>
    {\(x) {names(x)[x == max(x)]}}()
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
#' @param type \code{(character)} `|`, `&` supported
#' @return \code{(character)} grouped regex statement
#' @export

regex_op <- function(x, type = "|") {
  paste0(paste0("(?",switch(type, `|` = ":", `&` = "=.*"), x,")"), collapse = switch(type, `|` = "|", `&` = ""))
}

#' Create a compound regex grouped OR statement
#'
#' @param x \code{(character)} regex strings
#' @return \code{(character)} grouped regex OR statement
#' @export

regex_or <- function(x) regex_op(x)

#' @title start_cluster
#' @description Creates a compute cluster
#' @param workers \code{(numeric)} number of worker nodes. Defaults to 3/4 of the available.
#' @param outfile \code{(character/logical)} Path and name of the outfile or `FALSE` to disable an outfile.
#' @return \code{(RichSOCKcluster)}
#' @export

start_cluster <- function(workers = future::availableCores() %/% 2, timeout = 60 * 60 * 5, outfile = file.path(getwd(), "cl_out.log")) {
  .args <- list(
    workers = workers,
    timeout = timeout
  )
  if (!isFALSE(outfile)) {
    # Remove the previous outfile if it exists
    if (file.exists(outfile)) {
      file.remove(outfile)
    } else {
      file.create(outfile)
    }
    .args$outfile = outfile
  }


  # create the cluster
  do.call(parallelly::makeClusterPSOCK, purrr::compact(.args))
}



# ----------------------- Mon Apr 08 16:49:54 2019 ------------------------#
#' @title rle_df
#'
#' Given an \code{\link[base]{rle}} this function will return a data.frame of starts, ends, and indexes thereof of the run lengths.
#' Credit: \url{https://stackoverflow.com/questions/43875716/find-start-and-end-positions-indices-of-runs-consecutive-values}
#' @param x \code{(vector)} An object for which to run an `rle`
#' @return \item{(data.frame)}{ with length, values, start and end indices.}
#' @examples
#' rle_df(sample(c(T,F), replace = TRUE, 100))
#' @export

rle_df <- function(x) {
  input_rle <- rle(x)
  .out <- unclass(input_rle)
  .out <- dplyr::mutate(tibble::as_tibble(.out),
                        end = cumsum(lengths),
                        start = c(1, dplyr::lag(end)[-1] + 1)) |>
    dplyr::select(c(1,2,4,3))
  return(.out)
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
             " - Row duplicates, row counts:\n",
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
