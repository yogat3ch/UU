#' @title Is object legit?
#' @description Is object non-null, non-empty, non-NA, and not a try-error?
#' @param x \code{(object)}
#' @return \code{(logical)}
#' @export

is_legit <- function(x) {
  !(is.null(x) || rlang::is_empty(x) || suppressWarnings(is.na(x)) || inherits(x, c("try-error", "error")))
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

#' @title Extract the file extensions from a filepath
#' @description Given a path, extract the file extension
#' @param path \code{(character)} path
#' @return \code{(character)} with the extensions
#' @export

ext <- function(path) {
  out <- stringr::str_extract(path, "(?<=\\.)\\w+$")
  if (!is_legit(out))
    stop(path, " is not a valid file path")
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


#' @title List full file paths with the file name as the name
#'
#' @param path \code{(character)} dir path to files
#'
#' @return \code{(named character)}
#' @export
#'
#' @examples
#' list.files2("~")
list.files2 <- function(path) {
  files <- list.files(path, full.names = TRUE) |>
    {\(x) {rlang::set_names(x, stringr::str_extract(basename(x), ".*(?=\\.)"))}}()
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
    grepl("csv$", ., ignore.case = TRUE) && !write ~ readr::read_csv,
    grepl("feather$", ., ignore.case = TRUE) && !write ~ feather::read_feather,
    grepl("rds$", ., ignore.case = TRUE) && !write ~ readRDS,
    grepl("csv$", ., ignore.case = TRUE) ~ readr::write_csv,
    grepl("feather$", ., ignore.case = TRUE) ~ feather::write_feather,
    grepl("rds$", ., ignore.case = TRUE) ~ saveRDs
  )

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


object_fn <- function(x) {
  purrr::when(
    x,
    inherits(., "data.frame") ~ feather::write_feather,
    inherits(., "matrix") ~ function(x, path) {
      feather::write_feather(tibble::as_tibble(x, .name_repair = "minimal"), path = path)
    },!inherits(., "data.frame") ~ saveRDS
  )
}

#' @title Provide the appropriate file read/write function
#' @description Write an object to disk
#' @param x \code{(object)} to write to disk
#' @param write_path \code{(character)} path to write the object to
#' @param mkpath \code{(logical)} Whether to create the directory for `write_path` if it doesn't exist. **Default** `FALSE`
#' @return Success message if file is written
#' @export

object_write <- function(x, write_path, mkpath = FALSE) {
  if (mkpath && !dir.exists(dirname(write_path)))
    mkpath(write_path)
  # write the file based on it's type

  fn <- object_fn(x)
  fp <- file.path(path, rlang::expr_deparse(rlang::enexpr(x)), object_ext(x))
  fn(x, fp)
  if (file.exists(fp))
    cli::cli_alert_success(paste0(fp, " saved successfully."))
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
object_ext <- function(object) {
  purrr::when(object,
              inherits(., "data.frame") ~ ".feather",
              !inherits(., "data.frame") ~ ".rds")
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
#' @param corresponding_call \code{(call)} The call where the `calling_function` is called. See \link[rlang]{trace_back} or \lnik[base]{sys.call}
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
#' @param fn \coe{(function)} for which to retrieve the name
#'
#' @return \code{(character)} of the functions name
#' @export

fn_name <- function(fn) {
  trimws(stringr::str_extract(readLines(utils::getSrcFilename(fn, full.names = T))[utils::getSrcLocation(fn)], ".*(?=\\<\\-)"))

}

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
