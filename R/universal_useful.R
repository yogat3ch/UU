
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


#' @title Abbreviations of numeric magnitude
#' @export
num_chr_suffi <- c("K" = "in thousands", "M" = "in millions", "B" = "in billions", "T" = "in trillions")

#' @title Abbreviations of numeric magnitude for various units
#' @export
unit_conversion <- tibble::tribble(
  ~ unit,  ~magnitude, ~ begin, ~end, ~abbrev, ~unit_eng,
  "AF" , 10^3, "T", "in thousands", "k", "acre_feet",
  "AF" , 10^6, "M", "in millions", "M","acre_feet",
  "AF" , 10^9, "B", "in billions", "B","acre_feet",
  "F" , 10^3, NA, "in thousands", "k", "feet",
  "F" , 10^6, NA, "in millions", "M","feet",
  "F" , 10^9, NA, "in billions", "B","feet",
  "$", 10^3, NA, "in thousands", "k",  "dollars",
  "$", 10^6, NA, "in millions", "mn", "dollars",
  "$", 10^9, NA, "in billions", "bn", "dollars",
  "$" , 10^12, NA, "in trillions", "tr", "dollars",
  "W", 10^3, "k", "in thousands", "k",  "watts",
  "W", 10^6, "M", "in millions", "M", "watts",
  "W" , 10^9, "G", "in billions", "G", "watts",
  "MW", 10^3, NA, "in thousands", "k",  "megawatts",
  "MW", 10^6, NA, "in millions", "M", "megawatts",
  "MW" , 10^9,NA, "in billions", "G", "megawatts",
)


#' Compute the order of magnitude
#' @description Uses the \link[base]{floor} to round
#' @param x \code{num}
#'
#' @return \code{num}
#' @export
#'
#' @examples
#' magnitude_order(10^(1:10))
magnitude_order <- function (x) {
  floor(log10(abs(x)))
}

#' Is object an error
#'
#' @param x \code{obj}
#'
#' @return \code{lgl}
#' @export
#'
#' @examples
#' is_error(try(stop()))
is_error <- function(x) {
  inherits(x, c("error", "try-error"))
}

#' Compute the order of magnitude triplet ie thousand, million, trillion
#'
#' @param x \code{num}
#'
#' @return \code{num}
#' @export
#'
#' @examples
#' magnitude_triplet(10^(1:10))
magnitude_triplet <- function(x) {
  magnitude_order(x) %/% 3
}

unit_find_vze <- Vectorize(function(x) {
  names(unit_conversion)[purrr::map_lgl(unit_conversion, ~x %in% .x)]
})
unit_find <- function(x) {
  smode(unlist(unit_find_vze(x)))
}

#' Are most values TRUE
#' @description IF more than half the values are TRUE, returns TRUE
#' @param x \code{lgl}
#'
#' @return \code{lgl}
#' @export
#'
#' @examples
#' most(c(TRUE,TRUE,FALSE))
#' most(c(TRUE,FALSE,FALSE))
most <- function(x) {
  (sum(x, na.rm = TRUE) / length(na.omit(x))) > .5
}

#' Extract the units from a string
#' @description It is assumed that units are encased in parentheses at the end of the string
#' @param x \code{chr} String(s) to extract units from/assign units to
#'
#' @return \code{chr}
#' @export
#'
#' @examples
#' unit_string("Elevation (F)")
unit_string <- function(x) {
  stringr::str_extract(x, "(?<=\\()[^\\)]+")
}

#' @rdname unit_string
#' @export
`unit_string<-` <- function(x, value) {
  stringr::str_replace(x, "(?<=\\()[^\\)]+", value)
}

#' Modify unit abbreviations
#'
#' @param unit \code{chr} Type of units, supported values are: \code{`r glue::glue("{unique(unit_conversion$unit)}")`}
#' @param magnitude \code{num} The order of magnitude for the unit. The highest triplet will be used. See `magnitude_triplet`
#' @param outtype \code{chr} The type of output to be added. Current possibilities are: \code{`r glue::glue("{names(unit_conversion)[-c(1:2)]}")`}
#'
#' @return \code{chr} updated units
#' @export
#'
#' @examples
#' unit_modify(10^7, "AF", "abbrev")
unit_modify <- Vectorize(function(x, unit, outtype) {
  outtype <- ifelse(unit == "AF", "begin", outtype)
  out <- unit_conversion[unit_conversion$unit == unit & unit_conversion$magnitude == 10 ^ (3 * max(magnitude_triplet(x), na.rm = TRUE)), ][[outtype]]
  switch(outtype,
         begin = paste0(out, unit),
         end = paste(unit, out),
         abbrev = paste0(unit, out),
         unit_end = out)

})


#' Convert numeric value to a string abbreviation with K, M, B for Thousand, Million & Billion
#'
#' @param x \code{num}
#' @param sf \code{num} significant figures to round to
#' @param outtype \code{chr} Format of the outtype
#' \itemize{
#'   \item{\code{abbreviated}}{  takes the form `XX` where X are digits}
#'   \item{\code{with_suffix}}{ takes the form `XXS` where X are digits and S is the suffix}
#'   \item{\code{rounded}}{  takes the form `XX.XX` rounded with `sf` sig figs}
#' }
#' @return \code{chr}
#' @export
#'
#' @examples
#' num2str(10000)
num2str <- function(x, sf = 2, outtype = c("abbreviated", "with_suffix", "rounded"), suffix_lb = "K") {
  if (!is.numeric(x))
    x <- as.numeric(x)
  if (!is.numeric(x))
    gbort("{.code x} must be numeric")
  if (all(is.na(x)))
    gwarn("{.code x} is entirely NA")

  if (length(suffix_lb) != 1) {
    gbort("{.code suffix_lb} must be one of {num_chr_suffi}")
  }

  i <- max(magnitude_triplet(x), na.rm = TRUE)

  if (identical(outtype, "rounded"))
    as.character(round(x, sf))
  else if (is_legit(i) && i >= which(names(num_chr_suffi) == suffix_lb))
    paste0(round(x / 10^(3 * i), ifelse("rounded" %in% outtype, sf, 0)), ifelse("with_suffix" %in% outtype, names(num_chr_suffi)[i], ""))
}

#' @inherit num2str title params return examples
#' @export
num2str_vec <- Vectorize(num2str)


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

#' Get numeric day of the week
#'
#' @param x \code{chr} Days of the week. Abbreviations are fine, case insensitive
#'
#' @return \code{num/factor} If `x` is provided, the day of the week as integer with Monday as 1. If `x` is not provided, an ordered factor with Monday as 1.
#' @export
#'
#' @examples
#' week_factor()
#' week_factor('Tu')
week_factor <- function(x) {
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  if (missing(x))
    factor(days, days)
  else
    grep(paste0("^", x), days, perl = TRUE, ignore.case = TRUE)
}

#' Vlookup replace using a lookup column and reference table
#'
#' @param base \code{vector} of starting values. Replacements will be made in this vector before it is returned.
#' @param lookup_col \code{vector} of values with same length as `base` that will be matched to `lookup_ref` to deteremine the replacement indices
#' @param lookup_ref \code{vector} of reference values, which `lookup_col` will be matched to in order to determine replacement values.
#' @param value_col \code{vector} of replacement values with same length as `lookup_ref`
#'
#' @return \code{vector}
#' @export
#'
#' @examples
#' ref <- tibble::tibble(lookup = letters[1:5], value = 1:5)
#' original <- tibble::tibble(lookup = letters[1:20], base = runif(20, min = 6, max = 20))
#' dplyr::mutate(original, base = vlookup_from_ref(base, lookup, ref$lookup, ref$value))
vlookup_from_ref <- function(
    base,
    lookup_col,
    lookup_ref,
    value_col
) {
  col_ref_idx <- match(lookup_col, lookup_ref)
  replacements <- value_col[na.omit(col_ref_idx)]
  col_base_idx <- !is.na(col_ref_idx)
  if (any(col_base_idx))
    base[which(col_base_idx)] <- replacements
  return(base)
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
  trace = rlang::trace_back(bottom = 2),
  parent = NULL,
  e = rlang::caller_env()
) {
  rlang::abort(cli::format_error(message, .envir = e), class = class, ..., trace = trace, parent = parent)
}

#' Custom warning message
#' @description Throw \link[cli]{cli_alert_warning} with \link[cli]{format_warning}
#' @inheritParams gbort
#' @inheritParams rlang::warn
#' @export

gwarn <- function (
  message = NULL,
  body = NULL,
  footer = NULL,
  .frequency = c("always", "regularly", "once"),
  e = rlang::caller_env()
) {
  rlang::warn(cli::format_warning(message, .envir = e) , use_cli_format = TRUE)
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



is_project <- function() {
  desc <- utils::packageDescription("rstudioapi")
  if (is_legit(desc) && rstudioapi::isAvailable())
    is_legit(rstudioapi::getActiveProject())
  else
    FALSE
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
#' @return \code{(numeric)} value in `out_unit`s
#' @export
#'
#' @examples
#' size(50, "mb")
#' size(50, "gb")
#' size(50, "gb", "mb")

size <- function(x, in_unit = c("b", "Kb", "Mb", "Gb", "Tb", "Pb", "Eb", "Zb", "Yb")[1], out_unit = "b", standard = c("IEC", "SI")[1]) {
  if (!inherits(x, "object_size"))
    x <- object.size(x)

  .standard <- match_letters(standard, "IEC", "SI", ignore.case = TRUE)
  .in_unit <- match_letters(in_unit, .size$type, ignore.case = TRUE)
  .out_unit <- match_letters(out_unit, .size$type, ignore.case = TRUE)
  out_bytes <- (.size[grepl(paste0("^",.in_unit), .size$type, ignore.case = TRUE), .standard, drop = TRUE] * x)
  out <-  out_bytes / .size[grepl(paste0("^",.out_unit), .size$type, ignore.case = TRUE), .standard, drop = TRUE]
  print(out_bytes, units = .out_unit)
  return(as.numeric(out))
}




#' An alternative to \link[base]{max} that preserves names
#'
#' @param x \code{vec}
#'
#' @return \code{atomic} returns the largest element in the vector
#' @export
#'
#' @examples
#' max2(c(a = 1, b = 2))
max2 <- function(x) {
  x[which.max(x) %|0|% 1]
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
  if (is_legit(obj)) {
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

hash <- tibble::tribble(~ typ, ~ hud, ~ fun, ~ chr,
                       "integer", "I", readr::parse_integer, "i",
                       "numeric", "I", readr::parse_number, "n",
                       "character", "S", readr::parse_character, "c",
                       "logical", "S", readr::parse_logical, "l",
                       "factor", "I", readr::parse_factor, "f",
                       "Date", "D", readr::parse_date, "D",
                       "POSIXct", "T", readr::parse_datetime, "T",
                       "POSIXt", "T", readr::parse_datetime, "T",
                       "POSIXlt", "T", readr::parse_datetime, "T",
                       "list", "", readr::guess_parser, "?"
)

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
#'   \item{\code{"type"}}{ \code{(character)} The R data class}
#' }
#' @return See outtype
#' @export

col_types <- function(x, outtype = c("chr", "hud", "fun", "typ")[1]) {

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
  if (outtype == "fun")
    out <- out[[1]]
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
#' @title rle_df - create a run-length-encoding data.frame
#' @description
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


#' Break word every x characters
#'
#' @param x \code{chr} vector
#' @param every \code{num} of chars between each line break
#'
#' @return \code{chr}
#' @export
#'
#' @examples
#' str_break_every("I am a very long string that will broken into pieces", 10)
str_break_every <- function(x, every) {
  purrr::map_chr(x, ~paste0(strwrap(.x, every), collapse = "\n"))
}

#' Inequality conversion key
#' @export
inequality_key <- c(
  "less than" = "<",
  "greater than" = ">",
  "more than" = ">",
  "less than or equal to" = "<=",
  "more than or equal to" = ">=",
  "greater than or equal to" = ">=",
  "equal to" = "="
)
#' Convert inequality statements between character, mathematic, symbol and function representations
#'
#' @param x \code{chr} vector or inequality statements
#' @param outtype \code{chr} class of the outype
#' \itemize{
#'   \item{\code{"chr"}} A character
#'   \item{\code{"str"}} A character
#'   \item{\code{"sym"}} A symbol/name
#'   \item{\code{"name"}} A symbol/name
#'   \item{\code{"fun"}} A function
#' }
#'
#' @return \code{chr/name/fun} depending on the requested `outtype`
#' @export
#' @seealso inequality_key
#' @examples
#' str_inequality(">")
#' str_inequality("less than or equal to", "fun")
#' str_inequality("greater than or equal to", "sym")
str_inequality <- function(x, outtype = "chr") {

  out <- purrr::map_chr(x, ~{
    .switches <- if (.x %in% names(inequality_key))
      inequality_key
    else
      rlang::set_names(names(inequality_key), inequality_key)
      rlang::exec(switch, trimws(x),
             !!!.switches)
  })
  if (all(out %in% inequality_key) && !outtype %in% c("chr","str"))
    out <- switch(outtype,
                  name = ,
                  sym = rlang::syms(out),
                  fun = purrr::map(out, getFromNamespace, ns = "base"))

  return(out)
}


#' Change or apply filters to output type
#'
#' @param filters \code{list} of named filters to use. Each filter is named by the column name with the range as a length two numeric vector
#' @param out_type \code{obj} of desired output type
#' @param .data \code{tbl} data to use if `out_type = logical()/data.frame()`
#' @description Useful in concert with axis brushing
#' @return \code{obj} of same class as out_type
#' @export
#'
#' @examples
#' f <- list(wt = c(1,5))
#' d <- data.frame(wt = 1:10)
#' filter_to(f)
#' filter_to(f, out_type = data.frame(), .data = d)
filter_to <- function(filters, out_type = character(), .data) {
  # Determine the type of output requested tbl/lgl/chr
  is_lgl <- is.logical(out_type)
  is_df <- is.data.frame(out_type)
  df_lgl <- any(is_lgl, is_df)
  # IF there are filters
  if (UU::is_legit(unlist(filters))) {
    # Sort low to high (so dplyr::between works properly)
    filters <- purrr::map(filters, sort)
    # if character requested, return as srting
    if (is.character(out_type))
      out <- rlang::set_names(names(filters), purrr::imap(filters, ~ paste0(.y, ": ", round(.x[1], 1), " - ", round(.x[2], 1))))
    # if a tbl/lgl, create a filter expression with dplyr::between
    if (df_lgl && !missing(.data))
      .exp <- purrr::imap(filters, ~rlang::parse_expr(glue::glue("dplyr::between(`{.y}`, {.x[1]}, {.x[2]})"))) |>
        purrr::reduce(~rlang::expr(!!.x & !!.y))
    # Do the filtering and return the requested data type
    if (is_df)
      out <- dplyr::filter(.data, !!.exp)
    else if (is_lgl)
      out <- rlang::eval_tidy(.exp, data = rlang::as_data_mask(.data))
  } else if (!missing(.data) && df_lgl) {
    # handle case where there's no filters
    if (is_df)
      out <- .data
    else
      out <- rep(TRUE, nrow(.data))
  } else
    out <- filters
  return(out)
}
