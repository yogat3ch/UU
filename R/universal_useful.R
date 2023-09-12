
#' The length of unique values in a vector
#'
#' @param x \code{vctr}
#'
#' @return \code{dbl}
#' @export
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

#' Return a list of expressions all piped together as a single expression
#'
#' @param exprs \code{expressions} See \code{\link[rlang]{exprs}}
#' @return \code{expression}
#' @export
#'
#' @examples
#' ex <- tibble::tibble(cat = rep(letters, length.out = 6, each = 2), val = runif(6, 0, 10))
#' exps <- rlang::exprs(
#'   data,
#'   dplyr::mutate(data, val = val  + 3)
#' )
#' make_exp <- function(data, addtl_exp, summarise = TRUE) {
#'   if (summarise) {
#'     addtl_exp <- append(
#'       addtl_exp,
#'       rlang::exprs(
#'         dplyr::group_by(cat),
#'         dplyr::summarise(val = sum(val))
#'       )
#'     )
#'   }
#'   rlang::eval_bare(expr_pipe(addtl_exp))
#' }
#' make_exp(ex, exps)
#' make_exp(ex, exps, FALSE)

expr_pipe <- function(exprs) {
  rlang::parse_expr(glue::glue_collapse(purrr::reduce(exprs, \(.x, .y) {
    paste0(.x ," |>\n\t", rlang::expr_deparse(.y))
  })))
}


#' @title Abbreviations of numeric magnitude
#' @family rounding
#' @export
num_chr_suffi <- c("K" = "in thousands", "M" = "in millions", "B" = "in billions", "T" = "in trillions")

#' @title Abbreviations of numeric magnitude for various units
#' @export
unit_conversion <- tibble::tribble(
  ~ unit,  ~magnitude, ~ begin, ~end, ~abbrev, ~unit_eng,
  "AF" , 10^3, "K", "in thousands", "k", "acre_feet",
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
#' @family rounding
#' @examples
#' magnitude_order(10^(1:10))
magnitude_order <- function (x) {
  floor(log10(abs(x)))
}


#' Find the global minima/maxima of input vectors
#'
#' @param ... \code{num} vectors
#' @inheritParams plyr::round_any
#' @param fn \code{fun} \link[base]{min}/\link[base]{max}
#' @description If accuracy is omitted, number will be rounded to the nearest order of magnitude IE 145, if `fn = min`, will round to 100
#' @return \code{num}
#' @export
#' @family rounding
#' @examples
#' round_to(runif(10, 5:10), runif(2, 2:5))
#' round_to(145)
round_to <- function(..., accuracy = NULL, fn = min, f = NULL) {
  d <- do.call(c, rlang::dots_list(...))
  n <- fn(d)
  if (is.null(f)) {
    f <- if (identical(fn, min)) {
      floor
    } else {
      ceiling
    }
  }
  if (is.null(accuracy)) {
    accuracy <- 10 ^ UU::magnitude_order(n)
  }
  plyr::round_any(
    n,
    accuracy = accuracy,
    f = f
  )
}

#' Compute the order of magnitude triplet ie thousand, million, trillion
#'
#' @param x \code{num}
#'
#' @return \code{num}
#' @export
#' @family rounding
#' @examples
#' magnitude_triplet(10^(1:10))
magnitude_triplet <- function(x) {
  magnitude_order(x) %/% 3
}


unit_find_ <- Vectorize(function(x) {
  names(unit_conversion)[purrr::map_lgl(unit_conversion, ~x %in% .x)]
})
#' Find the row corresponding to a value in `unit_conversion`
#'
#' @param x \code{chr/num} vector to find
#'
#' @return \code{tbl}
#' @export
#' @family rounding
#' @examples
#' unit_find("B")
unit_find <- function(x) {
  .cols <- smode(unlist(unit_find_(x)))
  dplyr::filter(unit_conversion, !!purrr::reduce(.cols[2], \(.x, .y) {
    rlang::expr(!!.x & !!rlang::sym(.y) == !!x)
  }, .init = rlang::expr(!!rlang::sym(.cols[1]) == !!x)))
}

#' Get even numbers
#' @param x \code{int}
#' @export
#' @examples
#' evens(1:10)
#'
evens <- function(x) subset(x, x %% 2 == 0)
#' Get odd numbers
#' @param x \code{int}
#' @export
#' @examples
#' odds(1:10)
#'
odds <- function(x) subset(x, x %% 2 == 1)

#' Extract the units from a string
#' @description It is assumed that units are encased in parentheses at the end of the string
#' @param x \code{chr} String(s) to extract units from/assign units to
#'
#' @return \code{chr}
#' @export
#' @family rounding
#' @examples
#' unit_string("Elevation (F)")
unit_string <- function(x) {
  stringr::str_extract(x, "(?<=\\()[^\\)]+")
}

#' @rdname unit_string
#' @family rounding
#' @export
`unit_string<-` <- function(x, value) {
  stringr::str_replace(x, "(?<=\\()[^\\)]+", value)
}

#' Modify unit abbreviations
#' @param x \code{num} The maximum number in the dataset
#' @param unit \code{chr} Type of units, supported values are: \code{`r glue::glue("{unique(unit_conversion$unit)}")`}
#' @param outtype \code{chr} The type of output to be added. Current possibilities are: \code{`r glue::glue("{names(unit_conversion)[-c(1:2)]}")`}
#' @param magnitude \code{num} The order of magnitude for the unit. The highest triplet will be used. See `magnitude_triplet`
#' @family rounding
#' @return \code{chr} updated units
#' @export
#' @seealso unit_modify_vec
#' @examples
#' unit_modify(10^7, "AF", "abbrev")
unit_modify <- function(x, unit, outtype, magnitude = magnitude_order(x)) {
  outtype <- ifelse(unit == "AF", "begin", outtype)

  mt <- magnitude %/% 3

  out <- unit_conversion[unit_conversion$unit == unit & unit_conversion$magnitude == 10 ^ (3 * mt), ][[outtype]]
  trimws(switch(outtype,
         begin = paste0(out, unit),
         end = paste(unit, out),
         abbrev = paste0(unit, out),
         unit_eng = out))

}
#' Modify unit abbreviation, vectorized version
#' @inherit unit_modify  params return examples
#' @seealso unit_modify
#' @family rounding
#' @export
unit_modify_vec <- Vectorize(unit_modify)

#' Simple interpolate between two numbers
#'
#' @param start \code{num} first number
#' @param end \code{num} last number
#' @param n \code{int} total numbers in output vector
#'
#' @return \code{num} vector with length equivalent to n
#' @export
#'
#' @examples
#' interpolate(0, 6, 6)
interpolate = function(start, end, n) {
  out <- approx(x = c(start, end), n = n)$y
  return(out)
}

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
#' @seealso num2str_vec
#' @family rounding
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

#' @title Convert number to string Vectorized version
#' @inherit num2str params return examples
#' @seealso num2str
#' @family rounding
#' @export
num2str_vec <- Vectorize(num2str)


#' @title Statistical mode
#' @description Return the most frequenctly occuring item in a dataset
#' @family statistics
#' @param x \code{(vector)}
#' @return \code{(vector)}
#' @export

smode <- function(x) {
  .u <- unique(x)
  tab <- tabulate(match(x, .u))
  .u[tab == max(tab)]
}


#' Switch the names and the values of a vector
#'
#' @param x \code{named object}
#'
#' @return \code{obj}
#' @export
#' @family vectors
#' @examples
#' names_values_switch(c(a = 1, b = 2))
names_values_switch <- function(x) {
  rlang::set_names(names(x), x)
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
#' @family vectors
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

#' Simple lookup of values
#'
#' @param x \code{any} Values to lookup
#' @param lookup \code{named any} names will be used as replacement
#'
#' @return \code{any}
#' @export
#' @family vectors
#' @seealso [names_values_switch()]
#' @examples
#' lookup <- rlang::set_names(1:5, letters[1:5])
#' vlookup(sample(1:5, 5), lookup, switch_names_values = TRUE)
vlookup <- function(x, lookup, switch_names_values = FALSE) {
  if (switch_names_values)
    lookup <- UU::names_values_switch(lookup)
  nl <- names(lookup)
  .x <- x
  for (i in seq_along(x)) {
    .x[i] <- lookup[which(nl == x[i])]
  }
  .x
}

#' Custom error message
#' @description Throw \link[rlang]{abort} with \link[cli]{format_error}
#' @param message \code{(chr)} The message to display, formatted as a bulleted list. The first element is displayed as an alert bullet prefixed with ! by default. Elements named "*", "i", and "x" are formatted as regular, info, and cross bullets respectively. See \link[rlang]{topic-condition-formatting} for more about bulleted messaging.
#' @param class \code{(class)} Subclass of the condition
#' @param trace \code{(trace)} A `trace` object created by \link[rlang]{trace_back}
#' @param parent \code{(cond)} Supply `parent` when you rethrow an error from a condition handler
#' @param e \code{(environment)} calling environment. Passed to `glue` for making the message
#' @family condition signaling
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
#' @family condition signaling
#' @export

gwarn <- function (
  message = NULL,
  body = NULL,
  footer = NULL,
  .frequency = c("always", "regularly", "once"),
  e = rlang::caller_env()
) {
  rlang::warn(cli::format_warning(message, .envir = e) , use_cli_format = TRUE, .frequency = .frequency, .frequency_id = "UU", body = body, footer = footer)
}

#' Custom info message
#' @description Provide info with \link[rlang]{inform}
#' @inheritParams gbort
#' @inheritParams rlang::inform
#' @family condition signaling
#' @export

ginfo <- function (
  message = NULL,
  body = NULL,
  footer = NULL,
  .frequency = c("always", "regularly", "once"),
  e = rlang::caller_env()
) {
  rlang::inform(cli::format_message(message, .envir = e) , use_cli_format = TRUE, .frequency = .frequency, .frequency_id = "UU", body = body, footer = footer)
}

#' Custom message
#' Message using \link[cli]{format_message} & \link[cli]{cat_line}
#' @inheritParams cli::format_message
#' @family condition signaling
#' @export

gmsg <- function (
  msg,
  e = rlang::caller_env()
) {
  cli::cat_line(cli::format_message(msg, .envir = e))
}


#' Get a function from a package, abort if package not installed.
#'
#' @param pkg \code{chr} package
#' @param fn \code{fn} function name
#'
#' @return \code{fun}
#' @export
#' @family development
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
  type_legacy = c("b", "Kb", "Mb", "Gb", "Tb", "Pb", NA, NA, NA),
  type_IEC = c("B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"),
  type_SI = c("B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"),
  SI = c(1, 1000^(1:8))
)
#' Digital storage size conversion
#' See \link[utils]{object.size}
#' @param x \code{(numeric)}
#' @param in_unit \code{(character)} units of x
#' @param out_unit \code{(character)} units of output number **Default: Mb**. See \link[utils]{object.size} for the options.
#' @param standard \code{(character)}
#' @return \code{(numeric)} value in `out_unit`s
#' @export
#' @family rounding
#' @seealso size_
#' @examples
#' size(50, "Mb")
#' size(50, "Gb")
#' size(50, "Gb", "Mb")

size <- function(x, in_unit = "b", out_unit = c("b", "Kb", "Mb", "Gb", "Tb", "Pb", "Eb", "Zb", "Yb")[3]) {
  if (!is.numeric(x))
    x <- object.size(x)

  in_idx <- size_idx(in_unit)
  .x <- if (in_unit %nin% c("b", "B")) {
    # Get the multiplier
    in_mlt <- .size[[size_type(in_idx)]][in_idx]
    # Convert to bytes
    x * in_mlt
  } else
    x
  # Convert to out unit
  # Get the index
  out_idx <- size_idx(out_unit)
  # Get the divisor
  out_div <- .size[[size_type(out_idx)]][out_idx]
  # Get the out units
  out <- .x / out_div
  return(out)
}
size_idx <- function(unit) {
  col_out <- purrr::keep(dplyr::select(.size, tidyselect::starts_with("type")), \(.x) {any(.x %in% unit)})
  rlang::set_names(which(unit == col_out[[1]]), names(col_out))
}
size_type <- function(idx) {
  out <- stringr::str_extract(names(idx), UU::regex_or(c("IEC","SI", "legacy")))
  if (out == "SI")
    "SI"
  else
    "IEC"
}
#' Vectorized version of size
#' @export
#' @rdname size
size_ <- Vectorize(size)


#' An alternative to \link[base]{max} that preserves names
#'
#' @param x \code{vec}
#'
#' @return \code{atomic} returns the largest element in the vector
#' @export
#' @family statistics
#' @examples
#' max2(c(a = 1, b = 2))
max2 <- function(x) {
  x[which.max(x) %|0|% 1]
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

#' Rename a list
#' @description
#' From [krassowski](https://stackoverflow.com/users/6646912/krassowski) on SO [link](https://stackoverflow.com/a/73621060/2675597)
#'
#' @param .data \code{list} To be renamed
#' @param ... \code{named arguments} in the form new_name = old_name (unquoted)
#'
#' @return \code{list}
#' @export
#'
#' @examples
#' my_list = list(a=1, b=2, c=3)
#' list_rename(my_list, x=a, y=b)
list_rename <- function(.data, ...) {
  mapping = sapply(
    rlang::enquos(...),
    rlang::as_name
  )
  new_names = setNames(nm=names(.data))
  # `new_name = old_name` for consistency with `dplyr::rename`
  new_names[mapping] = names(mapping)
  # for `old_name = new_name` use: `new_names[names(mapping)] = mapping`
  setNames(.data, new_names)
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



#' @title Retrieve the function name
#' @description Sometimes a function is passed down the call stack and it's name is unknown. This function finds the name without having to pass it down the call stack as an argument.
#' @param fn \code{(function)} for which to retrieve the name
#'
#' @return \code{(character)} of the functions name
#' @export

fn_name <- function(fn) {
  trimws(stringr::str_extract(readLines(utils::getSrcFilename(fn, full.names = T))[utils::getSrcLocation(fn)], ".*(?=\\<\\-)"))

}

#' Print function formals as a list
#'
#' @param f \code{fun}
#' @param paired \code{lgl} Whether to output as paired arguments to be passed to another function.
#' @param to_console \code{lgl} whether to print \code{\link[base]{dput}} output to console. ** Default: FALSE **
#' @return \code{msg}
#' @export
#'
#' @examples
#' fml_list(base::apply)
fml_list <- function(f, paired = TRUE, to_console = FALSE) {
  fmls <- rlang::fn_fmls(f)
  out <- if (paired)
    rlang::syms(rlang::set_names(names(fmls)))
  else
    fmls
  if (to_console)
    dput(out)
  else
    capture.output((out <- dput(out)))
  out
}

#' Create a compound regex grouped statement
#'
#' @param x \code{(character)} regex strings
#' @param pre \code{(character)} regex tokens to precede each string group. IE `(?:[pre]x)` w/out the braces
#' @param suf \code{(character)} regex tokens to follow each string group. IE `(?:x[suf])` w/out the braces
#' @param type \code{(character)} `|`, `&`, `before`, `not_before` supported.
#' @return \code{(character)} grouped regex statement
#' @export

regex_op <- function(x, type = "|", prefix = "", suffix = "") {
  paste0(paste0("(?",switch(type, `|` = ":", `&` = "=.*", before = "<=", not_before = "!="),prefix, x,suffix,")"), collapse = switch(type, `before` =, `not_before` =, `|` = "|", `&` = ""))
}

#' Create a compound regex grouped OR statement
#'
#' @inheritParams regex_op
#' @return \code{(character)} grouped regex OR statement
#' @export

regex_or <- function(x, prefix = "", suffix = "", type = "|") regex_op(x, type = type, prefix = prefix, suffix = suffix)


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

#' Create an RLE Grouping from a logical vector
#'
#' @param x \code{lgl} vector
#'
#' @return \code{list}
#' @export
#'
#' @examples
#' rle_groups(sample(c(TRUE, FALSE), 20, replace = TRUE))
rle_groups <- function(x) {
  rle_df(x) |>
    dplyr::filter(values) |>
    apply(1, \(.x) {.x["start"]:.x["end"]})
}
#' Concatenate row values in a poorly scraped table
#'
#' @param .data \code{tbl} Of data with empty rows
#' @param col_to_check \code{num} The column with rows populated into which subsequent rows will be collapsed.
#'
#' @return \code{tbl}
#' @export
#'

concat_rows <- function(.data, col_to_check = 1) {


  col_to_check <- rlang::enexpr(col_to_check)
  wrapped <- zchar(.data[[col_to_check]]) | is.na(.data[[col_to_check]])
  new_tbl <- if (any(wrapped)) {
    to_concat <- rle_groups(wrapped)

    new_tbl <- .data[0, ]
    out <- list()
    for (i in seq_along(to_concat)) {
      idx <- to_concat[[i]]

      .rows <- trimws(purrr::imap_chr(.data[idx, ], \(.x, .y) {
        paste0(.x %|% '', collapse = " ")
      }))
      out[[i]] <- purrr::imap_dfc(.rows, \(.x, .y) {
        paste(.data[min(idx) - 1, .y], .x)
      })
    }
    new_tbl <- dplyr::bind_rows(out)
    new_tbl <- purrr::map_dfc(new_tbl, trimws)
  } else
    .data

  return(new_tbl)
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



unit_trans <- c(`Acre-feet` = "AF",
                Months = "mths",
                Percent = "%",
                Years = "yrs",
                Number = "#",
                `Cubic feet per second` = "Ft^3/s",
                `Million Acre-Feet` = "MAF",
                `Megawatt-hours` = "MWh",
                `Megawatts` = "MW",
                `Gigawatt-hours` = "GWh",
                `Gigawatt` = "GW",
                Feet = "Ft")


#' Easily translate long form unit names to shorthand
#' @description
#' Useful for condensed displays like axis titles
#'
#' @param x \code{chr/tbl} Character or data.frame with long form names. Currently supports `r glue::glue_collapse(paste0(names(unit_trans), " = ", unit_trans), sep =",")`
#' @param units \code{chr} Unit translation vector with shorthand as the vectur and long form names as the names. Uses `unit_trans` as default, append additional for specific translations not yet represent in `unit_trans` and open a PR or issue to add the translations if inclined.
#'
#' @return \code{chr/tbl} with same class as `x`
#' @export
#'
#' @examples
#' unit_shorthand(tibble::tibble("Max Gigawatt-hours" = 5, "Really big number" = 10^6))
unit_shorthand <- function(x, units = unit_trans) {
  UseMethod("unit_shorthand")
}

#' @export
unit_shorthand.character <- function(x, units = unit_trans) {
  out <- x
  purrr::iwalk(unit_trans, \(.x, .y) {
    out <<- stringr::str_replace_all(out, stringr::regex(.y, ignore_case = TRUE), .x)
  })
  return(out)
}

#' @export
unit_shorthand.data.frame <- function(x, units = unit_trans) {
  out <- names(x)
  purrr::iwalk(unit_trans, \(.x, .y) {
    out <<- stringr::str_replace(out, .y, .x)
  })
  rlang::set_names(x, out)
}

#' Math comparison comparator to plain english key
#' @export
comparison_key <- c(
  "less than" = "<",
  "greater than" = ">",
  "more than" = ">",
  "less than or equal to" = "<=",
  "more than or equal to" = ">=",
  "greater than or equal to" = ">=",
  "equal to" = "="
)


#' Math comparison comparator inverse key
#' @export
comparison_inverse_key <- tibble::tibble(key = comparison_key[c(1:2,4,5)], inverse = c(">=", "<=", ">", "<"))

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
#' @seealso comparison_key
#' @examples
#' str_comparison(">")
#' str_comparison("less than or equal to", "fun")
#' str_comparison("greater than or equal to", "sym")
str_comparison <- function(x, outtype = "chr") {

  out <- purrr::map_chr(x, ~{
    .switches <- if (.x %in% names(comparison_key))
      comparison_key
    else
      rlang::set_names(names(comparison_key), comparison_key)
      rlang::exec(switch, trimws(x),
             !!!.switches)
  })
  if (all(out %in% comparison_key) && !outtype %in% c("chr","str"))
    out <- switch(outtype,
                  name = ,
                  sym = rlang::syms(out),
                  fun = purrr::map(out, getFromNamespace, ns = "base"))

  return(out)
}


#' Convert a math comparator to it's inverse
#'
#' @param x \code{chr/fun} Math comparator, one of `r paste0(comparison_inverse_key$key, collapse = ', ')`
#'
#' @return \code{chr/fun} the inverse comparator as the same class as `x`
#' @export
#'
#' @examples
#' comparison_inverse(">=")
comparison_inverse <- function(x) {
  as_fun <- is.function(x)
  s <- if (as_fun)
    stringr::str_extract(rlang::expr_deparse(x), "[><=]+")
  else
    x

  inverse <- comparison_inverse_key[which(comparison_inverse_key$key == s),]$inverse
  if (as_fun)
    inverse <- getFromNamespace(inverse, "base")
  return(inverse)
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
