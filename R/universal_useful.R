#' @inherit plyr::match_df title params description
#' @param out \code{obj} Of class matching the desired output. **Default** `NULL` returns a `data.frame` with the row(s) of `x` that have matches in `y`. `numeric()` will return the matching indices of `x` with matches in `y` & `logical()` will return a matching logical vector with length equivalent to `x` of the rows matching in `y`
#' @param on \code{chr} Either a vector of the names on which to match, if they are named similarly in x & y, or a specification in the form `c(y_feature = x_feature)`
#' @seealso plyr::match_df
#' @return \code{tbl/dbl/lgl} Depending on the class of `out`
#' @export
match_df <- function(x, y, out = NULL, on = NULL, verbose = FALSE) {
  fn <- purrr::map
  vars_differ <- FALSE
  if (is.null(on)) {
    on <- intersect(names(x), names(y))
    if (rlang::is_empty(on))
      gbort("{.code x} and {.code y} have no common features, please specify features to be matched ")


    if (verbose)
      message("Matching on: ", paste(on, collapse = ", "))
  } else if (!rlang::is_empty(names(on))) {
    fn <- purrr::imap
    vars_differ <- TRUE
  }

  # Map over all the features in `on`
  keys <- fn(on, \(.x, .y) {
    if (!vars_differ) {
      .y <- .x
    }
    v <- x[[.x]]
    # Return the indices which intersect between the two
    which(v %in% intersect(v, y[[.y]]))
  }) |>
    # Reduce across all the features to only those in common
    purrr::reduce(intersect)
  if (rlang::is_empty(keys))
    gwarn("No common keys between {.code x} and {.code y} on feature{?s} {on}")

  key_out(x, keys, out)
}

#' Handle different output type requests for `match_df`
#' @export
key_out <- function(x, keys, out) {
  UseMethod("key_out", out)
}
#' @export
key_out.default <- function(x, keys, out) {
  x[keys, , drop = FALSE]
}
#' @export
key_out.numeric <- function(x, keys, out) {
  keys
}
#' @export
key_out.logical <- function(x, keys, out) {
  out <- rep(FALSE, nrow(x))
  out[keys] <- TRUE
  out
}

#' Return a list of expressions all piped together as a single expression
#' @description
#' Useful when making complex compound statements that require dynamic substitution via tidy eval for dynamically created variables derived from the context.
#'
#' @param exprs \code{expressions} See \code{\link[rlang]{exprs}}
#' @return \code{expression}
#' @export
#'
#' @examples
#'(.data <- tibble::tibble(val = runif(10)))
#' (exp <- expr_pipe(
#'   rlang::exprs(
#'     .data,
#'     dplyr::mutate(val = val + 5, category = sample(1:3, length(val), replace = TRUE)),
#'     dplyr::group_by(category),
#'     dplyr::summarise(s = sum(val))
#'   )
#' ))
#' rlang::eval_bare(exp)

expr_pipe <- function(exprs) {
  stopifnot("`exprs` must be a list." = is.list(exprs))
  if (length(exprs) < 2)
    gbort("`exprs` should have more tan 1 element for a pipe to take effect.")
  if (!is.name(exprs[[1]]))
    gwarn("The first element of `exprs` should be of class 'name'.")

  with_pipes <- purrr::reduce(exprs, \(.x, .y) {
    paste0(.x ," |>\n\t", glue::glue_collapse(rlang::expr_deparse(.y)))
  })
  rlang::parse_expr(glue::glue_collapse(with_pipes))
}

#' @title Find by class
#' @description Find an object by it's class
#'
#' @param class The \code{(class)} class to search
#' @param e The \code{(environment)} to search
#'
#' @return the first object assigned to the environment that matches the class. If more than one object of the class are found, it triggers a warning.
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
    return(NULL)
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

#' @title Find the common names between two objects
#' @description Given named objects, find the names in common
#' @param ... \code{(objects)}
#' @return \code{(character)} of the common names
#' @export
#' @examples
#' common_names(rlang::set_names(letters), rlang::set_names(letters[4:10]))

common_names <- function(...) {
  .d <- rlang::dots_list(..., .named = TRUE)
  nms <- purrr::map(.d, names)
  no_names <- purrr::map_lgl(nms, is.null)
  if (any(no_names))
    gwarn("Argument{?s} {names(.d)[no_names]} do{?esn't/n't} have names.")
  x <- table(do.call(c, nms))
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


#' @title Translate DT Column names to numeric indices using regex matching
#'
#' @param x \code{chr/num} Column numbers or names
#'
#' @return \code{num} Numeric representation of column
#' @examples
#' names(mtcars)
#' which_cols("c", mtcars)
#'
#' @export

which_cols <- function(x, .data) {
  UseMethod("which_cols")
}

#' @export
which_cols.numeric <- function(x, .data)
  x

#' @export
which_cols.character <- function(x, .data) {
  dplyr::matches(UU::regex_or(x), vars = names(.data))
}

#' Match the classes of one object to that of another object
#'
#' @param x \code{(object)} object to be matched
#' @param y \code{(object)} object to be coerced
#'
#' @return \code{y} with class types matching that of \code{x}
#' @export

map_class <- function(x, y) {
    purrr::map2(purrr::map(y, class), x, \(.x, .y) class_coercion_fn(.x)(.y))
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
#' @param fn \code{fn} for which to retrieve the name
#'
#' @return \code{chr} of the functions name
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

#' Alphabetize the _.gitignore_ file
#'
#' @param gitignore \code{chr} path to gitignore
#'
#' @return \code{None} overwrites the file with entries in alphabetical order
#' @export
#'
gitignore_alphabetize <- function(gitignore = ".gitignore") {
  if (file.exists(gitignore))
    zchar_remove(sort(readLines(gitignore))) |>
      writeLines(gitignore)
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

#' Create a formula given predictors and a label (response variable)
#'
#' @param predictors \code{chr} Column names used for prediction
#' @param label \code{chr} Column name of response variable
#'
#' @return \code{formula}
#' @export
#'
#' @examples
#' formula_make(predictors = c("max2yrlfflow", "min2yrlfflow"))
formula_make <- function(predictors, label = "response") {
  formula(paste0(label," ~ ", paste(collapse = " + ", glue::glue("`{predictors}`"))))
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
