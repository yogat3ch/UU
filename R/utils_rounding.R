

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


#' Find convenient limits of input vectors
#'
#' @param ... \code{num} vectors
#' @inheritParams plyr::round_any
#' @inheritParams base::max
#' @param fn \code{fun} \link[base]{min}/\link[base]{max}
#' @description If accuracy is omitted, number will be rounded to the nearest order of magnitude IE 145, if `fn = min`, will round to 100
#' @return \code{num}
#' @export
#' @family rounding
#' @examples
#' round_to(runif(10, 5, 10), sample(1:10, 5))
#' round_to(runif(10, 5, 10), sample(1:10, 5), fn = max)
#' round_to(45)
#' round_to(145)
#' round_to(145, fn = max)
round_to <-
  function(...,
           accuracy = NULL,
           fn = min,
           f = if (identical(fn, min)) {
             floor
           } else {
             ceiling
           },
           na.rm = TRUE) {
    d <- do.call(c, rlang::dots_list(...))
    .n <- fn(d, na.rm = na.rm)

    if (is.null(accuracy)) {
      accuracy <- 10 ^ UU::magnitude_order(.n)
    }
    plyr::round_any(.n,
                    accuracy = accuracy,
                    f = f)
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

