#' Get numeric day of the week
#'
#' @param x \code{chr} Days of the week. Abbreviations are fine, case insensitive
#'
#' @return \code{factor} If `x` is provided, the day of the week as a factor. If `x` is not provided, an ordered factor with Monday as 1.
#' @export
#'
#' @examples
#' week_factor()
#' week_factor('Tu')
week_factor <- function(x) {
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  if (missing(x))
    out <- days
  else
    out <- purrr::map_chr(x, ~stringr::str_subset(days, stringr::regex(paste0("^", .x), ignore_case = TRUE)))
  return(factor(out, days))
}

#' Translate a duration into the human-legible estimation as a character
#'
#' @param duration \code{Duration}
#'
#' @return \code{chr}
#' @export
#'
#' @examples
#' duration_print(lubridate::period(6223))
duration_print <- function(duration) {
  stringr::str_extract(as.character(duration), "(?<=\\()[^\\)]+")
}

#' Convert Excel character date representation to a Date
#'
#' @param .x \code{chr} representation of Dates from Excel, usually a 5 digit number
#'
#' @return \code{Date}
#' @export
#'
#' @examples
#' excel_date(c("44567", "44568"))
excel_date <- function(.x) {
  base::as.Date(as.integer(.x), origin = as.Date("1899-12-30"))
}
