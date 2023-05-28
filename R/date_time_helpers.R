#' Day of the week as factor/numeric
#'
#' @param x \code{chr/Date/POSIXt} Days of the week as case insensitive character vector, if using abbreviations, they must be of consistent length. Otherwise, a `Date` or `POSIXt`
#'
#' @return \code{numeric/factor} Depending on the value for `label`. If `x` is provided, the day of the week as a factor. If `x` is not provided, an ordered factor with Monday as 1.
#' @export
#' @family time
#' @examples
#' week_factor()
#' week_factor(c('Tu', 'We'))
#' week_factor(c('Tu', 'We'), label = TRUE)
#' week_factor(seq(lubridate::floor_date(Sys.time(), "year"), Sys.time(), by = "day"), label = TRUE, abbr = FALSE)
week_factor <- function(x, label = FALSE, abbr = TRUE, week_start = getOption("lubridate.week.start", 7)) {
  d_nms <- days <- rlang::set_names(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))[
    if (week_start == 7)
      c(7, 1:6)
    else if (week_start == 1)
      1:7
  ]

  if (abbr)
    days <- substr(days, 0, 3)

  if (missing(x))
    return(factor(days, days, ordered = TRUE))

  if (is.character(x)) {
    out <- x
    x_n <- nchar(x)
    x_u <- unique(x_n)
    if (any(x_n < 2))
      gbort("All abbreviations must be 2 characters or more to disambiguate")
    stopifnot(len_unique(x_n) == 1 && all(x %nin% days))
    # If using an abbreviation
    if (len_unique(x_n) == 1)
      days <- substr(days, 0, x_u)

    for (d in days) {
      lgl <- grep(d, x, ignore.case = TRUE)
      if (!rlang::is_empty(lgl))
        out[lgl] <- d_nms[days == d]
    }

    out <- factor(out, levels = d_nms, ordered = TRUE)
    if (!label)
      out <- as.integer(out)
  } else if (lubridate::is.Date(x) || lubridate::is.POSIXt(x)) {
    out <- lubridate::wday(x, label = label, abbr = abbr, week_start = week_start)
  }

  return(out)
}

#' Season as factor/numeric
#'
#' @param x \code{chr/Date/POSIXt} Names of seasons as case insensitive character vector, if using abbreviations, they must be of consistent length. Otherwise, a `Date` or `POSIXt`.
#'
#' @return \code{factor} If `x` is provided, the season as a factor. If `x` is not provided, an ordered factor of the seasons.
#' @export
#' @family time
#' @examples
#' season_factor()
#' season_factor(c('Sp', 'Su'))
#' season_factor(c('Sp', 'Su'), label = TRUE)
#' season_factor(seq(lubridate::floor_date(Sys.time(), "year"), Sys.time(), by = "day"), label = TRUE, abbr = FALSE)
season_factor <- function(x, label = FALSE, abbr = TRUE) {
  seasons <- c("Spring" = 3, "Summer" = 6, "Fall" = 9, "Winter" = 12)
  s_chr <- names(seasons)
  if (abbr)
    s_chr <- substr(s_chr, 0, 3)
  if (missing(x))
    return(factor(s_chr, s_chr))

  if (is.character(x)) {
    out <- x
    x_n <- nchar(x)
    x_u <- unique(x_n)
    if (any(x_n < 2))
      gbort("All abbreviations must be 2 characters or more to disambiguate")
    stopifnot(len_unique(x_n) == 1 && all(x %nin% days))
    # If using an abbreviation
    if (len_unique(x_n) == 1) {
      seasons <- substr(s_chr, 0, x_u)
    }


    val <- if (label)
      s_chr
    else {
      out <- vector(mode = "integer", length = length(out))
      as.integer(c(3,6,9,12))
    }

    for (se in seasons) {
      lgl <- grep(se, x, ignore.case = TRUE)
      if (!rlang::is_empty(lgl))
        out[lgl] <- val[seasons == se]
    }

    if (label)
      out <- factor(out, levels = s_chr, ordered = TRUE)

  } else if (lubridate::is.Date(x) || lubridate::is.POSIXt(x)) {
    out <- lubridate::month(lubridate::floor_date(x, "season"))
    if (label) {
      for (m in unique(out)) {
        lgl <- out == m
        if (!rlang::is_empty(lgl))
          out[lgl] <- s_chr[seasons == m]
      }
      out <- factor(out, levels = if (label) s_chr else seasons)
    }
  }

  return(out)
}

#' Translate a duration into the human-legible estimation as a character
#'
#' @param duration \code{Duration}
#'
#' @return \code{chr}
#' @export
#' @family time
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
#' @family time
#' @examples
#' excel_date(c("44567", "44568"))
excel_date <- function(.x) {
  base::as.Date(as.integer(.x), origin = as.Date("1899-12-30"))
}


#' @title Timespans as character
#' @family time
#' @export
time_aggregates <-
  list(
    Year = "year",
    Season = "season",
    Quarter = "quarter",
    Month = "month",
    Week = "week",
    Day = "day",
    Hour = "hour"
  )

#' @title Timespans as durations
#' @family time
#' @export
time_difftimes <- purrr::map(time_aggregates, \(.x) {
  x <- switch(.x,
              season = ,
              quarter = 3,
              1)
  unit <- switch(.x,
                 season = ,
                 quarter = "months",
                 paste0(.x,"s"))
  lubridate::duration(x, units = unit)
}) |> rlang::set_names(time_aggregates)

#' @title Timespans as factor
.time_factor <- unlist(time_aggregates) |>
  {\(.x) {factor(.x, levels = rev(.x), ordered = TRUE)}}()

#' Turn timespans into an ordered factor
#'
#' @param x \code{chr} of timespans
#' @family time
#' @return \code{factor}
#' @export
#'
#' @examples
#' time_factor(c("year", "week"))
time_factor <- function(x) {
  if (missing(x))
    .time_factor
  else
    factor(x, levels = levels(.time_factor), ordered = TRUE)

}
#' Create a timespan duration
#' @family time
#' @param x \code{chr/POSIXt/Date}
#' \itemize{
#'   \item{\code{chr}}{ One of `r paste0(time_aggregates, collapse = ', ')`}
#'   \item{\code{POSIXt/Date/Datetime}}{ A Date or Datetime vector}
#' }
#'
#' @return \code{Duration} See \code{\link[lubridate]{duration}}
#' @export
#'
#' @examples
#' timespan(seq(Sys.time() - lubridate::dyears(), Sys.time(), by = 1))
#' timespan("season")
timespan <- function(x) {
  UseMethod("timespan")
}
#' @export
timespan.POSIXct <- function(x) {
  r <- range(x)
  lubridate::as.duration(difftime(r[1], r[2]))
}
#' @export
timespan.POSIXlt <- timespan.POSIXct
#' @export
timespan.Date <- timespan.POSIXct
#' @export
timespan.character <- function(x) {
  stopifnot(x %in% names(time_difftimes))
  time_difftimes[[grep(x, unlist(time_aggregates), ignore.case = TRUE)]]
}

#' Return a logical on an interval
#'
#' @param file \code{chr} filename in which to store the interval time
#' @param interval \code{period/duration} Default 1 week
#'
#' @return \code{lgl}
#' @export
#' @family time

time_elapsed <- function(file = ".interval_timer.rds", interval = lubridate::weeks(1)) {
  if (file.exists(".gitignore"))
    usethis::use_git_ignore(".interval_timer.rds")
  if (file.exists(file)) {
    prev_time <- readRDS(file)
    first_run <- FALSE
  } else {
    prev_time <- Sys.time()
    saveRDS(prev_time, file)
    first_run <- TRUE
  }


  if (Sys.time() > prev_time + interval || first_run) {
    saveRDS(Sys.time(), file)
    TRUE
  } else
    FALSE
}
