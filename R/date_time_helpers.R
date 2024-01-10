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
    l_x <- len_unique(x_n) == 1
    if (any(x_n < 2) || !l_x)
      gbort("All abbreviations must be 2 characters or more to disambiguate")

    # If using an abbreviation
    if (l_x) {
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
      out <- factor(out, levels = if (label) s_chr else seasons, ordered = TRUE)
    }
  }

  return(out)
}

#' Month as factor/numeric
#'
#' @param x \code{chr/Date/POSIXt} Names of months as case insensitive character vector, if using abbreviations, they must be of consistent length. Otherwise, a `Date` or `POSIXt`.
#'
#' @return \code{factor} If `x` is provided, the month as a factor. If `x` is not provided, an ordered factor of the months
#' @export
#' @family time
#' @examples
#' month_factor()
#' month_factor(c('Se', 'No'))
#' month_factor(c('Se', 'No'), label = TRUE)
#' month_factor(seq(lubridate::floor_date(Sys.time(), "year"), Sys.time(), by = "day"), label = TRUE, abbr = FALSE)
month_factor <- function(x, label = FALSE, abbr = TRUE) {
  months <- if (abbr) {
    month.abb
  } else {
    month.name
  }

  m_num <- 1:12

  if (missing(x))
    return(factor(months, months))

  if (is.character(x)) {
    out <- x
    x_n <- nchar(x)
    x_u <- unique(x_n)
    l_x <- len_unique(x_n) == 1
    if (any(x_n < 2) || !l_x)
      gbort("All abbreviations must be 2 characters or more to disambiguate")

    # If using an abbreviation
    m_chr <- if (l_x) {
      substr(months, 0, x_u)
    } else {
      months
    }



    val <- if (label)
      months
    else {
      out <- vector(mode = "integer", length = length(out))
      as.integer(1:12)
    }

    for (m in m_chr) {
      lgl <- grep(m, x, ignore.case = TRUE)
      if (!rlang::is_empty(lgl))
        out[lgl] <- val[m_chr == m]
    }

    if (label)
      out <- factor(out, levels = months, ordered = TRUE)

  } else if (lubridate::is.Date(x) || lubridate::is.POSIXt(x)) {
    out <- lubridate::month(x)
    if (label) {
      for (m in unique(out)) {
        lgl <- out == m
        if (!rlang::is_empty(lgl))
          out[lgl] <- months[m_num == m]
      }
    }
    out <- factor(out, levels = months, ordered = TRUE)
  } else if (is.numeric(x)) {
    out <- x
    if (label) {
      for (i in unique(out)) {
        lgl <- out == i
        out[lgl] <- months[i]
      }
    }
    out <- factor(out, levels = months, ordered = TRUE)
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
  lubridate::as.duration(difftime(r[2], r[1]))
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

#' Make a file path compliant ISO8601 timestamp
#'
#' @param x \code{Datetime}
#' @inheritParams lubridate::with_tz
#'
#' @return \code{chr} of timestamp
#' @export
#'
#' @examples
#' fs::path(paste0("file_", file_timestamp()), ext = "zip")
file_timestamp <- function(x = Sys.time(), tzone = "") {
  stringr::str_replace_all(lubridate::format_ISO8601(lubridate::with_tz(x, tzone = tzone)), '\\:', '\\.')
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



#' Create a tbl with each day of the year, it's week number, and the interval containing that week.
#'
#' @param year \code{int} Year number
#' @param inclusive \code{lgl} Whether to include any days from the previous and next years in the first and final weeks of the year respectively.
#'
#' @return \code{tbl} With features:
#' \itemize{
#'   \item{\code{day}}{ The Date of each day}
#'   \item{\code{wn}}{ week number. See \code{\link[lubridate]{isoweek}} for details.}
#'   \item{\code{interval}}{ The \code{\link[lubridate]{interval}} for the week.}
#' }
#' @export

week_bins_per_year <- function(year, inclusive = TRUE) {
  week_table <- tibble::tibble(day = seq(lubridate::make_date(year), lubridate::make_date(year, 12, 31), by = "day")) |>
    dplyr::mutate(wn = lubridate::isoweek(day)) |>
    dplyr::group_by(wn) |>
    dplyr::mutate(interval = lubridate::interval(min(day), max(day)))
  if (inclusive) {
    # Include weeks from the previous year in the first week of the year
    week_1_end <- dplyr::filter(head(week_table, 7), wn == 1) |>
      dplyr::pull(day) |>
      dplyr::last()
    week_table[1:which(week_table$day == week_1_end), "wn"] <- 1
    if (any(tail(week_table, 7)$wn == 1)) {
      all_1s <- which(week_table$wn == 1)
      # Indexes for the days which fall in the first week of the following year
      idx <- all_1s[(which(diff(all_1s) > 1) + 1):length(all_1s)]
      week_table[idx, "wn"] <- max(week_table$wn)
    }
  }
  return(week_table)

}

#' Partition a meeting into evenly distributed sections based on how much time is left between when intros end and the end of the meeting
#'
#' @param start_hms \code{chr} Meeting start time in 24-hour H:M:S Time format. This is HMS since it's easier to supply this argument in advance of the meeting.
#' @param intros_end \code{Datetime} If you know when intros should end in advance, supply it, otherwise this function is intended to to be run when they end for a dynamic calculation of sections
#' @param t4q \code{Duration} how long to save for questions/closing remarks at the end
#' @param m_length \code{Duration} total length of the meeting
#' @param sections \code{num} Number of sections to distribute time
#'
#' @return \code{list} With:
#' \itemize{
#'   \item{\code{each_section}}{ time allotted to each section}
#'   \item{\code{Section X}}{ The end time for each section}
#'   \item{\code{end}}{ The end time}
#' }
#' @export
#'
#' @examples
#' # If the meeting started 10 minutes ago and the intros just ended
#' meeting_start_time <- (lubridate::now() - lubridate::minutes(10)) |> as.character() |> stringr::str_extract("\\d{2}:\\d{2}:\\d{2}")
#' meeting_timer(meeting_start_time)
meeting_timer <- function(start_hms = "17:40:00", t4q = lubridate::minutes(10), m_length = lubridate::minutes(75), sections = 4, intros_end = lubridate::now()) {

  intros_end <- lubridate::now()
  start_time <- lubridate::make_datetime(year = lubridate::year(intros_end), month = lubridate::month(intros_end), day = lubridate::day(intros_end), tz = "America/New_York") + lubridate::hms(start_hms)
  end_time <- start_time + m_length
  questions <- end_time - t4q
  each_section = (questions - intros_end) / sections
  rlang::list2(
    each_section = each_section,
    !!!purrr::map(setNames(1:sections, paste0("Section ", 1:sections)), \(.x) {
      intros_end + (each_section) * .x
    }),
    end = end_time
  )
}




