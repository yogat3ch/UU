#' Load project & user-level _.Renviron_ & _.Rprofile_
#' @export
startup <- function() {
  if (!getOption("UU_startup", FALSE)) {
    profiles <-
      list(
        .Rprofile_user = Sys.getenv("R_PROFILE_USER", "~/.Rprofile"),
        .Rprofile = Sys.getenv("R_PROFILE" , ".Rprofile")
      )
    profiles_to_load <- purrr::map_lgl(profiles, file.exists)
    profiles_to_load <- profiles[profiles_to_load]
    profiles_chr <- purrr::map(rlang::set_names(profiles_to_load, unlist(profiles_to_load)), readLines)
    startup_lines <- purrr::map(profiles_chr, \(.x) {
      .x[!stringr::str_detect(.x, "UU::startup")]
    })

    purrr::iwalk(startup_lines, \(.x, .y) {
      eval(parse(text = .x), envir = .GlobalEnv)
      gmsg("{.path { .y}} loaded.")
    })

    list(
      .Renviron_user = Sys.getenv("R_ENVIRON_USER", "~/.Renviron"),
      .Renviron = Sys.getenv("R_ENVIRON", ".Renviron")
    ) |>
      purrr::iwalk(\(.x, .y){
        if (file.exists(.x)) {
          base::readRenviron(.x)
          gmsg("{.path { .x}} loaded.")
        }
      })
    options(UU_startup = TRUE)
  }


  on.exit({
    unloadNamespace("UU")
    pkgs_to_unload <- stringr::str_split(utils::packageDescription("UU")$Imports, ",")[[1]] |>
      stringr::str_trim() |>
      stringr::str_extract("^[:alnum:]+") |>
      base::setdiff(utils::installed.packages(priority = c("base", "recommended"))[,"Package"])
    sapply(pkgs_to_unload, \(.x) try(utils::unloadNamespace(.x), silent = TRUE))
  })
}
