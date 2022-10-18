#' Load project & user-level _.Renviron_ & _.Rprofile_
#' @export
startup <- function() {
  if (!getOption("UU_startup", FALSE)) {
    options(UU_startup = TRUE)
    list(
      .Renviron_user = Sys.getenv("R_ENVIRON_USER", "~/.Renviron"),
      .Renviron = Sys.getenv("R_ENVIRON", ".Renviron"),
      .Rprofile_user = Sys.getenv("R_PROFILE_USER", "~/.Rprofile"),
      .Rprofile = Sys.getenv("R_PROFILE" , ".Rprofile")
    ) |>
      purrr::iwalk(~{
        if (file.exists(.x)) {
          gmsg("{.path {.x}} loaded.")
          rlang::exec(switch(.y,
                             .Rprofile = ,
                             .Rprofile_user = base::source,
                             .Renviron = ,
                             .Renviron_user = base::readRenviron), .x)
        }

      })
  }
  on.exit(unload_namespaces(c("UU", "purrr", "rlang")))
}
