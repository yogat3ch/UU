folder_helpers <- function(.envir = rlang::ns_env("UU")) {
  dir_folders <- purrr::compact(purrr::map(dirs, ~{
    if (dir.exists(.x()))
      rlang::new_function(rlang::pairlist2(path = .x()), body = rlang::expr(rstudioapi::filesPaneNavigate(path)))
  }))
  assign("folder", dir_folders, envir = .envir)
}

file_helpers <- function(.envir = rlang::ns_env("UU")) {
  file <- purrr::compact(purrr::map(
    .file,
    ~{
      if (file.exists(.x)) {
        rlang::new_function(
          rlang::pairlist2(path = .x),
          body = rlang::expr(rstudioapi::navigateToFile(path))
        )
      }

    }))
  assign("file", file, envir = .envir)
}
