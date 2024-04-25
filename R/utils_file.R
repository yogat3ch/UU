folder_helpers <- function(.env = rlang::ns_env("UU")) {
  if (is.null(get0("folder", envir = .env, inherits = FALSE))) {
    dir_folders <- purrr::compact(purrr::map(dirs, ~{
      if (dir.exists(.x()))
        rlang::new_function(rlang::pairlist2(path = .x()), body = rlang::expr(rstudioapi::filesPaneNavigate(fs::path_abs(path))))
    }))
    assign("folder", dir_folders, envir = .env)
  }

}

file_helpers <- function(.env = rlang::ns_env("UU")) {
  if (is.null(get0("file", envir = .env, inherits = FALSE))) {
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
    if (dir.exists("R"))
      file$R <- purrr::map(list.files2("R"), ~rlang::new_function(
        rlang::pairlist2(path = .x),
        body = rlang::expr(rstudioapi::navigateToFile(path))
      ))
    assign("file", file, envir = .env)
  }
}
