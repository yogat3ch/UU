

.onLoad <- function (libname, pkgname) {
  .envir = rlang::ns_env("UU")
  browser()
  opts_helpers(.env = .envir)
  dir_folders <- purrr::compact(purrr::map(dirs, ~{
    if (dir.exists(.x()))
      rlang::new_function(rlang::pairlist2(path = .x()), body = rlang::expr(rstudioapi::filesPaneNavigate(path)))
  }))
  assign("folder", dir_folders, envir = .envir)

  purrr::compact(purrr::map(
    .file,
    ~{
      if (file.exists(.x))
        rlang::new_function(
          rlang::pairlist2(path = .x),
          body = rlang::expr(rstudioapi::navigateToFile(path))
        )
    }))
}
