

.onLoad <- function (libname, pkgname) {
  .envir = rlang::ns_env("UU")
  opts_helpers(.env = .envir)
  file_helpers(.env = .envir)
  folder_helpers(.env = .envir)


}

