#' Return the current package namespace
#'
#' @param pkg \code{chr} package name. Defaults to current
#'
#' @return \code{env}
#' @export

pkg_ns <- function(pkg = basename(rprojroot::find_package_root_file())) {
  .getNamespace(pkg)
}

#' @title Unload namespaces prior to package install
#' @param ns \code{(chr)} namespaces to unload
#' @param verboe \code{lgl}
#' @export

unload_namespaces <- function(ns, verbose = FALSE) {
  if (missing(ns))
    ns <- loadedNamespaces()
  .ns <- ns[!ns %in% c("rstudio", "stats", "graphics", "utils", "datasets", "methods",
                       "base", "bit64", "tools")]
  purrr::walk(.ns, purrr::possibly(unloadNamespace, NA, quiet = TRUE))
  .ns <- loadedNamespaces()
  if (length(.ns) < length(ns) && verbose)
    cli::cli_alert_success("Unloaded: {cli::col_grey(paste0(ns[!ns %in% .ns], sep = ', '))}")
}

#' Get an object from the global environment
#'
#' @return \code{obj}
#' @export
#'
#' @examples
#' get_global(".Last.value")
get_global <- function(global = "active") {
  get0(global, envir = .GlobalEnv)

}
