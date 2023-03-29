#' Is working directory a package?
#'
#' @return \code{lgl}
#' @export
#' @examples
#' is_package()

is_package <- function () {
  pkg_path() %||% FALSE
}

pkg_path <- function() {
  pkgload::pkg_path() %|try|% NULL
}

pkg_name <- function() {
  pkgload::pkg_name() %|try|% NULL
}

#' Is package in development or installed
#'
#' @param pkg_nm \code{chr} name of package
#'
#' @return \code{lgl}
#' @export
#'

is_package_dev <- function(pkg_nm = pkg_name()) {
  pkgload::is_dev_package(pkg_nm)
}

#' Get an object from a namespace
#'
#' @param nm \code{chr} name of object to retrieve. Current are `active` & `state`.
#' @param .env \code{env} from which to retrieve the object
#' \itemize{
#'   \item{\code{active}}{ tracks app details for internal use}
#'   \item{\code{state}}{ tracks user-specified variables to be saved/exported}
#' }
#'
#' @return \code{obj}
#' @export

get_from_ns <- function(nm = c("active", "state")[1], .env = .GlobalEnv) {
  .global <- get0(nm, envir = .env, inherits = FALSE)
  if (!inherits(.global, c("R6", "reactivevalues")))
    UU::gwarn("`{nm}` object was not found")
  .global
}


#' Assign a variable into a namespace
#' @description Unlocks and relocks namespaces and bindings as needed
#' @param x \code{object/chr} either the object itself or the name of the object to assign
#' @param nm \code{chr} name for object in the namespace
#' @param ns_env \code{env} of the namespace
#'
#' @return \code{x} the object
#' @export

assign_in_ns <- function(x, nm = rlang::expr_deparse(rlang::enexpr(x)), ns_env = rlang::ns_env(pkg_name())) {
  e_is_locked <- rlang::env_is_locked(ns_env)
  if (is.character(x)) {
    nm <- x
    x <- get0(x, envir = rlang::caller_env())
  }

  b_is_locked <- rlang::env_has(ns_env, nm) && rlang::env_binding_are_locked(ns_env, nm)
  if (e_is_locked)
    rlang::env_unlock(ns_env)
  if (b_is_locked)
    rlang::env_binding_unlock(ns_env, nm)
  rlang::env_bind(ns_env, !!nm := x)
  if (b_is_locked)
    rlang::env_binding_lock(ns_env, nm)
  if (e_is_locked)
    rlang::env_lock(ns_env)
  return(x)
}

