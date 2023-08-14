#' Return the current package namespace
#'
#' @param pkg \code{chr} package name. Defaults to current
#' @family namespaces
#' @return \code{env}
#' @export

pkg_ns <- function(pkg = basename(rprojroot::find_package_root_file())) {
  .getNamespace(pkg)
}

#' @title Unload namespaces prior to package install
#' @param ns \code{(chr)} namespaces to unload
#' @param verbose \code{lgl}
#' @family namespaces
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
#' @family namespaces
#' @examples
#' get_global(".Last.value")
get_global <- function(global = "active") {
  get0(global, envir = .GlobalEnv)
}



#' Assign an object to the global environment
#'
#' @param x \code{obj} to assign
#' @param nm \code{chr} vector of the object and subsets to assign
#'
#' @return \code{x}
#' @family namespaces
#' @export
#'
#' @examples
#' assign_global(FALSE, "test")
assign_global <- function(x, nm = rlang::expr_deparse(rlang::enexpr(x))) {
  if (length(nm) > 1) {
    ex <- paste0("`<-`(.GlobalEnv", glue::glue_collapse(glue::glue("[['{nm}']]")), ",x)")
    ex <- rlang::parse_expr(ex)
    eval(ex)
  } else {
    .GlobalEnv[[nm]] <- x
  }
  x
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
#' @family namespaces
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
#' @param x_is_obj_name \code{lgl} If x is a character, is it the name of the object? Set to `FALSE` if x is a character object that you wish to assign _and not the name of the object_.
#' @family namespaces
#' @return \code{x} the object
#' @export

assign_in_ns <- function(x, nm = rlang::expr_deparse(rlang::enexpr(x)), ns_env = rlang::ns_env(pkg_name()), x_is_obj_name = TRUE) {

  force(nm)
  e_is_locked <- rlang::env_is_locked(ns_env)
  if (is.character(x) && x_is_obj_name) {
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

