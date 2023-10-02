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
assign_global <- function(x, nm = rlang::expr_deparse(rlang::enexpr(x)), env = .GlobalEnv) {
  if (length(nm) > 1) {
    ex <- paste0("`<-`(.GlobalEnv", glue::glue_collapse(glue::glue("[['{nm}']]")), ",x)")
    ex <- rlang::parse_expr(ex)
    eval(ex)
  } else {
    env[[nm]] <- x
  }
  x
}




#' Create a function that creates an object and assigns it to a namespace the first time it's called and subsequently retrieves it from the namespace thereafter.
#' @description
#' Useful when the object depends on a long running task such as a database query.
#'
#' @param obj_nm \code{chr} Name of the object
#' @param ns_chr \code{expr/chr} name of the namespace to assign the object to, or an expression that returns the environment to assign to
#' @param call_expr \code{expr} The code used to construct the object if the object hasn't already been constructed
#' @param as_character Should the function return code as a character? (Default is an expression)
#' @return \code{chr} The function at the console for copy/paste
#' @export
#'
#' @examples
#' create_simple_get_function("mt_cars", .GlobalEnv, dplyr::mutate(mtcars, cyl = as.character(cyl)))

create_simple_get_function <- function(obj_nm, env_expr, call_expr, as_character = FALSE) {
  exp <- rlang::enexpr(env_expr)
  if (rlang::is_character(env_expr))
    exp <- rlang::expr(rlang::ns_env(!!env_expr))
  get_fn <- rlang::new_function(
    rlang::pairlist2(env = rlang::expr(!!exp)),
    rlang::expr({
      if (exists(!!obj_nm, envir = env)) {
        get0(!!obj_nm, envir = env)
      } else {
        !!rlang::enexpr(call_expr)
      }

    })
  )
  code <- rlang::call2(`<-`,rlang::expr(!!rlang::sym(paste0("get_", obj_nm))), rlang::expr(!!get_fn))
  code_chr <- deparse(code)
  cat(code_chr, sep = "\n")
  return(if (as_character) code_chr else code)
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
  if (!rlang::is_environment(.env))
    .env <- rlang::ns_env(.env)
  .global <- get0(nm, envir = .env, inherits = FALSE)
  if (!inherits(.global, c("R6", "reactivevalues")))
    UU::gwarn("`{nm}` object was not found")
  .global
}


#' Assign a variable into a namespace
#' @description Unlocks and relocks namespaces and bindings as needed
#' @param x \code{object/chr} either the object itself or the name of the object to assign
#' @param nm \code{chr} name for object in the namespace. IF `x` is not supplied, this object will be retrieved by name from the calling environment via \code{\link[base]{get0}}
#' @param ns_env \code{env} of the namespace
#' @family namespaces
#' @return \code{x} the object
#' @export
#' @examples
#' (function(x = mtcars) {
#' y <- dplyr::mutate(x, cylinders = cyl)
#'   assign_in_ns(nm = "y", ns_env = "UU")
#' })()
#' uu <- rlang::ns_env("UU")
#' exists("y", uu)
#' assign_in_ns(mtcars, ns_env = "UU")
#' exists("mtcars", uu)
#' assign_in_ns(mtcars, "cars_copy", ns_env = "UU")
#' exists("cars_copy", uu)
#' rlang::env_unlock(uu)
#' rm(list = c("y", "mtcars", "cars_copy"), envir = uu)
#' rlang::env_lock(uu)

assign_in_ns <- function(x, nm = rlang::expr_deparse(rlang::enexpr(x)), ns_env = rlang::ns_env(pkg_name())) {

  force(nm)
  if (!rlang::is_environment(ns_env))
    ns_env <- rlang::ns_env(ns_env)
  e_is_locked <- rlang::env_is_locked(ns_env)
  if (missing(x)) {
    x <- get0(nm, envir = rlang::caller_env())
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

