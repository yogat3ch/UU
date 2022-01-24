rm_data <- "~/R/Contributor_Repos/COHHIO/Rm_data"
devtools::load_all(rm_data)
bodies <- UU::get_package_fns("Rm_data", pattern = "^dq_") |>
  rlang::set_names() |>
  purrr::map(~rlang::fn_body(getFromNamespace(.x, "Rm_data")))
bodies <-  rapply(bodies,  as.list, how = "replace")

remove_symbols <- function(.x) {

  keep <- c()
  for (i in seq_along(.x)) {
    if (inherits(try(.x[[i]], silent = TRUE), c("list", "call", "language", "integer", "character", "numeric")))
      keep <- c(keep, i)
  }
  if (inherits(.x, c("symbol", "name"))) {
    return(NULL)
  }

  .x[keep]
}

find_call <- function(.x, .call, arg_names, calls = list()) {

  for (i in seq_along(.x)) {
    val <- try(.x[[i]], silent = TRUE)
    if (rlang::is_call(val) && any(names(rlang::call_args(val)) %in% arg_names)) {
        calls <- append(calls, val)
    } else if (any(stringr::str_detect(rlang::expr_deparse(val) , rlang::expr_deparse(.call)))) {
      print(val)
      calls <- find_call(val, .call, arg_names, calls)
    }
  }
  calls
}


call <- purrr::map(bodies, ~find_call(.x, .call = as.call(rlang::expr(dplyr::mutate)), arg_names = c("Issue", "Type", "Guidance")))

call_args <- rapply(call, rlang::call_args, how = "replace")

call_args <- purrr::map_depth(call_args, 2, rlang::as_function(~.x[names(.x) %in% c("Issue", "Type", "Guidance")]))


call_args <- purrr::map_depth(call_args, 3, ~{
  if (!is.character(.x))
    .x <- paste0(rlang::expr_deparse(.x), collapse = " ")
  .x
})


purrr::map_depth(call_args, 2, ~dplyr::bind_rows(purrr::compact(.x))) |>
  purrr::map_depth(1, ~dplyr::bind_rows(purrr::compact(.x))) |>
  dplyr::bind_rows(.id = "DQ_Check") |>
  readr::write_csv(file.path(rm_data, "inst", "src", "DataQualityChecks.csv"))

# writing guidance file ----
# Tue Oct 26 14:11:40 2021
source(file.path(rm_data, "R/04_Guidance.R"))
call_args <- rapply(call_args, class = "call", function(x) {
  if (stringr::str_detect(rlang::expr_deparse(x), "^guidance\\$"))
    out <- eval(x)
  else if (!is.character(x))
    out <- paste0(rlang::expr_deparse(x), collapse = "\n")
  else
    out <- x
  browser(expr = is.null(out))
  out
}, how = "replace")
dput(call_args, file = "R/guidance.R")

new_guidance <- source("r/guidance.R")$value
