#' Create a formula given predictors and a label (response variable)
#'
#' @param predictors \code{chr} Column names used for prediction
#' @param label \code{chr} Column name of response variable
#'
#' @return \code{formula}
#' @export
#'
#' @examples
#' formula_make(predictors = c("max2yrlfflow", "min2yrlfflow"))
formula_make <- function(predictors, label = "response") {
  formula(paste0(label," ~ ", paste(collapse = " + ", glue::glue("`{predictors}`"))))
}
