#' Generate xpath to find sibling nodes between two elements
#' The function produces a compounding xpath with each subsequent argument provided. Thus the final argument specified will be the node that is selected by the resulting xpath with the exception of `nested_tag_contains` which helps to identify a nested tag by its contents
#' @param start_tag \code{chr} xpath of the upper bounding element
#' @param following_sibling \code{chr} relative xpath of the lower bounding element
#' @param preceding_sibling \code{chr} relative xpath for the type of elements in between the upper and lower bounding elements to retrieve
#' @param nested_tag \code{chr} relative xpath of an element nested within the `preceding_sibling` elements
#' @param nested_tag_contains \code{chr} of text that the nested_tag element contains (in order to identify it. The nested tag element itself will be selected).
#'
#' @return \code{chr} xpath statement
#' @export

xpath_sibling_between <- function(start_tag, following_sibling, preceding_sibling, nested_tag, nested_tag_contains) {
  out <- glue::glue("//{start_tag}")
  if (!missing(following_sibling))
    out <- paste0(out, glue::glue("/following-sibling::{following_sibling}"))
  if (!missing(preceding_sibling))
    out <- paste0(out, glue::glue("/preceding-sibling::{preceding_sibling}"))
  if (!missing(nested_tag))
    out <- paste0(out, glue::glue("/{nested_tag}"))
  if (!missing(nested_tag_contains))
    out <- paste0(out, glue::glue("[contains(text(), '{nested_tag_contains}')]/parent::{nested_tag}"))

  return(out)
}
