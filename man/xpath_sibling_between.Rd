% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_scraping.R
\name{xpath_sibling_between}
\alias{xpath_sibling_between}
\title{Generate xpath to find sibling nodes between two elements
The function produces a compounding xpath with each subsequent argument provided. Thus the final argument specified will be the node that is selected by the resulting xpath with the exception of \code{nested_tag_contains} which helps to identify a nested tag by its contents}
\usage{
xpath_sibling_between(
  start_tag,
  following_sibling,
  preceding_sibling,
  nested_tag,
  nested_tag_contains
)
}
\arguments{
\item{start_tag}{\code{chr} xpath of the upper bounding element}

\item{following_sibling}{\code{chr} relative xpath of the lower bounding element}

\item{preceding_sibling}{\code{chr} relative xpath for the type of elements in between the upper and lower bounding elements to retrieve}

\item{nested_tag}{\code{chr} relative xpath of an element nested within the \code{preceding_sibling} elements}

\item{nested_tag_contains}{\code{chr} of text that the nested_tag element contains (in order to identify it. The nested tag element itself will be selected).}
}
\value{
\code{chr} xpath statement
}
\description{
Generate xpath to find sibling nodes between two elements
The function produces a compounding xpath with each subsequent argument provided. Thus the final argument specified will be the node that is selected by the resulting xpath with the exception of \code{nested_tag_contains} which helps to identify a nested tag by its contents
}
\concept{webscraping}
