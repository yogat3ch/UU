% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_css_color.R
\name{color_text_by_luminance}
\alias{color_text_by_luminance}
\title{Set text color based on luminance}
\usage{
color_text_by_luminance(colors, text_light = "white", text_dark = "black")
}
\arguments{
\item{colors}{\code{chr} of css colors}

\item{text_light}{\code{chr} CSS color for light text}

\item{text_dark}{\code{chr} CSS color for dark text}
}
\value{
\code{chr} CSS text colors
}
\description{
Useful for applying one or another of text colors based on the luminance of a background
}
\examples{
color_text_by_luminance(c("white", "magenta", "red", "brown", "yellow"))
}
\seealso{
Other color: 
\code{\link{color_cycle}()},
\code{\link{color_distance}()},
\code{\link{color_luminance}()},
\code{\link{color_match}()},
\code{\link{color_rgb_table}()},
\code{\link{color_separate}()},
\code{\link{colors2css}()},
\code{\link{css_col2vec}()},
\code{\link{luminance_filter}()},
\code{\link{rgb2hex}()}
}
\concept{color}
