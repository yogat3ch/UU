
#' Convert r,g,b,a values as string or numeric to hex
#'
#' @param red \code{chr/num} Either a CSS `rgb()` or `rgba()` declaration as a string, or the red value as numeric.
#' @inheritParams grDevices::rgb
#' @param with_alpha \code{lgl} Whether to include the alpha value (an 8 digit hex) in the output or not
#'
#' @return \code{chr} The hex value
#' @export
#'
#' @examples
#' rgb2hex("rgba(18,180,211,1)", with_alpha = TRUE)
#' rgb2hex("rgba(18,180,211,1)", with_alpha = FALSE)
rgb2hex <- function(red, green, blue, with_alpha = FALSE) {
  if (is.character(red))
    v <- css_col2vec(red)
  else
    v <- c(red,green,blue)

  if (length(v) == 3 && with_alpha)
    v <- c(v, 1)
  else if (length(v) == 4 && !with_alpha)
    v <- v[-4]
  rlang::exec(rgb, !!!v, maxColorValue = 255)
}

#' Convert a CSS representation of a color to an r,g,b numeric
#'
#' @param x \code{chr} CSS Vector in hexadecimal or rgb/rgba format
#'
#' @return \code{num} with names r,g,b
#' @export
#'
#' @examples
#' css_col2vec("#12B4D3")
#' css_col2vec("rgba(111,96,140,1)")
css_col2vec <- function(x) {
  if (stringr::str_detect(x, "rgb"))
    out <- as.numeric(stringr::str_extract_all(x, "\\d{1,3}")[[1]][1:3])
  else
    out <- col2rgb(x)[,1]
  rlang::set_names(out, c("r","g","b"))
}


#' Compute color distance
#'
#' @param x \code{num/chr} A CSS hex or rgb/rgba color, or a numeric vector of r,g,b values
#' @param y \code{num/chr} A CSS hex or rgb/rgba color, or a numeric vector of r,g,b values
#'
#' @return \code{num} A representation of the distance for comparison with other distances
#' @export
#'
#' @examples
#' color_distance("rgba(111,96,140,1)", "#12B4D3")

color_distance <- function(x, y) {
  .cols <- purrr::map(list(x, y), ~purrr::when(!is.numeric(.x), isTRUE(.) ~ css_col2vec(.x), ~ .x))
  dc <- .cols[[1]] - .cols[[2]]
  rb <- .5 * (.cols[[1]][1] + .cols[[2]][1])
  sqrt(
    (2 + rb / 256) * dc[1] ^ 2 + 4 * dc[2] ^ 2 + (2 + (255 - rb / 256)) * dc[3] ^ 2
  )
}

#' Separate a vector of colors based on their distance
#'
#' @param x \code{chr} Vector of hex or rgb/rgba color values
#'
#' @return \code{chr} The original vector, sorted for contrast
#' @export
#'
#' @examples
#' color_separate(c("rgba(18,180,211,1)", "rgba(2,120,170,1)", "rgba(0,57,73,1)",
#' "rgba(72,36,18,1)", "rgba(111,96,140,1)", "rgba(0,166,212,1)",
#' "rgba(9,119,168,1)"))
color_separate <- function(x) {
  ox <- x
  sep_cols <- c(x[1])
  x <- x[-1]
  while (length(x)) {
    i <- which.max(purrr::map_dbl(x, ~ color_distance(utils::tail(sep_cols, 1), .x)))
    sep_cols <- c(sep_cols, x[i])
    x <- x[-i]
  }
  sep_cols
}

#' Makes a cyclic color palette of a specified length using the specified transformation each cycle
#'
#' @param colors \code{chr} vector of colors in hex format
#' @param n \code{num} length of color vector needed
#' @param transform_fn \code{fun} See \link[colorspace]{lighten} for examples
#' @param ... \code{args} passed on to `transform_fn`
#'
#' @return \code{chr} vector of `n` length
#' @export
#'
#' @examples
#' color_cycle(c("rgba(18,180,211,1)", "rgba(2,120,170,1)", "rgba(0,57,73,1)"), n = 9, amount = .2)
color_cycle <- function(colors, n, transform_fn = colorspace::lighten, ...) {
  if (any(stringr::str_detect(colors, "rgb")))
    colors <- purrr::map_chr(colors, rgb2hex)
  col_len <- length(colors)
  out <- colors
  i <- 1
  while (length(out) != n) {
    out <- c(out, transform_fn(out[i], ...))
    i <- i + 1
  }
  out
}
