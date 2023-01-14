
#' Convert r,g,b,a values as string or numeric to hex
#'
#' @param red \code{chr/num} Either a CSS `rgb()` or `rgba()` declaration as a string, or the red value as numeric.
#' @inheritParams grDevices::rgb
#' @param alpha \code{lgl/num} Whether to include the alpha value (an 8 digit hex) in the output or not, or the alpha value to apply, If set to TRUE, and alpha is not set, an alpha value of 1 will be appended. If numeric, a value between 0 & 1 inclusive to set as the alpha value.
#'
#' @return \code{chr} The hex value
#' @export
#'
#' @examples
#' rgb2hex("rgba(18,180,211,1)", alpha = TRUE)
#' rgb2hex("rgba(18,180,211,1)", alpha = FALSE)
rgb2hex <- function(red, green, blue, alpha = FALSE) {
  if (is.character(red))
    v <- css_col2vec(red)
  else
    v <- c(red,green,blue)


  if (alpha) {
    num_alpha <- is.numeric(alpha)
    if (length(v) == 3)
      v <- c(v, alpha = ifelse(num_alpha, alpha, 1))
    else
      v[4] <- ifelse(num_alpha, alpha, v[4])
  } else if (length(v) == 4 && !alpha)
    v <- v[-4]
  i <- which(v < 1)
  v[i] <- round(v[i] * 255)
  if (isTRUE(v[4] == 1))
    v[4] <- 255

  rlang::exec(grDevices::rgb, !!!v, maxColorValue = 255)
}

#' Convert a CSS representation of a color to an r,g,b numeric
#'
#' @param x \code{chr} CSS Vector in hexadecimal or rgb/rgba format
#'
#' @return \code{num} with names r,g,b
#' @export
#' @seealso css_col2vec_
#' @examples
#' css_col2vec("#12B4D3")
#' css_col2vec("rgba(111,96,140,1)")
#' css_col2vec("green")
css_col2vec <- function(x) {
  if (stringr::str_detect(x, "^rgb")) {
    out <- as.numeric(stringr::str_extract_all(x, "[\\d\\.]+")[[1]])
  } else
    out <- col2rgb(x, alpha = TRUE)[,1]
  if (length(out) != 4)
    out <- c(out, alpha = ifelse(all(out < 1), 1, 255))
  rlang::set_names(out, c("red","green","blue","alpha"))
}

#' @export
css_col2vec_ <- Vectorize(css_col2vec)

#' Convert vector of colors to named tbl
#'
#' @param colors \code{chr} vector of css color representations
#'
#' @return \code{tbl} Columns `red`, `green`, `blue`, `alpha` and each color is a row
#' @export
#'
#' @examples
#' color_rgb_table(c("red", "green", "#45fa32"))
color_rgb_table <- function(colors) {
  x <- t(css_col2vec_(colors))
  out <- tibble::as_tibble(x)
  if (!is.null(rownames(x)))
    out <- tibble::add_column(out, name = rownames(x), .before = 1)
  return(out)
}

#' Find the luminance of a particular color, scaled 0-1
#'
#' @inheritParams grDevices::rgb
#' @param ... additional arguments

#' @return \code{num}
#' @export
#'
#' @examples
#' color_luminance(css_col2vec('white'))
color_luminance <- function(red, green, blue, alpha, ...) {
  .rgb <- c(red = red, green = green, blue = blue)
  .rgb <- .rgb / 255
  lt <- .rgb <= .04045
  .rgb[lt] <- .rgb[lt] / 12.92
  .rgb[!lt] <- ((.rgb[!lt] + .055) / 1.055) ^ 2.4

  unname(0.2126*.rgb["red"] + 0.7152*.rgb["green"] + 0.0722*.rgb["blue"])
}


#' Filter colors based on a luminance threshold
#'
#' @param colors \code{chr} color vector
#' @param dark_mode \code{lgl} colors will be `>=` if `TRUE`, otherwise `<=` if `FALSE`
#' @param luminance_threshold \code{lgl} 0-1 luminance threshold to filter
#'
#' @return \code{chr} vector of colors
#' @export
#'
#' @examples
#' x <- tibble::tribble(
#' ~CSS.Name, ~Hex.Code, ~Decimal.Code,
#' "Black",        "#000000",            "(0,0,0)",
#' "White",        "#FFFFFF",      "(255,255,255)",
#' "Red",        "#FF0000",          "(255,0,0)",
#' "Lime",        "#00FF00",          "(0,255,0)",
#' "Blue",        "#0000FF",          "(0,0,255)",
#' "Yellow",        "#FFFF00",        "(255,255,0)",
#' "Cyan / Aqua",        "#00FFFF",        "(0,255,255)",
#' "Magenta / Fuchsia",        "#FF00FF",        "(255,0,255)",
#' "Silver",        "#C0C0C0",      "(192,192,192)",
#' "Gray",        "#808080",      "(128,128,128)",
#' "Maroon",        "#800000",          "(128,0,0)",
#' "Olive",        "#808000",        "(128,128,0)",
#' "Green",        "#008000",          "(0,128,0)",
#' "Purple",        "#800080",        "(128,0,128)",
#' "Teal",        "#008080",        "(0,128,128)",
#' "Navy",        "#000080",          "(0,0,128)"
#' )
#' luminance_filter(x$Hex.Code, TRUE)
#' luminance_filter(x$Hex.Code, FALSE)
luminance_filter <- function(colors, dark_mode, luminance_threshold = .5) {
  luminance <- UU::color_rgb_table(colors) |>
    purrr::pmap_dbl(\(...) {
      .x <- list(...)
      rlang::exec(UU::color_luminance, !!!.x)
    })
  op <- if (dark_mode)
    base::`>=`
  else
    base::`<=`

  i <- op(luminance, luminance_threshold)
  colors[i]
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
  .cols <- css_col2vec_(c(x, y))
  dc <- .cols[,1] - .cols[,2]
  rb <- .5 * (.cols[,1][1] + .cols[,2][1])
  unname(sqrt(
    (2 + rb / 256) * dc[1] ^ 2 + 4 * dc[2] ^ 2 + (2 + (255 - rb / 256)) * dc[3] ^ 2
  ))
}

#' Match colors by visual distance
#' @description Helpful for pairing colors across light/dark palettes
#' @param x \code{chr} CSS representations of colors that will be matched
#' @param x \code{chr} CSS representations of colors that will be selected from as matches
#' @return \code{tbl} Of matches for all of `x` with the associated distance
#' @export
color_match <- function(x, y) {
  tidyr::expand_grid(x ,y) |>
    tidyr::unnest() |>
    dplyr::mutate(dist = purrr::map2_dbl(light, dark, UU::color_distance)) |>
    dplyr::group_by(light) |>
    dplyr::filter(min(dist) == dist) |>
    dplyr::arrange(dist)
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
  if (n <= col_len) {
    # Handle case where n asks for fewer than the colors provided
    out <- out[1:n]
  } else {
    i <- 1
    if (!n)
      gbort("{.code n} must be greater than 0")
    while (length(out) != n) {
      out <- c(out, transform_fn(out[i], ...))
      i <- i + 1
      if (i > 100 && i > n)
        gbort("{.code color_cycle} while loop exceeds {.code n} and 100 without completion")
    }
  }

  out
}
