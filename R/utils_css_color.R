
#' Convert r,g,b,a values as string or numeric to hex
#' @family color
#' @param red \code{chr/num} Either a CSS `rgb()` or `rgba()` declaration as a string, or the red value as numeric.
#' @inheritParams grDevices::rgb
#' @param alpha \code{lgl/num} Whether to include the alpha value (an 8 digit hex) in the output or not, or the alpha value to apply, If set to TRUE, and alpha is not set, an alpha value of 1 will be appended. If numeric, a value between 0 & 1 inclusive to set as the alpha value.
#'
#' @return \code{chr} The hex value
#' @export
#' @seealso rgb2hex_
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
#' @export
rgb2hex_ <- Vectorize(rgb2hex)
#' Convert a CSS representation of a color to an r,g,b numeric
#'
#' @param x \code{chr} CSS Vector in hexadecimal or rgb/rgba format
#' @family color
#' @return \code{num} with names r,g,b
#' @export
#' @seealso css_col2vec_
#' @examples
#' css_col2vec("#12B4D3")
#' css_col2vec("rgba(111,96,140,1)")
#' css_col2vec("green")
css_col2vec <- function(x) {
  stopifnot(`x must be character` = inherits(x, "character"))
  if (stringr::str_detect(x, "^rgb")) {
    out <- as.numeric(stringr::str_extract_all(x, "[\\d\\.]+")[[1]])
  } else
    out <- col2rgb(x, alpha = TRUE)[,1]
  lo <- length(out)
  if (lo != 4)
    out <- c(out, alpha = ifelse(all(out < 1), 1, 255))
  else if (lo == 4 && out[4] <= 1) {
    out[4] <- round(255 * out[4])
  }

  rlang::set_names(out, c("red","green","blue","alpha"))
}
#' Vectorized version of `css_col2vec`
#' @rdname css_col2vec_
#' @export
css_col2vec_ <- Vectorize(css_col2vec)

#' Interpolate between two colors
#'
#' @param colors \code{chr} of hex colors
#' @param n \code{int} of the number of colors expected
#' @inheritDotParams grDevices::colorRamp
#'
#' @return \code{chr} of hex values
#' @export
#'
#' @examples
#' color_interpolate(n = 3)
color_interpolate <- function(colors = c("#9A3324", "#016f90"), n, ...) {
  .n <- n - 1
  apply(grDevices::colorRamp(colors, ...)(c(0, 1 / .n * 1:.n)), 1, \(.x) do.call(rgb2hex, as.list(.x)))
}

#' Convert vector of colors to named tbl
#'
#' @param colors \code{chr} vector of css color representations
#' @family color
#' @return \code{tbl} Columns `red`, `green`, `blue`, `alpha` and each color is a row
#' @export
#' @family color
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
#' See `color_text_by_luminance` for creating contrasting text color to a background.
#' @return \code{num}
#' @export
#' @family color
#' @seealso [color_text_by_luminance()]
#' @examples
#' rlang::exec(color_luminance, !!!css_col2vec('white'))
color_luminance <- function(red, green, blue, alpha, ...) {
  .rgb <- rlang::set_names(c(red, green, blue), c("red", "green", "blue"))
  .rgb <- .rgb / 255
  lt <- .rgb <= .04045
  .rgb[lt] <- .rgb[lt] / 12.92
  .rgb[!lt] <- ((.rgb[!lt] + .055) / 1.055) ^ 2.4

  unname(0.2126*.rgb["red"] + 0.7152*.rgb["green"] + 0.0722*.rgb["blue"])
}

#' Set text color based on luminance
#' @description
#' Useful for applying one or another of text colors based on the luminance of a background
#'
#' @param colors \code{chr} of css colors
#' @param text_light \code{chr} CSS color for light text
#' @param text_dark \code{chr} CSS color for dark text
#'
#' @return \code{chr} CSS text colors
#' @export
#' @family color
#' @examples
#' color_text_by_luminance(c("white", "magenta", "red", "brown", "yellow"))
color_text_by_luminance <- function(colors, text_light = "white", text_dark = "black") {
  l <- apply(css_col2vec_(colors),2, \(.x) {color_luminance(.x["red"], .x["green"], .x["blue"], .x["alpha"])} )
  out <- vector("character", length = length(l))
  out[l > .5] <- text_dark
  out[l < .5] <- text_light
  return(out)
}

#' Filter colors based on a luminance threshold
#'
#' @param colors \code{chr} color vector
#' @param dark_mode \code{lgl} colors will be `>=` if `TRUE`, otherwise `<=` if `FALSE`
#' @param luminance_threshold \code{lgl} 0-1 luminance threshold to filter
#'
#' @return \code{chr} vector of colors
#' @export
#' @family color
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
#' @param x \code{num/chr} A Named vector of CSS hex or rgb/rgba color
#' @param y \code{num/chr} A Named vector of CSS hex or rgb/rgba color
#'
#' @return \code{num} A representation of the distance for comparison with other distances
#' @export
#' @family color
#' @examples
#' color_distance(c(a = "rgba(111,96,140,1)"), c("#12B4D3", "green"))

color_distance <- function(x, y) {
  x <- unlist(x)
  y <- unlist(y)

  LAB <- purrr::map(list(x = x, y = y), \(.x) {
    t(UU::css_col2vec_(.x))[,1:3] |>
      grDevices::convertColor(from = "sRGB", to = "Lab")
  })

  #https://gist.github.com/ryancat/9972419b2a78f329ce3aebb7f1a09152
  out <- apply(LAB$x, 1, \(.x) {
    dL <- .x["L"] - LAB$y[,"L"]
    dA <- .x["a"] - LAB$y[,"a"]
    dB <- .x["b"] - LAB$y[,"b"]
    c1 <- sqrt(.x["L"] * .x["L"] + .x["a"] * .x["a"])
    c2 <- sqrt(LAB$y[,"L"] * LAB$y[,"L"] + LAB$y[,"a"] * LAB$y[,"a"])
    dC <- c1 - c2
    dH <- dA * dA + dB * dB - dC * dC
    dH[dH < 0] <- 0
    dH <- sqrt(dH)
    sc <- 1 + .045 * c1
    sh <- 1 + .015 * c1
    dCk <- dC / sc
    dHk <- dH / sh
    out <- dL * dL  + dCk * dCk + dHk * dHk
    out[out < 0] <- 0
    sqrt(out)
  })

  if (length(out) != 1) {
    if (dim(out)[2] == 1) {
      nm <- if (!is.null(names(x))) {
        names(x)
      } else {
        "x"
      }
      colnames(out) <- nm
    }
  }

  return(out)
}

#' Match colors by visual distance
#' @description Helpful for pairing colors across light/dark palettes
#' @param x \code{chr} Named CSS representations of colors that will be matched
#' @param y \code{chr} Named CSS representations of colors that will be selected from as matches
#' @param with_replacement \code{lgl} Whether colors in x should have unique matches in y, or if y can be matched with replacement. Default TRUE to match with the closest color in y, regardless if it's already been matched with a previous color in x.
#' @return \code{tbl} Of matches for all of `x` with the associated distance
#' @family color
#' @export
#' @examples
#' a <- list(
#'   light = list(
#'     aquamarine = "rgb(137,210,215)",
#'     teal = "rgb(79,194,198)",
#'     turquoise = "rgb(3,119,132)",
#'     evergreen = "rgb(0,84,92)",
#'     brown = "rgb(72,36,18)",
#'     lightbrown = "rgb(132,96,78)",
#'     orange = "rgb(250,173,25)",
#'     darkorange = "rgb(198,143,44)",
#'     aliceblue = "rgb(180,243,249)",
#'     gainsboro = "rgb(223,254,255)"
#'   ),
#'   dark = list(
#'     lightblue = "rgba(18,180,211,1)",
#'     darkblue = "rgba(2,120,170,1)",
#'     navyblue = "rgba(0,57,73,1)",
#'     brown = "rgba(72,36,18,1)",
#'     lightbrown = "rgb(132,96,78)",
#'     lighterbrown = "rgba(181,141,122, 1)",
#'     purple = "rgba(111,96,140,1)",
#'     lightpurple = "rgba(165,150,194,1)",
#'     darkcyan = "rgba(0,166,212,1)",
#'     darkturquoise = "rgba(9,119,168,1)"
#'   )
#' )
#' color_match(a$light, a$dark)

color_match <- function(x, y, with_replacement = TRUE) {
  dist <- color_distance(x, y)
  if (length(dist) > 1) {
    out <- if (with_replacement) {
      out <- apply(dist, 2, \(.x) {
        .x[which.min(.x)]
      }, simplify = FALSE)
      out <- tibble::tibble(x = names(out), y = unname(sapply(out, names)), dist = unname(unlist(out)))

    } else {
      # Need to find global minima :-/
      dd <- dim(dist)
      ld <- length(dist)
      cn <- colnames(dist)
      rn <- rownames(dist)
      out <- purrr::map_dfr(order(dist), \(.x) {
        .col <- plyr::round_any(.x, 10, f = ceiling) %/% dd[1]
        .row <- .x %% dd[1]
        .row <- if (.row == 0)
          dd[1]
        else
          .row

        c(x = cn[.col], y = rn[.row], dist = dist[.x])
      })
      out_x <- out[!duplicated(out$x),]
      dup_y <- which(duplicated(out_x$y))
      used_y <- out_x$y[setdiff(1:nrow(out_x), dup_y)]
      unused_y <- setdiff(names(y), used_y)
      while (length(used_y) != length(y)) {
        dup_y <- which(duplicated(out_x$y))
        left_to_match <- out_x$x[dup_y]
        o <- dplyr::filter(out, x %in% left_to_match & !y %in% used_y)
        o_x <- o[!duplicated(o$x),]
        to_add <- o_x[1,]
        out_x[out_x$x == to_add$x, ] <- o_x[1,]
        used_y <- c(to_add$y, used_y)
        unused_y <- setdiff(names(y), used_y)
      }
      out <- out_x
    }
  } else
    UU::gbort("Must supply more than 1 color for y")

  out <- dplyr::mutate(
    out,
    x_nm = x,
    x = unname(unlist(.env$x)[match(x_nm, names(.env$x))]),
    y_nm = y,
    y = unname(unlist(.env$y)[match(y_nm, names(.env$y))])
  ) |>
    dplyr::select(dplyr::starts_with("x"),dplyr::starts_with("y"), dist)
  out
}

#' Separate a vector of colors based on their distance
#'
#' @param x \code{chr} Vector of hex or rgb/rgba color values
#'
#' @return \code{chr} The original vector, sorted for contrast
#' @export
#' @family color
#' @examples
#' color_separate(c("rgba(18,180,211,1)", "rgba(2,120,170,1)", "rgba(0,57,73,1)",
#' "rgba(72,36,18,1)", "rgba(111,96,140,1)", "rgba(0,166,212,1)",
#' "rgba(9,119,168,1)"))
color_separate <- function(x) {
  ox <- x
  sep_cols <- c(x[1])
  x <- x[-1]
  while (length(x)) {
    i <- which.max(color_distance(utils::tail(sep_cols, 1), x))
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
#' @family color
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

#' Convert a list of colors to SCSS/Sass variables or classes
#'
#' @param color_theme \code{2-layer list} with dark/light themes
#' @inheritParams base::cat
#' @return \code{msg} Sass/SCSS formatted variables or classes
#' @export
#' @family color
#' @examples
#' colors2css(c(a = "white", b = "green"))
colors2css <- function(colors, file = "", sass_vars = TRUE) {
  colors_formatted <- unlist(colors)
  # Format w/ CSS compliant chrs
  colors_formatted <- rlang::set_names(colors_formatted, paste0(ifelse(sass_vars, "$", "."), stringr::str_replace(names(colors_formatted), "\\.", "\\-")))

  fmt <- if (sass_vars) {
    "%s: %s;"
  } else {
    "%s { color : %s; }"
  }
  out <- sprintf(fmt, names(colors_formatted), colors_formatted)
  if (nzchar(file))
    cat(out, file = file, sep = "\n")
  return(out)
}
