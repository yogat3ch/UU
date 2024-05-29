# Write tests for the rgb2hex function
test_that("rgb2hex works correctly with character input and alpha", {
  expect_equal(rgb2hex("rgba(18,180,211,1)", alpha = TRUE), "#12B4D3FF")
  expect_equal(rgb2hex("rgba(18,180,211,1)", alpha = FALSE), "#12B4D3")
})

test_that("rgb2hex works correctly with character input without alpha", {
  expect_equal(rgb2hex("rgb(18,180,211)", alpha = TRUE), "#12B4D3FF")
  expect_equal(rgb2hex("rgb(18,180,211)", alpha = FALSE), "#12B4D3")
})

test_that("rgb2hex works correctly with numeric input and alpha as boolean", {
  expect_equal(rgb2hex(18, 180, 211, alpha = TRUE), "#12B4D3FF")
  expect_equal(rgb2hex(18, 180, 211, alpha = FALSE), "#12B4D3")
})

test_that("rgb2hex works correctly with numeric input and specific alpha value", {
  expect_equal(rgb2hex(18, 180, 211, alpha = 0.5), "#12B4D380")
  expect_equal(rgb2hex(18, 180, 211, alpha = 1), "#12B4D3FF")
})

test_that("rgb2hex works correctly with boundary values", {
  expect_equal(rgb2hex(0, 0, 0, alpha = FALSE), "#000000")
  expect_equal(rgb2hex(255, 255, 255, alpha = FALSE), "#FFFFFF")
  expect_equal(rgb2hex(0, 0, 0, alpha = TRUE), "#000000FF")
  expect_equal(rgb2hex(255, 255, 255, alpha = TRUE), "#FFFFFFFF")
})

# Write tests for the css_col2vec function
test_that("css_col2vec works correctly with hexadecimal input", {
  expect_equal(css_col2vec("#12B4D3"), c(red = 18, green = 180, blue = 211, alpha = 255))
})

test_that("css_col2vec works correctly with rgb input", {
  expect_equal(css_col2vec("rgb(111,96,140)"), c(red = 111, green = 96, blue = 140, alpha = 255))
})

test_that("css_col2vec works correctly with rgba input", {
  expect_equal(css_col2vec("rgba(111,96,140,1)"), c(red = 111, green = 96, blue = 140, alpha = 255))
  expect_equal(css_col2vec("rgba(111,96,140,0.5)"), c(red = 111, green = 96, blue = 140, alpha = 128))
})

test_that("css_col2vec works correctly with named color input", {
  expect_equal(css_col2vec("green"), c(red = 0, green = 255, blue = 0, alpha = 255))
})

test_that("css_col2vec stops on invalid input", {
  expect_error(css_col2vec(123), "x must be character")
})

# Write tests for the css_col2vec_ function (vectorized version)
test_that("css_col2vec_ works correctly with multiple inputs", {
  inputs <- c("#12B4D3", "rgb(111,96,140)", "green")
  expected_outputs <- structure(
    c(18, 180, 211, 255, 111, 96, 140, 255, 0, 255, 0, 255),
    dim = 4:3,
    dimnames = list(
      c("red", "green", "blue", "alpha"),
      c("#12B4D3", "rgb(111,96,140)", "green")
    )
  )

  results <- css_col2vec_(inputs)
  expect_equal(results, expected_outputs)
})


# Write tests for the color_interpolate function
test_that("color_interpolate works correctly with default colors", {
  result <- color_interpolate(n = 3)
  expect_equal(result, c("#9A3324", "#4D5159", "#016F90"))

  result <- color_interpolate(n = 5)
  expect_equal(result, c("#9A3324", "#73423F", "#4D5159","#276075", "#016F90"))
})

test_that("color_interpolate works correctly with custom colors", {
  result <- color_interpolate(colors = c("#FF0000", "#00FF00"), n = 3)
  expect_equal(result, c("#FF0000", "#7F7F00", "#00FF00"))

  result <- color_interpolate(colors = c("#0000FF", "#FFFF00"), n = 4)
  expect_equal(result, c("#0000FF", "#5555AA", "#AAAA55", "#FFFF00"))
})

test_that("color_interpolate works correctly with single color interpolation", {
  result <- color_interpolate(colors = c("#FF0000", "#00FF00"), n = 1)
  expect_equal(result, c("#FF0000"))
})

test_that("color_interpolate works correctly with a large number of colors", {
  result <- color_interpolate(colors = c("#9A3324", "#016f90"), n = 10)
  expect_equal(length(result), 10)
})

test_that("color_interpolate handles invalid inputs", {
  expect_error(color_interpolate(colors = c("red", "blue"), n = 3), "`colors` must be a character vector of valid hex color codes.")
  expect_error(color_interpolate(colors = c("#FF0000", "#00FF00"), n = -1), "`n` must be a positive integer.")
  expect_error(color_interpolate(colors = c("#FF0000", "#00FF00"), n = 1.5), "`n` must be a positive integer.")
})
