# Define the unit tests for sort_by_names function
test_that("sort_by_names works with named vectors", {
  expect_equal(sort_by_names(c(b = "b", a = "a")), c(a = "a", b = "b"))
  expect_equal(sort_by_names(c(d = 4, c = 3, b = 2, a = 1)), c(a = 1, b = 2, c = 3, d = 4))
  expect_equal(sort_by_names(c(z = 3, x = 1, y = 2)), c(x = 1, y = 2, z = 3))
})

test_that("sort_by_names works with named vectors when by_names is FALSE", {
  expect_equal(sort_by_names(c(b = "b", a = "a"), by_names = FALSE), c(a = "a", b = "b"))
  expect_equal(sort_by_names(c(d = 4, c = 3, b = 2, a = 1), by_names = FALSE), c(a = 1, b = 2, c = 3, d = 4))
  expect_equal(sort_by_names(c(z = 3, x = 1, y = 2), by_names = FALSE), c(x = 1, y = 2, z = 3))
})

test_that("sort_by_names works with unnamed vectors", {
  expect_equal(sort_by_names(c(4, 3, 2, 1), sort_by_values = TRUE), c(1, 2, 3, 4))
  expect_equal(sort_by_names(c(3, 1, 2), sort_by_values = TRUE), c(1, 2, 3))
})

test_that("sort_by_names throws error for unnamed vectors when sort_by_values is FALSE", {
  expect_error(sort_by_names(c(4, 3, 2, 1), sort_by_values = FALSE), "`x` has no names")
})

test_that("sort_by_names works with named lists", {
  expect_equal(sort_by_names(list(b = "b", a = "a")), list(a = "a", b = "b"))
  expect_equal(sort_by_names(list(d = 4, c = 3, b = 2, a = 1)), list(a = 1, b = 2, c = 3, d = 4))
  expect_equal(sort_by_names(list(z = 3, x = 1, y = 2)), list(x = 1, y = 2, z = 3))
})

test_that("sort_by_names works with vectors when sort_by_values is TRUE", {
  expect_equal(sort_by_names(c(4, 3, 2, 1), sort_by_values = TRUE), c(1, 2, 3, 4))
})
test_that("sort_by_names gives message when `by_names` is TRUE but no names exist and sort_by_values is TRUE", {
  expect_message(sort_by_names(c(4, 3, 2, 1), sort_by_values = TRUE), regexp = "sorting by values")
})

test_that("sort_by_names throws error with unnamed lists when sort_by_values is TRUE", {
  expect_error(sort_by_names(list(3, 1, 2), sort_by_values = TRUE), regexp = "x must be a list")
})


test_that("sort_by_names throws error for unnamed lists when sort_by_values is FALSE", {
  expect_error(sort_by_names(list(4, 3, 2, 1), sort_by_values = FALSE), "`x` has no names")
})
