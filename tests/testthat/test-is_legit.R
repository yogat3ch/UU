
# Unit tests for is_legit and its helper functions
test_that("legit_null correctly identifies NULL", {
  expect_true(legit_null(NULL))
  expect_false(legit_null(1))
  expect_false(legit_null(NA))
  expect_false(legit_null("a"))
})

test_that("legit_empty correctly identifies empty objects", {
  expect_true(legit_empty(character(0)))
  expect_true(legit_empty(list()))
  expect_true(legit_empty(integer(0)))
  expect_false(legit_empty(1))
  expect_false(legit_empty(NA))
  expect_false(legit_empty(c(1, 2, 3)))
})

test_that("legit_na correctly identifies NA objects", {
  expect_true(legit_na(NA))
  expect_true(legit_na(c(NA, NA)))
  expect_false(legit_na(NULL))
  expect_false(legit_na(1))
  expect_false(legit_na(c(1, NA)))
  expect_false(legit_na(character(0)))
  expect_true(legit_na(data.frame(a = c(NA, NA))))
  expect_false(legit_na(data.frame(a = 1)))
})

test_that("legit_error correctly identifies errors", {
  expect_true(legit_error(try(log("a"), silent = TRUE)))
  expect_false(legit_error(1))
  expect_false(legit_error(NA))
  expect_false(legit_error(NULL))
  expect_false(legit_error("error"))
})

test_that("is_legit works with different combinations of checks", {
  expect_false(is_legit(NULL)) # NULL
  expect_false(is_legit(character(0))) # Empty
  expect_false(is_legit(NA)) # NA
  expect_false(is_legit(try(log("a"), silent = TRUE))) # Error

  expect_true(is_legit(1)) # Legit
  expect_true(is_legit("a")) # Legit
  expect_true(is_legit(c(1, 2, 3))) # Legit

  # Combinations
  expect_false(is_legit(NULL, is.null = FALSE)) # FALSE because null also counts as empty
  expect_true(is_legit(character(0), is_empty = FALSE))
  expect_true(is_legit(NA, is.na = FALSE))
  expect_true(is_legit(try(log("a"), silent = TRUE), not_error = FALSE))

  # Multiple checks disabled
  expect_true(is_legit(NULL, is.null = FALSE, is_empty = FALSE))
  expect_true(is_legit(NA, is.na = FALSE, is_empty = FALSE))
})
