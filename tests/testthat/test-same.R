# Unit tests for the same function
test_that("same function works with named lists", {
  # lists are sorted by name and thereby the same
  expect_true(same(list(x = 1, y = 2), list(y = 2, x = 1)))
  expect_true(same(list(a = 1, b = 2), list(b = 2, a = 1)))
  # Lists are different
  expect_false(same(list(a = 1, b = 2), list(a = 1, b = 3)))
  expect_false(same(list(a = 1, b = 2), list(a = 2, b = 1)))
})

test_that("same function works with unnamed lists", {
  expect_false(same(list(1, 2), list(2, 1), sort_by_names = FALSE))
  expect_false(same(list(1, 2), list(2, 1)))
  expect_false(same(list(1, 2, 3), list(3, 2, 1), sort_by_names = FALSE))
  expect_false(same(list(1, 2, 3), list(3, 2, 1)))
  expect_true(same(list(1, 2, 3), list(1, 2, 3)))
})

test_that("same function works with atomic vectors", {
  expect_true(same(c(1, 2, 3), c(3, 2, 1), sort_by_names = FALSE))
  expect_true(same(c(1, 2, 3), c(3, 2, 1)))
  expect_false(same(c(1, 2, 3), c(3, 2, 1), no_sort = TRUE))
  expect_true(same(c("a", "b", "c"), c("c", "b", "a"), sort_by_names = FALSE))
  expect_true(same(c("a", "b", "c"), c("c", "b", "a")))
  expect_false(same(c("a", "b", "c"), c("c", "b", "a"), no_sort = TRUE))
})

test_that("same function works with data frames", {
  df1 <- data.frame(a = c(1, 2), b = c(3, 4))
  df2 <- data.frame(b = c(3, 4), a = c(1, 2))
  df3 <- data.frame(a = c(2, 1), b = c(4, 3))

  expect_true(same(df1, df2))
  expect_false(same(df1, df3))
})

test_that("same function works with more complex lists", {
  # Sorting by names is non-recursive, therefore these are false.
  expect_false(same(list(a = 1, b = list(c = 3, d = 4)), list(b = list(d = 4, c = 3), a = 1)))
  expect_false(same(list(a = 1, b = list(c = 3, d = 4)), list(a = 1, b = list(c = 4, d = 3))))
  # Target and current identical
  expect_true(same(list(a = 1, b = list(c = 3, d = 4)), list(a = 1, b = list(c = 3, d = 4))))
})

test_that("same function works with mixed types", {
  # lists are non-atomic, and thus cannot be sorted, order now matters.
  expect_false(same(list(1, "a", TRUE), list(TRUE, "a", 1)))
  expect_false(same(list(1, "a", TRUE), list(TRUE, "a", 1)))
  expect_true(same(list(1, "a", TRUE), list(1, "a", TRUE)))

})

test_that("same function returns FALSE for different length objects", {
  expect_false(same(c(1, 2, 3), c(1, 2)))
  expect_false(same(list(a = 1, b = 2), list(a = 1)))
})
