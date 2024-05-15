test_that("match_df works", {
  x <- data.frame(A = 1:10, B = letters[1:10], Z = "Text")
  y <- data.frame(A = 8:12, B = letters[8:12], Z = "Text")

  results_basic <- match_df(x, y)
  expect_s3_class(results_basic, "data.frame")
  expect_equal(nrow(results_basic), 3)

  expect_message(match_df(x, y, verbose = TRUE), "Matching on: A, B, Z")
  expect_silent(match_df(x, y, verbose = FALSE))

  results_onZ <- match_df(y, x, on = "Z")
  expect_s3_class(results_onZ, "data.frame")
  expect_equal(nrow(results_onZ), 5)

  results_log_verb <- match_df(x, y, out = logical())
  expect_identical(results_log_verb, c(rep(FALSE, 7), rep(TRUE, 3)))

  results_numeric <- match_df(x, y, out = numeric())
  expect_identical(results_numeric, c(8:10))
  ## I'm not sure if this is really the expected result.
  ## If so, there's a bug.
})
