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
  # Test warning
  y$B <- letters[11:15]
  expect_warning(match_df(x, y, on = "B"), regexp = stringr::fixed("No common keys between `x` and `y` on feature B"))

  y <- rlang::set_names(x, letters[8:10])
  expect_error(match_df(x, y), regexp = "no common features")

  y <- y[2:7,]
  expect_identical(match_df(x, y, on = c(i = "B"), out = numeric()), 2:7)
})
