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


test_that("expr_pipe works", {
  df <- data.frame(val = 1:10)
  exprs <- list(
    quote(df),
    quote(dplyr::mutate(val = val + 5, category = ifelse(val > 10, "High", "Low"))),
    quote(dplyr::group_by(category)),
    quote(dplyr::summarise(s = sum(val)))
  )
  exp_piped <- expr_pipe(exprs)

  expect_true(is.call(exp_piped))
  expect_error(
    expr_pipe(quote(df)),
    "`exprs` must be a list."
  )
  expect_error(
    expr_pipe(list(quote(df))),
    "`exprs` should have more tan 1 element for a pipe to take effect."
  )
  expr_pipe(list(quote(data.frame(val = 1:10)), quote(dplyr::mutte(new = val * 2)))) |>
    expect_error() |>
    expect_warning("The first element of `exprs` should be of class 'name'.")

  expect_identical(
    rlang::eval_bare(exp_piped),
    tibble::tibble(category = c("High", "Low"), s = c(65, 40))
  )
})


test_that("find_by_class works", {
  UU_testing_env <- new.env(parent = emptyenv())
  df <- data.frame(A = 1:10, B = letters[1:10], C = rnorm(10))
  assign("df", df, envir = UU_testing_env)

  expect_silent(x <- find_by_class("data.frame", UU_testing_env))

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 10)
  expect_equal(ncol(x), 3)

  df2 <- data.frame(A = 1, B = "B", C = 1.23)
  assign("df2", df2, envir = UU_testing_env)

  expect_warning(
    find_by_class("data.frame", UU_testing_env),
    "More than one object with class: data.frame. Returning the first found.")
  expect_warning(
    find_by_class("numeric", UU_testing_env),
    "Could not find object with class numeric. Has it been instantiated?")
})
