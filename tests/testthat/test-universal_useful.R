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


test_that("match_letters function matches letters correctly", {
  # Test case 1: Single letter match
  result_1 <- match_letters("apple", "a", n = 1)
  expect_equal(result_1, "a")

  # Test case 2: Case insensitive match
  result_2 <- match_letters(c("apple", "banana"), "A", ignore.case = TRUE)
  expect_equal(result_2, "A")

  # Test case 3: Capitalize matched letters
  result_3 <- match_letters(c("apple", "banana"), "a", capitalize = TRUE)
  expect_equal(result_3, "A")
})


test_that("missing_args function identifies missing arguments correctly", {
  # Test case 1: No missing arguments when all arguments are passed
  fn1 <- function(a, b) missing_args()
  result_1 <- fn1(a = 1, b = 2)
  print(result_1)
  expect_length(result_1, 0)
  expect_equal(result_1, character(0))

  # Test case 2: Identify missing arguments when some arguments are not passed
  fn2 <- function(a, b) missing_args()
  result_2 <- fn2(a = 1)
  expect_length(result_2, 1)
  expect_equal(result_2, "b")

  # Test case 3: Include NULL arguments in missing arguments
  fn3 <- function(a = NULL, b) missing_args()
  result_3 <- fn3()
  expect_length(result_3, 2)
  expect_equal(result_3, c("a", "b"))

  # Test case 4: Exclude default arguments from missing arguments
  fn4 <- function(a = 1, b) missing_args()
  result_4 <- fn4(b = 2)
  expect_length(result_1, 0)
  expect_equal(result_1, character(0))
})


test_that("list_rename function renames list elements correctly", {
  # Sample data
  data <- list(a = 1, b = 2, c = 3)

  # Expected result after renaming
  expected_result <- list(x = 1, y = 2, z = 3)

  # Apply the function
  result <- list_rename(data, a = "x", b = "y", c = "z")

  # Check if the result matches the expected result
  expect_equal(result, expected_result)
})
