
temp_dir <- tempdir()

# Start writing tests
test_that("list.files2 returns named character vector", {
  # Setup: Create a temporary directory and files for testing

  file.create(file.path(temp_dir, "file1.txt"))
  file.create(file.path(temp_dir, "file2.csv"))

  # Test: Check if the function returns a named character vector
  result <- list.files2(temp_dir)
  expect_type(result, "character")
  expect_named(result)
  expect_true(all(c("file1", "file2") %in% names(result)))
})

test_that("list.files2 returns full file paths when full.names = TRUE", {
  file.create(file.path(temp_dir, "file1.txt"))

  # Test: Check if the function returns full file paths
  result <- list.files2(temp_dir, full.names = TRUE)
  expect_true(all(startsWith(result, temp_dir)))
})

test_that("list.files2 handles non-existent directories", {
  # Test: Check if the function throws an error for non-existent directories
  non_existent_dir <- file.path(temp_dir, "non_existent_dir")
  expect_error(list.files2(non_existent_dir),
               regexp = paste0(" does not exist."))
})

test_that("list.files2 passes additional arguments to list.files", {
  sub_dir <- file.path(temp_dir, "sub_dir")
  dir.create(sub_dir)
  file.create(file.path(temp_dir, "file1.txt"))
  file.create(file.path(sub_dir, "file2.txt"))

  # Test: Check if the function respects the `recursive` argument
  result <- list.files2(temp_dir, recursive = TRUE)
  expect_true(any(grepl("sub_dir/file2.txt", result)))

  result_non_recursive <- list.files2(temp_dir, recursive = FALSE)
  expect_false(any(grepl("sub_dir/file2.txt", result_non_recursive)))
})

test_that("list.files2 names are stripped extensions", {
  file.create(file.path(temp_dir, "file1.txt"))

  # Test: Check if the function names are stripped extensions
  result <- list.files2(temp_dir)
  expect_equal(names(result), ext(basename(result), strip = TRUE))
})

# Cleanup: Remove temporary files and directories created for testing
unlink(temp_dir, recursive = TRUE)
