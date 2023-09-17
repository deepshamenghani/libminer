test_that("lib_summary returns expected result", {
  res <- lib_summary() # Call the summary function inside the test to make sure the res version is as expected
  expect_s3_class(res, "data.frame") # check the expected object type
  expect_equal(ncol(res), 2) # The number of columns in the output
  expect_equal(names(res), c("Library", "n_packages"))
  expect_type(res$Library, "character")
  expect_type(res$n_packages, "integer")
})

test_that("lib_summary fails appropriately", {
  expect_error(lib_summary("foo"), "'sizes must be TRUE or FALSE.'")
})

test_that("sizes argument works", {
  res <- lib_summary(sizes = TRUE)
  expect_equal(names(res), c("Library", "n_packages", "lib_size"))
  expect_type(res$lib_size, "double")
})
