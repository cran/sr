test_that("fe_search works", {
  # successful tests are covered by gamma_histogram and mask_histogram tests
  # a couple of failure tests:
  x <- 1:500
  expect_error(fe_search(x, x))
})
