test_that("int_to_intMask works", {
  expect_error(int_to_intMask(42, 33))
  expect_error(int_to_intMask(42, -1))
  expect_equal(int_to_intMask(42, 16), c(0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0))
  # otherwise adequately tested by increasing_search and fe_searchv
})
