test_that("select_by_mask works", {
  # two examples
  pt <- get_pt(8)
  p <- pt$p

  # works
  # empty mask
  goodmask <- integer(8)
  expect_error(select_by_mask(p, goodmask))

  goodmask[c(1,2,4,6,7)] <- 1
  pf <- select_by_mask(p, goodmask)
  expect_equal(gamma_test(pf, pt$t)$Gamma, 0.084865164)

  # works
  goodmask <- int_to_intMask(15, 8)     # select the first 4 columns
  pf <- select_by_mask(p, goodmask)
  expect_equal(gamma_test(pf, pt$t)$Gamma, 0.0078743941)

  # mask all ones
  # mask too long
  badmask <- integer(9)
  badmask[c(1,2,4,6,7)] <- 1
  expect_error(select_by_mask(p, badmask))

  # too short
  badmask <- integer(7)
  badmask[c(1,2,4,6,7)] <- 1
  expect_error(select_by_mask(p, badmask))


  # non vector predictors
  # Nan and character in vector
})
