test_that("gamma_test works", {

  pt <- get_pt(8)
  p <- pt$p
  t <- pt$t

  # data.frame input equals matrix
  ml <- gamma_test(p,t)
  expect_equal(gamma_test(as.data.frame(p), t), ml)

  # vector input
  p1 <- select_by_mask(p,int_to_intMask(1,8))
  ml <- gamma_test(p1, t)
  expect_equal(gamma_test(as.vector(p1), t), ml)

  # file lengths wrong
  # verbose output
  skip_on_cran()
  # plot is stable
  vdiffr::expect_doppelganger("gamma test plot", gamma_test(p,t, plot=TRUE, caption="test plot"))


})
