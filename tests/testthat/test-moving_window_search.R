test_that("moving_window_search works", {
  # vector, matrix, data.frame inputs

  # number of windows and N/M

  # plots look good
  pt <- get_pt(8)
  p <- pt$p
  t <- pt$t

  skip_on_cran()
  # plot
  vdiffr::expect_doppelganger("moving window works",
                              moving_window_search(p,t,
                                                   window_size = 50,
                                                   by = 2))
  vdiffr::expect_doppelganger("moving window vratio",
                              moving_window_search(p,t,
                                                   window_size = 50,
                                                   by = 2,
                                                   show="vratio"))



})
