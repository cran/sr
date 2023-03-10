test_that("tests fe_search and mask_histogram", {
  # compare some results (one fe_search, multiple samples)
  skip_on_cran()

  me <- embed(mgls, 11)
  t <- me[ ,1]
  p <- me[ ,2:11]
  fe_out <- fe_search(p, t)
  #save(fe_out, file = "fe_out.rda")
  #load(file = "fe_out.rda")

  goodies <- fe_out[1:50, ]
  vdiffr::expect_doppelganger("plot 50 good",
                              mask_histogram(goodies, 8, caption="plot good"))

  baddies <- tail(fe_out, 50)
  vdiffr::expect_doppelganger("plot 50 bad",
                              mask_histogram(baddies, 8, caption = "plot bad"))

})
