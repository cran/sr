test_that("gamma histogram is stable", {
  skip_on_cran()
  me <- embed(mgls, 9)
  t <- me[ ,1]
  p <- me[ ,2:9]
  fe_out <- fe_search(p, t)
  #save(fe_out, file = "fe_out.rda")
  #load(file = "fe_out.rda")
  vdiffr::expect_doppelganger("full embedding histogram",
                              gamma_histogram(fe_out, bins=32, caption="test plot"))
})
