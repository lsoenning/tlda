test_that("Formula returns dispersion scores in the unit interval [0,1]", {
  expect_equal(min(disp_DP(
    c(0,0,0,0,0,0,0,0,0,10), 
    rep(1000, 10),
    formula = "gries_2008")) >= 0, TRUE)
  
  expect_equal(max(disp_DP(
    rep(1, 10), 
    rep(1000, 10),
    formula = "gries_2008")), 1, tolerance = 1e-5)
  
  expect_equal(min(disp_DP(
    rep(1, 10), 
    rep(1000, 10),
    formula = "gries_2008")), 1, tolerance = 1e-5)
  
  
  expect_equal(min(disp_DP(
    c(0,0,0,0,0,0,0,0,0,10), 
    rep(1000, 10),
    formula = "lijffijt_gries_2012")), 0, tolerance = 1e-5)
  
  expect_equal(max(disp_DP(
    rep(1, 10), 
    rep(1000, 10),
    formula = "lijffijt_gries_2012")), 1, tolerance = 1e-5)
  
  expect_equal(min(disp_DP(
    rep(1, 10), 
    rep(1000, 10),
    formula = "lijffijt_gries_2012")), 1, tolerance = 1e-5)
  
  
  expect_equal(min(disp_DP(
    c(0,0,0,0,0,0,0,0,0,10), 
    rep(1000, 10),
    formula = "egbert_etal_2020")), 0, tolerance = 1e-5)
  
  expect_equal(max(disp_DP(
    rep(1, 10), 
    rep(1000, 10),
    formula = "egbert_etal_2020")), 1, tolerance = 1e-5)
  
  expect_equal(min(disp_DP(
    rep(1, 10), 
    rep(1000, 10),
    formula = "egbert_etal_2020")), 1, tolerance = 1e-5)
})
