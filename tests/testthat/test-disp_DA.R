test_that("Formula returns dispersion scores in the unit interval [0,1]", {
  expect_equal(min(disp_DA(
    c(0,0,0,0,0,0,0,0,0,1), 
    rep(1000, 10))) >= 0, TRUE)
  
  expect_equal(max(disp_DA(
    rep(1, 10), 
    rep(1000, 10))), 1, tolerance = 1e-5)
  
  expect_equal(min(disp_DA(
    rep(1, 10), 
    rep(1000, 10))), 1, tolerance = 1e-5)

})
