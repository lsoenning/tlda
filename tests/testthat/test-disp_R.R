test_that("Formula returns dispersion scores in the unit interval [0,1]", {
  expect_equal(min(disp_R(
    c(0,0,0,0,0,0,0,0,0,10), 
    rep(1000, 10),
    type = "relative")) >= 0, TRUE)
  
  expect_equal(min(disp_R(
    c(0,0,0,0,0,0,0,0,0,10), 
    rep(1000, 10),
    type = "relative_withsize")) >= 0, TRUE)

  
  expect_equal(max(disp_R(
    rep(1, 10), 
    rep(1000, 10),
    type = "relative")) <= 1, TRUE, tolerance = 1e-5)
  
  expect_equal(max(disp_R(
    rep(1, 10), 
    rep(1000, 10),
    type = "relative_withsize")) <= 1, TRUE, tolerance = 1e-5)
  
  
  expect_equal(min(disp_R(
    rep(1, 10), 
    rep(1000, 10),
    type = "relative_withsize")) <= 1, TRUE, tolerance = 1e-5)
  
  expect_equal(min(disp_R(
    rep(1, 10), 
    rep(1000, 10),
    type = "relative")) <= 1, TRUE, tolerance = 1e-5)
})
