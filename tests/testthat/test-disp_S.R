test_that("Formula returns dispersion scores in the unit interval [0,1]", {
  expect_true(disp_S(c(0,0,0,0,0,0,0,0,0,10), rep(1000, 10)) >= 0)
  expect_true(round(disp_S(rep(1, 10), rep(1000, 10)), 10) <= 1)
})
