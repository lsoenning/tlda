test_that("Formula returns dispersion scores in the unit interval [0,1]", {
  expect_equal(min(disp(c(0,0,0,0,0,0,0,0,0,1), rep(1000, 10))), 0)
  expect_equal(max(disp(rep(1, 10), rep(1000, 10))), 1, tolerance = 1e-5)
  expect_equal(min(disp(rep(1, 10), rep(1000, 10))), 1, tolerance = 1e-5)
})
