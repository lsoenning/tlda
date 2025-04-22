test_that("standardization returns dispersion scores in the unit interval [0,1]", {
  
  expect_true(round(
    disp_DKL(
      c(0,0,0,0,0,0,0,0,0,10), 
      rep(1000, 10),
      standardization = "o2p"), 10) >= 0)

  expect_true(round(
    disp_DKL(
      rep(1, 10), 
      rep(1000, 10),
      standardization = "o2p"), 10) <= 1)
  
  
  expect_true(round(
    disp_DKL(
      c(0,0,0,0,0,0,0,0,0,10), 
      rep(1000, 10),
      standardization = "base_e"), 10) >= 0)

  expect_true(round(
    disp_DKL(
      rep(1, 10), 
      rep(1000, 10),
      standardization = "base_e"), 10) <= 1)
  
  
  expect_true(round(
    disp_DKL(
      c(0,0,0,0,0,0,0,0,0,10), 
      rep(1000, 10),
      standardization = "base_2"), 10) >= 0)

  expect_true(round(
    disp_DKL(
      rep(1, 10), 
      rep(1000, 10),
      standardization = "base_2"), 10) <= 1)
})
