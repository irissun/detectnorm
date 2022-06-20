
test_that("Check if destrunc provide stable result", {
  #run the simulation data
  set.seed(342301)
  test <- truncnorm::rtruncnorm(n = 5000, a = 0, b = 6, mean = 3.611, sd = 2.216)
  #check population mean: 3.611
  expect_equal(
    destrunc(vmean= mean(test), vsd = sd(test), lo=0, hi=6)$pmean, 3.611, tolerance = 1
  )
  #check population sd: 2.216
  expect_equal(
    destrunc(vmean= mean(test), vsd = sd(test), lo=0, hi=6)$psd, 2.216, tolerance = 1
  )
  #check the truncated mean: mean(test)
  expect_equal(
    destrunc(vmean= mean(test), vsd = sd(test), lo=0, hi=6)$tm,
    mean(test), tolerance = .001
  )
  #check the truncated sd:sd(test)
  expect_equal(
    destrunc(vmean= mean(test), vsd = sd(test), lo=0, hi=6)$tsd,
    sd(test), tolerance = .001
  )
})
