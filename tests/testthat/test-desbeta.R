test_that("Check if desbeta provide stable result", {
  #metadat[11,]group1
  #check skewness
  expect_equal(
    desbeta(vmean = 3.611, vsd = 2.216, lo = 0, hi = 6)$bskew,
    -.40007, tolerance = .01
  )
  #metadat[7,]group1
  #check kurtosis
  expect_equal(
    desbeta(vmean = 4.05, vsd = 1.38, lo = 0, hi = 6)$bskew,
    -.591313, tolerance = .01
  )
  #metadat[3,]group1
  #check alpha
  expect_equal(
    desbeta(vmean = 9.9, vsd = 3.69, lo = 0, hi = 14)$balpha,
    1.400871, tolerance = .01
  )
  #metadat[9,]group1
  #check beta
  expect_equal(
    desbeta(vmean = .66, vsd = .25, lo = 0, hi = 1)$bbeta,
    0.880736, tolerance = .01
  )
  #metadat[10, ]group 1
  #check mean
  expect_equal(
    desbeta(vmean = 11.1, vsd = 2.8, lo = 0, hi = 14)$bmean,
    0.7928571, tolerance = .01
  )
  #metadat[19, ]group 1
  #check sd
  expect_equal(
    desbeta(vmean = 54.6, vsd = 29.8, lo = 0, hi = 100)$bsd,
    0.298, tolerance = .01
  )
})
