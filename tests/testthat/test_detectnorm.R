test_that("Check if detectnorm provide stable result", {
  #metadat[1,]group1
  #check skewness
  expect_equal(
    detectnorm(m1i = m1,sd1i = sd1,n1i = n1,
               hi1i = p.max,lo1i = 0,m2i = m2,sd2i = sd2,n2i = n2,
               hi2i = p.max,lo2i=0,distri = "beta", data = metadat)[1, ]$skewness1,
    1.04186, tolerance = .01
  )
  #metadat[7,]group1
  #check kurtosis
  expect_equal(
    detectnorm(m1i = m1,sd1i = sd1,n1i = n1,
               hi1i = p.max,lo1i = 0,m2i = m2,sd2i = sd2,n2i = n2,
               hi2i = p.max,lo2i=0,distri = "beta", data = metadat)[7, ]$kurtosis1,
    -0.5369345, tolerance = .01
  )
  #metadat[20,]group2
  #check skewness
  expect_equal(
    detectnorm(m1i = m1,sd1i = sd1,n1i = n1,
               hi1i = p.max,lo1i = 0,m2i = m2,sd2i = sd2,n2i = n2,
               hi2i = p.max,lo2i=0,distri = "beta", data = metadat)[20, ]$skewness2,
    2.442969, tolerance = .01
  )
  #metadat[19,]group1
  #check kurtosis
  expect_equal(
    detectnorm(m1i = m1,sd1i = sd1,n1i = n1,
               hi1i = p.max,lo1i = 0,m2i = m2,sd2i = sd2,n2i = n2,
               hi2i = p.max,lo2i=0,distri = "beta", data = metadat)[19, ]$kurtosis2,
    -0.1480539, tolerance = .01
  )
})
