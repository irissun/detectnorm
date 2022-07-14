test_that("Check if detectnorm provide stable result", {
  #metadat[1,]group1
  #check skewness
  expect_equal(
    detectnorm(m1i = 1.02592,sd1i = 0.8995642,n1i = 160,
               hi1i = 5.578894,lo1i = 0.2083603,m2i = 1.430021,sd2i = 1.059845,n2i = 160,
               hi2i = 2.274204,lo2i = -3.177617,distri = "beta")$g1_skewness,
    1.483046, tolerance = .01
  )
  #metadat[7,]group1
  #check kurtosis
  expect_equal(
    detectnorm(m1i = 1.02592,sd1i = 0.8995642,n1i = 160,
               hi1i = 5.578894,lo1i = 0.2083603,m2i = 1.430021,sd2i = 1.059845,n2i = 160,
               hi2i = 2.274204,lo2i = -3.177617,distri = "beta")$g1_kurtosis,
    1.890163, tolerance = .01
  )
  #metadat[20,]group2
  #check skewness
  expect_equal(
    detectnorm(m1i = 1.02592,sd1i = 0.8995642,n1i = 160,
               hi1i = 5.578894,lo1i = 0.2083603,m2i = 1.430021,sd2i = 1.059845,n2i = 160,
               hi2i = 2.274204,lo2i = -3.177617,distri = "beta")$g2_kurtosis,
    2.004892, tolerance = .01
  )
})
