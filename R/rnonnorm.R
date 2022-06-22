rnonnorm <- function(n, mean = 0, sd = 1, skew = 0, kurt = 0){
  var <- sd^2
  model <- function(x){
    f <- numeric(3)
    f[1] = x[1]^2 + 6*x[1]*x[3] + 2*x[2]^2 + 15*x[3]^2 - var
    f[2] = 2*x[2]*(4*x[2]^2 + 3*x[1]^2 + 36*x[1]*x[3] + 135*x[3]^2) - skew
    f[3] = 3*x[1]^4 + 60*x[1]^2*x[2]^2 + 60*x[2]^4 + 60*x[1]^3*x[3]+
      936*x[1]*x[2]^2*x[3] + 630*x[1]^2*x[3]^2 + 4500*x[2]^2*x[3]^2 +
      3780*x[1]*x[3]^3 + 10395*x[3]^4 - 3 - kurt
    f
  }
  xstart <- c(0, 0, -.1)
  ans <- as.data.frame(nleqslv::nleqslv(xstart, model,
                                        method = "Newton",
                                        jacobian=TRUE,
                                        control = list(trace=1,
                                                       btol=.00000000000000001,
                                                       delta="newton",
                                                       allowSingular=TRUE)))
  a <- - ans$x[2]
  b <- ans$x[1]
  c <- ans$x[2]
  d <- ans$x[3]
  xdat <- rnorm(n, mean = 0, sd = 1)
  dat <- (a + b*xdat + c*xdat^2 + d*xdat^3)*sd + mean
  info <- c(a, b, c, d)
  result <- list(dat = dat, Fleishman_coef = info)
  return(result)
}
