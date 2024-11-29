.beta_fun <- function(a, b){
  m <- a + b
  bmean <- a/(a+b)
  bsd <- sqrt(a*b/(m^2*(m+1)))
  bskew <- 2*(b-a)*sqrt(a+b+1)/((a+b+2)*sqrt(a*b))
  bkurt <- 6*((b - a)^2*(a+b+1) - a*b*(a+b+2))/(a*b*(a+b+2)*(a+b+3))
  return(data.frame(bmean, bsd, balpha = a, bbeta = b, bskew, bkurt))
}
