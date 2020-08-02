dbeta4param <- function(x,alpha,beta,lo,hi){
  ((x-lo)^(alpha-1) * (hi - x)^(beta-1)) /
    ((hi - lo)^(alpha+beta-1) * beta(alpha,beta))
}
