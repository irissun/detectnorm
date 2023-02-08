rdirich <- function(n, alpha){
  alpha <- rbind(alpha)
  alpha.dim <- dim(alpha)
  if (n > alpha.dim[1]) 
    alpha <- matrix(alpha, n, alpha.dim[2], byrow = TRUE)
  x <- matrix(rgamma(alpha.dim[2] * n, alpha), ncol = alpha.dim[2])
  sm <- x %*% rep(1, alpha.dim[2])
  return(x/as.vector(sm))
}