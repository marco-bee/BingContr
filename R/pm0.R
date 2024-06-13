#' Estimating a dynamic mixture via AMLE

pm0 <- function(X,lambda)
{
  nr <- nrow(X)
  f <- rep(0,nr)
  up = length(lambda)
  for (i in 1:nr)
  {
    f[i] <- exp(-sum(lambda[1:up]*X[i,1:up]^2))
  }
  return(f)
}
