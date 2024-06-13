#' Density of the ACG distribution

#' @param X matrix of observations (data matrix).
#' @param A parameter matrix.
#' @return f ACG density evaluated at X.
#' @examples
#' N <- 100
#' q <- 3
#' b <- 1
#' lambda_n <- c(.588,.421,0)
#' A <- diag(lambda_n)
#' Psinv = diag(q) + (2/b) * A
#' Psi = solve(Psinv)
#' Y <- rACG(N,Psi)
#' f <- dACG(Y,Psi)

dACG <- function(X,A)
{
  p <- ncol(X)
  nr <- nrow(X)
  f <- rep(0,nr)
  Adet <- det(A)
  for (i in 1:nr)
  {
    f[i] <- exp(t(X[i,]) %*% A %*% X[1,])^(-p/2) * sqrt(Adet)
  }
  return(f)
}
