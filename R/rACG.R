#' Simulation of the Angular Central Gaussian (ACG) distribution
#'
#' This function simulates the Angular Central Gaussian (ACG) distribution
#' @param N: number of simulated random vectors.
#' @param Psi: (q x q) matrix of parameters of the angular central Gaussian distribution
#' @return Y matrix of nreps simulated vectors of length q.
#' @examples
#' N <- 100
#' q <- 3
#' b <- 1
#' lambda_n <- c(.588,.421,0)
#' A <- diag(lambda_n)
#' Psinv = diag(q) + (2/b) * A
#' Psi = solve(Psinv)
#' Y <- rACG(N,Psi)

rACG <- function(N,Psi)
{
require(mvtnorm)
q = nrow(Psi)
Y = matrix(0,N,q)
X = rmvnorm(N,rep(0,q),Psi)
Y = X/sqrt(rowSums(X^2))
return(Y)
}
