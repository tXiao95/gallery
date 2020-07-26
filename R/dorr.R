# dorr: Dorr matrix -------------------------------------------------------

#' @name dorr
#' @title Create Dorr matrix
#'
#' @description Returns a \code{n}-by-\code{n} row diagonally dominant,
#'   tridiagonal matrix that is ill-conditioned for small nonnegative values
#'   of \code{theta}. The default value of \code{theta} is 0.01.
#'
#' @param n order of matrix
#' @param theta determines conditionality. Ill-conditioned when theta is nonnegative.
#'
#' @export
dorr <- function(n, theta=0.01){
  h <- 1 / (n + 1)
  m <- floor( (n + 1) / 2)
  term <- theta / h^2

  c <- rep(0, n); e <- c; d <- c

  i <- 1:m
  c[i] <- -term * rep(1, m)
  e[i] <- c[i] - (0.5 - i * h) / h
  d[i] <- -(c[i] + e[i])

  i <- (m+1):n
  e[i] <- -term * rep(1, n-m)
  c[i] <- e[i] + (0.5 - i * h) / h
  d[i] <- -(c[i] + e[i])

  c <- c[2:n]
  e <- e[1:(n-1)]

  return(tridiag(n, c, d, e))
}
