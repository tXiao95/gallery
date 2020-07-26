# fiedler: Fiedler Symmetric matrix ------------------------------------------------

#' @name fiedler
#' @title Create Fiedler matrix
#'
#' @description Fiedler matrix that has a dominant positive eigenvalue and all others are negative
#'
#' @param c N-vector. If \code{c} is a scalar, then returns fiedler(1:c)
#'
#' @return a symmetric dense matrix A with a dominant positive eigenvalue and all others are negative.
#'
#' @export
fiedler <- function(c){
  if (!is.vector(c)){
    stop("'c' is not a vector")
  }
  if(length(c) == 1){
    c <- 1:c
  }
  n <- length(c)
  A <- matrix(raw(), n, n)
  A <- matrix(data = abs(c[col(A)] - c[row(A)]), nrow = n, ncol = n)

  return(A)
}
