#' @importFrom pracma Toeplitz

# dramadah: Matrix of zeros and ones --------------------------------------

#' @name dramadah
#' @title Create anti-Hadamard matrix \code{A}
#'
#' @description Returns a \code{n}-by-\code{n} nonsingular matrix of 0's and 1's.
#'   With large determinant or inverse. If \code{k=1}, \code{A} is Toeplitz and \code{abs(det(A))=1}.
#'   If \code{k=2}, \code{A} is upper triangular and Toeplitz. If \code{k=3}, \code{A} has maximal
#'   determinant among (0,1) lower Hessenberg matrices. Also is Toeplitz.
#'
#'   Also known as an anti-Hadamard matrix.
#'
#' @param n order of matrix
#' @param k decides type of matrix returned.
#'
#' @export
dramadah <- function(n, k=1){
  if(k == 1){ # Toeplitz
    c <- rep(1, n)
    for(i in seq(2, n, 4)){
      m <- min(1, n-i)
      c[i:(i+m)] <- 0
    }
    r <- rep(0, n)
    r[1:4] <- c(1, 1, 0, 1)
    if(n < 4){
      r <- r[1:n]
    }
    A <- Toeplitz(c, r)
  } else if(k == 2){ # Upper triangular and Toeplitz
    c <- rep(0, n)
    c[1] <- 1
    r <- rep(1, n)
    r[seq(3, n, 2)] <- 0
    A <- Toeplitz(c, r)
  } else if(k == 3){ # Lower Hessenberg
    c <- rep(1, n)
    c[seq(2, n, 2)] <- 0
    A <- Toeplitz(c, c(1, 1, rep(0, n-2)))
  }
  return(A)
}
