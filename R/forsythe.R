# forsythe: Perturbed Jordan block ----------------------------------------------------------------

#' @name forsythe
#' @title Create Forsythe matrix or perturbed Jordan block
#'
#' @description Returns a \code{n}-by-\code{n} matrix equal to the Jordan block with
#'   eigenvalue \code{lambda}, except that \code{A[n,1]=alpha}.
#'
#' @param n order of matrix
#' @param alpha value of perturbation at \code{A[n,1]}
#' @param lambda eigenvalue of Jordan block
#'
#' @export
forsythe <- function(n, alpha=.Machine$double.eps, lambda = 0){
  A <- jordbloc(n, lambda)
  A[n, 1] <- alpha
  return(A)
}
