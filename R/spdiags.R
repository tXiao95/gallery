#' @importFrom Matrix sparseMatrix

# spdiags: Sparse Diagonal Matrix --------------------------------------------------

#' @name spdiags
#' @title Create sparse diagonal matrix
#'
#' @description Creates a sparse representation of multiple diagonal matrix
#'
#' @param A matrix where columns correspond to the desired diagonals
#' @param d indices of the diagonals to be filled in. 0 is main diagonal. -1
#'   is first subdiagonal and +1 is first superdiagonal.
#' @param m row dim
#' @param n col dim
#'
#' @return dgcMatrix sparse diagonal
#'
#' @export
spdiags <- function(A, d, m, n){
  num_diags <- length(d)
  A_vec <- rows <- cols <- vector(mode = "list", length = num_diags)

  for(k in 1:num_diags){
    d_k <- d[k]
    if(d_k == 0){
      i <- 1:m
      j <- 1:n
    } else if(d_k > 0){
      i <- 1:(m-d_k)
      j <- (1 + d_k):n
    } else if(d_k < 0){
      i <- (1 - d_k):m
      j <- 1:(n + d_k)
    }
    rows[[k]] <- i
    cols[[k]] <- j
    A_vec[[k]] <- A[j,k]
  }

  rows  <- unlist(rows)
  cols  <- unlist(cols)
  A_vec <- unlist(A_vec)
  B <- sparseMatrix(i = rows, j = cols, x = A_vec,dims = c(m, n))

  return(B)
}
