context("Circulant matrices")

library(pracma)

# circul: Circulant matrix --------------------------------------------------------

test_that("Circulant matrix is circular", {
  # Check every diagonal in the matrix is of the same value (Toeplitz)
  n <- 100
  A <- circul(n)
  B <- circul(rnorm(n))

  i <- row(A)
  j <- col(A)

  bound <- (2*n - 2) / 2
  for (k in -bound:bound){
    # Check that matrix is Toeplitz (diagonals all equal)
    expect_equal(length(unique(A[i == (j + k)])), 1)
    expect_equal(length(unique(B[i == (j + k)])), 1)

    # Check that matrix is circulant: ith diagonal equal to (i+n)th diagonal
    if(k < 0){
      expect_equal(unique(Diag(A, k = k)), unique(Diag(A, k = k + n)))
      expect_equal(unique(Diag(B, k = k)), unique(Diag(B, k = k + n)))
    }
  }
})
