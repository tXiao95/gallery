context("Gallery matrices")

# binomial_matrix: Binomial matrix ---------------------------------------------------------

test_that("Binomial matrix", {
  for(n in 10:13){
    A <- binomial_matrix(n)
    expect_equal(norm(A %*% A - 2^(n-1) * diag(n)), 0)

    # B is involutory
    B <- A*2^((1-n)/2)
    expect_equal(norm(B %*% B - diag(n)), 0)
  }
  # Test for n = 4
  A <- binomial_matrix(4)
  B <- matrix(c(1,1,1,1,3,1,-1,-3,3,-1,-1,3,1,-1,1,-1), nrow=4)
  expect_equal(norm(A-B),0)
})
