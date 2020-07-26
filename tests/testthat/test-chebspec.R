context("Chebspec matrices")

# chebspec: Chebyshev spectral differentiation matrix ---------------------------------------------------------

test_that("Chebspec matrix is nilpotent", {
  C <- chebspec(4)
  # C is nilpotent, k=0
  expect_equal(norm(C %*% C %*% C %*% C), 0)

  # C has nullspace dim 1
  sigmas <- svd(C)$d
  smallest <- sigmas[length(sigmas)]
  next_smallest <- sigmas[length(sigmas)-1]
  expect_equal(smallest, 0)
  expect_true(next_smallest > sqrt(.Machine$double.eps))

  # and the null vector is all ones
  expect_equal(norm(C %*% c(1,1,1,1)), 0)

  # Eigenvalues have all negative real parts when k=1
  C <- chebspec(4, 1)
  eigs <- eigen(C)$values
  expect_true(all(Re(eigs) < 0))
})
