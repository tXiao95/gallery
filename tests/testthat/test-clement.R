context("Clement matrices")

# clement: Tridagonal matrix with 0 diagonal ------------------------------

test_that("clement matrix", {
  # Diagonal is 0
  A <- clement(10)
  B <- clement(10,1)
  expect_equal(unique(diag(A)), 0)
  expect_equal(unique(diag(B)), 0)

  # Symmetric if k=1
  expect_true(isSymmetric(B))
})
