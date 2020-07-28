context("Involutory matrices")

# invol: involutory ------------------------------

test_that("involutory matrix", {
  A <- invol(3)

  B <- matrix(c(-3, -36, 30, 1/2, 8, -7.5, 1/3, 6, -6),
              nrow=3)

  # Verify
  expect_equal(norm(A-B), 0)
  # Is involutory
  expect_equal(norm(A%*%A - diag(3)), 0)
})
