context("Involutory matrices")

# invol: involutory ------------------------------

test_that("hanowa matrix", {
  A <- hanowa(4)
  A2 <- hanowa(4, 2)

  B <- matrix(c(-1, 0, 1, 0, 0, -1, 0, 2, -1, 0, -1, 0, 0, -2, 0, -1),
              nrow = 4)

  C <- B
  diag(C) <- 2

  # Breaks on odd
  expect_error(hanowa(3))
  expect_error(hanowa(7))

  expect_equal(norm(A-B), 0)
  expect_equal(norm(A2-C), 0)
})
