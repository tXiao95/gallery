context("minij matrices")

# SPD minij matrix ---------------------------------------------------------

test_that("SPD minij matrix", {
  A <- minij(10)

  for(i in 1:nrow(A)){
    for(j in i:nrow(A)){
      expect_equal(A[i,j], i)
      expect_equal(A[j,i], i)
    }
  }
})
