test_data <- function() {
  cp <- matrix(c(1,2,1,
                -2,0,2,
                -3,1,1,
                 0,0,2), nrow = 4, ncol = 3, byrow = TRUE)


  dm <- matrix(c(0,0,0,0,
                 sqrt(9+4+1),0,0,0,
                 sqrt(16+1+0),sqrt(1+1+1),0,0,
                 sqrt(1+4++1),sqrt(4+0+0),sqrt(9+1+1),0),
               nrow = 4, ncol = 4, byrow = TRUE)

  l = list()
  l[["positions"]] <- cp
  l[["dm"]] <- dm

  return(l)
}

test_that("distance matrix", {
  positions <- test_data()[["positions"]]
  expected <- test_data()[["dm"]]

  expect_equal(prepare_current_distance_matrix(positions), expected)
})
