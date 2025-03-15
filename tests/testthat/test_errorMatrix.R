
test_data <- function() {
  dm <- matrix(c(0.0, 2.0, 3.0, 3.4, 3.0,
                 2.0, 0.0, 4.1, 4.0, 4.0,
                 3.0, 4.1, 0.0, 2.0, 0.1,
                 3.4, 4.0, 2.0, 0.0, 2.1,
                 3.0, 4.0, 0.1, 2.1, 0.0),
               nrow = 5, ncol = 5, byrow = TRUE)


  me <- matrix(c(0.0, 0.0, 0.0, 0.0, 0.0,
                 7.0, 0.0, 0.0, 0.0, 0.0,
                 2.1, 3.0, 0.0, 0.0, 0.0,
                 3.3, 4.5, 4.4, 0.0, 0.0,
                 1.0, 1.0, 1.0, 2.0, 0.0),
               nrow = 5, ncol = 5, byrow = TRUE)

  expected_part <- c(5/2, 0.9/3, 0.1/3.4, 2/3,
                     1.1/4.1, 0.5/4,3/4,
                     2.4/2, 0.9/0.1,
                     0.1/2.1)

  expected_mean <- mean(expected_part)
  expected_max <- max(expected_part)

  dm2 <- matrix(c(0,8,
                  8,0),
                nrow = 2, ncol = 2, byrow = TRUE)

  me2 <- matrix(c(0,0,
                  3,0),
                nrow = 2, ncol = 2, byrow = TRUE)

  l <- list()
  l[["dm"]] <- dm
  l[["me"]] <- me
  l[["expected_mean"]] <- expected_mean
  l[["expected_max"]] <- expected_max

  return(l)
}


test_that("mean error", {
  dm <- test_data()[["dm"]]
  me <- test_data()[["me"]]
  expected <- test_data()[["expected_mean"]]

  expect_equal(calculate_distance_error(dm, me, "mean"), expected)
})

test_that("max error", {
  dm <- test_data()[["dm"]]
  me <- test_data()[["me"]]
  expected <- test_data()[["expected_max"]]

  expect_equal(calculate_distance_error(dm, me, "max"), expected)
})
