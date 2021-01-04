context("Check Distance Matrix")

m_ok <- matrix(c(0,1,1,
                 1,0,1,
                 1,1,0),3,3)
m_wrong_rule1 <- matrix(rep(1,9),3,3)
m_wrong_rule2 <- matrix(c(0,1.3,1,
                          1,0,1,
                          1,1,0),3,3)
m_wrong_rule3 <- matrix(c(0,3,1,
                          3,0,1,
                          1,1,0),3,3)
m_2_3 <- matrix(rep(1,6),2,3)
m_with_NA <- matrix(c(0,1,1,1,0,1,1,NA,0),3,3)
m_with_negative <- matrix(c(0,1,1,1,0,1,1,-1,0),3,3)


test_that("validation against NA", {
  expect_equal(is_distance_matrix_ok(m_with_NA), FALSE)
})

test_that("validation of square matrix ", {
  expect_equal(is_square_matrix(m_ok), TRUE)
  expect_equal(is_square_matrix(m_wrong_rule1), TRUE)
  expect_equal(is_square_matrix(m_2_3), FALSE)
})

test_that("validation against negative", {
  expect_equal(is_distance_matrix_ok(m_with_negative), FALSE)
})

test_that("validation of first metric rule ", {
  expect_equal(is_metric_rule1_fulfilled(m_ok), TRUE)
  expect_equal(is_metric_rule1_fulfilled(m_wrong_rule1), FALSE)
})

test_that("validation of second metric rule ", {
  expect_equal(is_metric_rule2_fulfilled(m_ok), TRUE)
  expect_equal(is_metric_rule2_fulfilled(m_wrong_rule3), TRUE)
  expect_equal(is_metric_rule2_fulfilled(m_wrong_rule2), FALSE)
})

test_that("validation of third metric rule ", {
  expect_equal(is_metric_rule3_fulfilled(m_ok), TRUE)
  expect_equal(is_metric_rule3_fulfilled(m_wrong_rule2), TRUE)
  expect_equal(is_metric_rule3_fulfilled(m_wrong_rule3), FALSE)
})

test_that("is_distance_matrix_ok", {
  expect_equal(is_distance_matrix_ok(m_ok), TRUE)
  expect_error(is_distance_matrix_ok(m_2_3), "This is not a square matrix")
  expect_equal(is_distance_matrix_ok(m_wrong_rule1), FALSE)
  expect_equal(is_distance_matrix_ok(m_wrong_rule2), FALSE)
  expect_equal(is_distance_matrix_ok(m_wrong_rule3), FALSE)
})

test_that("debug proper distanse matrix", {
  expect_output(debug_distance_matrix(m_ok),"No NA in matrix: OK")
  expect_output(debug_distance_matrix(m_ok), "No negative values in matrix: OK")
  expect_output(debug_distance_matrix(m_ok), "First metric rule: OK")
  expect_output(debug_distance_matrix(m_ok), "Second metric rule: OK")
  expect_output(debug_distance_matrix(m_ok), "Third metric rule: OK")
})

test_that("debug distanse matrix with basic errors", {
  expect_error(debug_distance_matrix(m_2_3), "This is not a square matrix")
  expect_output(debug_distance_matrix(m_with_NA),
                "NA found in matrix: <-- ERROR")
  expect_output(debug_distance_matrix(m_with_NA),
                "No negative values in matrix: OK")
})

test_that("debug distanse matrix with first metric rule broken", {
  expect_output(debug_distance_matrix(m_wrong_rule1),
                "First metric rule: Not OK")
  expect_output(debug_distance_matrix(m_wrong_rule1), "Second metric rule: OK")
  expect_output(debug_distance_matrix(m_wrong_rule1), "Third metric rule: OK")
})

test_that("debug distanse matrix with second metric rule broken", {
  expect_output(debug_distance_matrix(m_wrong_rule2), "First metric rule: OK")
  expect_output(debug_distance_matrix(m_wrong_rule2),
                "Second metric rule: Not OK")
})

test_that("debug distanse matrix with third metric rule broken", {
  expect_output(debug_distance_matrix(m_wrong_rule3), "First metric rule: OK")
  expect_output(debug_distance_matrix(m_wrong_rule3), "Second metric rule: OK")
  expect_output(debug_distance_matrix(m_wrong_rule3),
                "Third metric rule: Not OK")
})

