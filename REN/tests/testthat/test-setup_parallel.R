test_that("setup_parallel creates a valid parallel cluster", {
  # Test with default number of cores (7)
  cl <- setup_parallel()  # By default, should use 7 cores
  expect_true(inherits(cl, "cluster"))  # Check if the object is a parallel cluster
  expect_equal(length(cl), 7)  # Check if the cluster has the correct number of cores
  stopCluster(cl)  # Ensure to stop the cluster after the test
})
