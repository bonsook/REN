test_that("setup_parallel creates a valid parallel cluster", {
  # Test with default number of cores (7)
  cl <- setup_parallel()  # By default, should use 7 cores
  expect_true(inherits(cl, "cluster"))  # Check if the object is a parallel cluster
  expect_equal(length(cl), 7)  # Check if the cluster has the correct number of cores
  stopCluster(cl)  # Ensure to stop the cluster after the test

  # Test with a specified number of cores (e.g., 4)
  cl <- setup_parallel(num_cores = 4)  # Manually set to 4 cores
  expect_true(inherits(cl, "cluster"))  # Check if the object is a parallel cluster
  expect_equal(length(cl), 4)  # Check if the cluster has 4 cores
  stopCluster(cl)  # Ensure to stop the cluster after the test
})
