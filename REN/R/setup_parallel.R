setup_parallel <- function(num_cores = 7) {
  # Load necessary libraries
  Packages <- c("glmnet", "quadprog", "doParallel", "mvtnorm", "doSNOW", "lubridate", "CCA", "Matrix", "tictoc", "corpcor", "PerformanceAnalytics", "ggplot2", "reshape2")
  lapply(Packages, library, character.only = TRUE)
  source("Functions/po_fun_test.R")  # Update the path accordingly

  # Allow the user to override the number of cores
  user_input <- as.numeric(readline(prompt = sprintf("Enter the number of cores to use (default is %d): ", num_cores)))
  if (!is.na(user_input) && user_input > 0) {
    num_cores <- user_input
  }

  # Set up parallel processing
  cl <- makeCluster(num_cores)  # Create a cluster
  registerDoParallel(cl)

  return(cl)
}
