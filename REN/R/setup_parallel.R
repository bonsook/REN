#' Setup Parallel Processing for Portfolio Analysis
#'
#' This function sets up parallel processing by loading necessary libraries, allowing the user to specify the
#' number of cores to use, and creating a parallel backend for faster computation.
#'
#' @param num_cores The default number of cores to use for parallel processing. Default is 7.
#'
#' @return A parallel cluster object that can be used with functions that support parallel computation.
#'
#' @details This function allows the user to specify the number of cores for parallel processing either through the
#' argument \code{num_cores} or via interactive user input. The function also loads a set of libraries required for
#' portfolio analysis.
#'
#' @examples
#' \dontrun{
#' # Set up parallel processing with the default number of cores
#' cl <- setup_parallel()
#'
#' # Set up parallel processing with 4 cores
#' cl <- setup_parallel(num_cores = 4)
#'
#' # Remember to stop the cluster when finished
#' stopCluster(cl)
#' }
#'
#' @export
setup_parallel <- function(num_cores = 7) {
  # Load necessary libraries
  Packages <- c("glmnet", "quadprog", "doParallel", "mvtnorm", "doSNOW", "lubridate", "CCA", "Matrix", "tictoc", "corpcor", "PerformanceAnalytics", "ggplot2", "reshape2")
  lapply(Packages, library, character.only = TRUE)

  # Since po_fun_test.R is now in the R/ folder, there's no need to source it manually.

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
