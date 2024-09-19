#' Regularization Ensemble Portfolio Analysis (REN)
#'
#' This function serves as the main entry point for performing portfolio analysis using the Regularization Ensemble
#' (REN) methodology. It sets up parallel processing, prepares the input data, and performs the analysis to generate
#' performance metrics and visualizations.
#'
#' @param dat A data frame or matrix where the first column is the date and the remaining columns are the data (typically asset returns).
#' @param date_column_index The index of the date column in the input data. Default is 1.
#' @param start_date A character string specifying the start date for filtering the data in 'YYYYMMDD' format. Default is '19990101'.
#' @param end_date A character string specifying the end date for filtering the data in 'YYYYMMDD' format. Default is '20231231'.
#' @param num_cores The number of cores to use for parallel processing. Default is 7.
#'
#' @return A list containing the results of the portfolio analysis including:
#' \describe{
#'   \item{cumulative_return_plot}{A ggplot object representing the cumulative returns for each method.}
#'   \item{cumulative_turnover_plot}{A ggplot object representing the cumulative turnover for each method.}
#'   \item{turnover_mean}{A numeric vector of the mean turnover for each method.}
#'   \item{sharpe_ratio}{A numeric vector of the Sharpe ratio for each method.}
#'   \item{volatility}{A numeric vector of the annualized volatility for each method.}
#'   \item{max_drawdown}{A numeric vector of the maximum drawdown for each method.}
#'   \item{vw_to_mean}{The mean turnover for the volume-weighted (VW) portfolio.}
#'   \item{vw_sharpe_ratio}{The Sharpe ratio for the VW portfolio.}
#'   \item{vw_volatility}{The annualized volatility for the VW portfolio.}
#'   \item{vw_max_drawdown}{The maximum drawdown for the VW portfolio.}
#' }
#'
#' @details The function first sets up parallel processing based on the specified number of cores. It then prepares
#' the input data by filtering it based on a date range and handling missing data. The core analysis is performed using
#' various portfolio optimization methods, and the results include performance metrics such as Sharpe ratio, volatility,
#' and maximum drawdown, as well as visualizations of cumulative returns and turnover.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result <- ren(dat)
#' print(result$cumulative_return_plot)
#' print(result$cumulative_turnover_plot)
#' }
#'
#' @export
ren <- function(dat, date_column_index = 1, start_date = '19990101', end_date = '20231231', num_cores = 7) {
  # Setup parallel processing
  cl <- setup_parallel(num_cores)

  # Data preparation
  data_prep <- prepare_data(dat, date_column_index, start_date, end_date)
  x <- data_prep$x
  mon <- data_prep$mon
  count <- data_prep$count
  Date <- data_prep$Date

  # Perform core analysis including performance metrics and visualization
  result <- perform_analysis(x, mon, count, Date, num_cores)

  return(result)
}
