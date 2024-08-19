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
