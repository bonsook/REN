prepare_data <- function(dat, date_column_index = 1, start_date = '19990101', end_date = '20231231') {
  # Remove the date column and filter data based on the date range
  dat <- dat[,-date_column_index]
  dat <- matrix(unlist(dat), ncol = ncol(dat), byrow = "FALSE")
  data <- dat[dat[,1] >= start_date & dat[,1] <= end_date, ]

  # Prepare date and time-related columns
  Date <- as.Date(as.character(data[,1]), '%Y%m%d')
  mon <- month(Date) + (year(Date) - year(Date)[1]) * 12
  count <- as.numeric(table(mon))

  x <- data[,2:ncol(dat)]

  # Handle missing data
  if (any(x == -99.99)) {
    x <- x[, !apply(x, 2, function(col) any(col == -99.99))]
  }

  return(list(x = x, mon = mon, count = count, Date = Date))
}
