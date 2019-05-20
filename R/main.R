#' Retrieves the air quality data for a specific year, sorts the rows and columns and stores the data in a CSV file
#'
#' @param air.data The data to be processed
#'
#' @return Nothing
#' @export
#'
#' @examples
#' collectAirqData(getYearData(2014), 2014)  # Get the data for a full year
#' collectAirqData(getDaysData("2019-01-01", "2019-04-30"), 2019)  # Get data for a specific range of dates
collectAirqData <- function(air.data, aYear) {
  
  datetime <- dmy_hm(air.data$`FECHA-HORA`)
  air.data$HORA  <- paste0(
    str_sub(paste0("0", hour(datetime)), -2, -1),
    ":",
    str_sub(paste0("0", minute(datetime)), -2, -1))
  air.data$FECHA <- as.Date(datetime)
  
  air.data <- air.data[ , c("FECHA", "HORA", "Station", "SO2", "PART", "NO2", "CO", "O3")]
  air.data <- air.data[order(as.Date(datetime)), ]
  write.csv(air.data,
            file = paste0("data/airdata", aYear, ".csv"),
            row.names = FALSE, quote = FALSE)
}

#' Applies post-processing steps to the data retrieved from the air quality web page
#' 
#' @param infile The file name with the original data
#' @param outfile The file name where the post-processed data will be written
#' 
#' @return Nothing
#' @export
#' 
postprocess <- function(infile, outfile) {
  infile  <- paste0("data/", infile)
  outfile <- paste0("data/", outfile)
  
  stopifnot(file.exists(infile))
  
  data <- read.csv(infile, stringsAsFactors = FALSE)
  
  # Remove NAs from data columns
  data_columns <- 4:8
  for (column in data_columns) {
    NAs <- which(is.na(data[ , column]))
    for (row in NAs) {
      key_date <- data[row, "FECHA"]
      data[row, column] <-data %>% 
        filter(FECHA == key_date) %>% 
        summarize(median(column))
    }
  }
  
  # Summarize data by day
  new_data <- data %>%
    group_by(FECHA) %>%
    summarize(
      max_SO2    = max(SO2),
      min_SO2    = min(SO2),
      mean_SO2   = mean(SO2),
      median_SO2 = median(SO2),
      max_PART    = max(PART),
      min_PART    = min(PART),
      mean_PART   = mean(PART),
      median_PART = median(PART),
      max_NO2    = max(NO2),
      min_NO2    = min(NO2),
      mean_NO2   = mean(NO2),
      median_NO2 = median(NO2),
      max_CO    = max(CO),
      min_CO    = min(CO),
      mean_CO   = mean(CO),
      median_CO = median(CO),
      max_O3    = max(O3),
      min_O3    = min(O3),
      mean_O3   = mean(O3),
      median_O3 = median(O3)
    )
  
  write.csv(new_data, outfile, row.names = FALSE, quote = FALSE)
}