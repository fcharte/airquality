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

