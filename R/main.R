#' Retrieves the air quality data for a specific year, sorts the rows and columns and stores the data in a CSV file
#'
#' @param aYear The year of interest
#'
#' @return Nothing
#' @export
#'
#' @examples
collectAirqData <- function(aYear) {
  air.data <- getYearData(aYear)
  
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

