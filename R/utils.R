#
# Utility functions to scrap data from the air quality stations in Jaén
#
# April 2019 - Francisco Charte
#

library(lubridate)
library(dplyr)
library(httr)
library(rvest)
library(stringr)

# Components of the URL to request from the server
#
baseURL <- "http://www.juntadeandalucia.es/medioambiente/atmosfera/informes_siva/"
months <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")
place <- "/nja"  # This component sets the province to "Jaén". It should be changed to get data from other places
extension <- ".htm"


#' Composes the URL to get air quality data for a given day
#'
#' @param aday The `Date` to retrieve
#'
#' @return A string with the URL
#' @export
#'
#' @examples
#' makeURL(as.Date("2019-03-31"))
#' 
makeURL <- function(aday) {
  stopifnot(lubridate::is.Date(aday))
  
  paste0(baseURL,
         months[lubridate::month(aday)],
         lubridate::year(aday)-2000,
         place,
         format(aday, "%y%m%d"),
         extension)
}
