#' @title Passenger Volume by Origin/Destination Train Station
#'
#' @description Returns a link from LTA that enables the user to download a CSV file containing a month's worth of data on passenger volume by origin/destination MRT station.
#'
#' @param date Date to return the dataset for. The default is two months prior. Needs to be in the Date format.
#' @param api_key API key for LTA's Datamall
#' @return A link from which the user can download the CSV file.
#' @examples
#' getTrainPassengerVol_od(Sys.Date()-60, Sys.getenv('LTA_DATAMALL_KEY'))
#' @import httr
#' @export getTrainPassengerVol_od

getTrainPassengerVol_od <- function(date = Sys.Date()-60, api_key) {

  if (class(date) != 'Date') {
    stop("Please enter the date in the R Date format.")
  }

  request_date <- format(date, format = '%Y%m')
  print_date <- format(date, format = '%B %Y')

  params <- list(Date = request_date)
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/PV/ODTrain",
                httr::add_headers(AccountKey = api_key),
                query = params)

  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for train passenger volume (by origin and destination) data. Please check your parameters or connection.")
    return(NA)
  } else {
    message("Successful API call for train passenger volume (by origin and destination) data for ", print_date)
  }
  link <- unlist(httr::content(result)$value[[1]], use.names = FALSE)
  message('Link to download train passenger volume data (by origin and destination) available below. Note that it expires within 5 minutes.')
  return(link)
}
