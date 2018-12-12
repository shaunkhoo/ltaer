#' @title Passenger Volume by Origin/Destination Bus Stops
#'
#' @description Returns a link from LTA that enables the user to download a CSV file containing a month's worth of data on passenger volume by origin/destination bus stops.
#'
#' @param date Date to return the dataset for. The default is two months prior. Needs to be in the Date format.
#' @param api_key API key for LTA's Datamall
#' @return A link from which the user can download the CSV file.
#' @examples
#' \donttest{
#' getBusPassengerVol_od(Sys.Date()-60, mykey)
#' }
#' @import httr
#' @export getBusPassengerVol_od

getBusPassengerVol_od <- function(date = Sys.Date()-60, api_key) {

  if (class(date) != 'Date') {
    stop("Please enter the date in the R Date format.")
  }

  request_date <- format(date, format = '%Y%m')
  print_date <- format(date, format = '%B %Y')

  params <- list(Date = request_date)
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/PV/ODBus",
                httr::add_headers(AccountKey = api_key),
                query = params)

  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for bus passenger volume data (by origin and destination). Please check your parameters or connection.")
    return(result)
  } else {
    message("Successful API call for bus passenger volume data (by origin and destination) for ", print_date)
  }
  link <- unlist(httr::content(result)$value[[1]], use.names = FALSE)
  message('API call complete. Link to download bus passenger volume data (by origin and destination) available below. Note that it expires within 5 minutes.')
  return(link)
}
