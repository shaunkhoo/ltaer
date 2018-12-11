#' @title Taxi Availability
#'
#' @description Returns the coordinates for all taxis that are currently available for hire. Does not include 'hired' or 'busy' taxis.
#'
#' @param api_key API key for LTA's Datamall
#' @return A dataframe containing the latitude and longitude information for all the taxis available for hire.
#' @examples
#' \donttest{
#' getTaxiAvail(Sys.getenv('LTA_DATAMALL_KEY'))
#' }
#' @import httr
#' @export getTaxiAvail

getTaxiAvail <- function(api_key) {

  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/Taxi-Availability",
                httr::add_headers(AccountKey = api_key))
  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for taxi availability data. Please check your parameters or connection.")
    return(NA)
  } else {
    message("Successful API call for taxi availability data. Processing page 1")
  }

  output <- data.frame(matrix(nrow = 0, ncol = 3))
  taxis_list <- httr::content(result)[[2]]
  for (i in 1:length(taxis_list)) {
    interim <- taxis_list[[i]]
    interim[sapply(interim, is.null)] <- 0
    output <- rbind(output, t(unlist(interim)))
  }
  names(output) <- names(unlist(taxis_list[[1]]))

  num <- 2
  while(length(taxis_list) > 499) {
    result <- httr::GET(paste("http://datamall2.mytransport.sg/ltaodataservice/Taxi-Availability", "?$skip=", (num-1)*500, sep=""),
                  httr::add_headers(AccountKey = api_key)
    )
    if (httr::http_status(result)$category != "Success") {
      message("Unsuccessful API call for taxi availability data. Please check your parameters or connection.")
      return(result)
    } else {
      message("Successful API call for taxi availability data. Processing page ", num, "...")
    }
    output0 <- data.frame(matrix(nrow = 0, ncol = 12))
    taxis_list <- httr::content(result)[[2]]
    for (i in 1:length(taxis_list)) {
      interim <- taxis_list[[i]]
      interim[sapply(interim, is.null)] <- 0
      output0 <- rbind(output0, t(unlist(interim)))
    }
    output <- rbind(output, output0)
    num <- num + 1
  }
  output$Longitude <- as.numeric(as.character(output$Longitude))
  output$Latitude <- as.numeric(as.character(output$Latitude))
  message('API call complete. Number of active taxis: ', nrow(output))
  return(output)
}
