#' @title Traffic Incidents
#'
#' @description Returns incidents currently happening on the roads, such as accidents, vehicle breakdowns etc.
#'
#' @param api_key API key for LTA's Datamall
#' @return A dataframe containing each traffic incident, its type, coordinates, and description message
#' @examples
#' \donttest{
#' getTrafficIncidents(Sys.getenv('LTA_DATAMALL_KEY'))
#' }
#' @import httr
#' @export getTrafficIncidents

getTrafficIncidents <- function(api_key) {
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/TrafficIncidents",
                httr::add_headers(AccountKey = api_key))

  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for traffic incidents. Please check your parameters or connection.")
    return(result)
  } else {
    message("Successful API call for traffic incidents. Processing page 1...")
  }

  output <- data.frame(matrix(nrow = 0, ncol = 4))
  trafficincident_list <- httr::content(result)[[2]]
  for (i in 1:length(trafficincident_list)) {
    interim <- trafficincident_list[[i]]
    interim[sapply(interim, is.null)] <- 0
    output <- rbind(output, t(unlist(interim)))
  }
  names(output) <- names(unlist(trafficincident_list[[1]]))

  num <- 2
  while(length(trafficincident_list) > 499) {
    result <- httr::GET(paste("http://datamall2.mytransport.sg/ltaodataservice/TrafficIncidents", "?$skip=", (num-1)*500, sep=""),
                  httr::add_headers(AccountKey = api_key)
    )
    if (httr::http_status(result)$category != "Success") {
      message("Unsuccessful API call for traffic incidents. Please check your parameters or connection.")
      return(result)
    } else {
      message("Successful API call for traffic incidents. Processing page ", num, "...")
    }
    output0 <- data.frame(matrix(nrow = 0, ncol = 12))
    trafficincident_list <- httr::content(result)[[2]]
    for (i in 1:length(trafficincident_list)) {
      interim <- trafficincident_list[[i]]
      interim[sapply(interim, is.null)] <- 0
      output0 <- rbind(output0, t(unlist(interim)))
    }
    output <- rbind(output, output0)
    num <- num + 1
  }
  output$Type <- as.character(output$Type)
  output$Message <- as.character(output$Message)
  output$Latitude <- as.numeric(as.character(output$Latitude))
  output$Longitude <- as.numeric(as.character(output$Longitude))
  message("API call successful. Number of traffic incidents found: ", nrow(output))
  return(output)
}
