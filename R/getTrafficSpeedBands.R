#' @title Traffic Speed Bands
#'
#' @description Returns current traffic speeds on expressways and major roads, expressed in speed bands
#'
#' @param api_key API key for LTA's Datamall
#' @return A dataframe containing each stretch of road, speed bands, and location data
#' @examples
#' \donttest{
#' getTrafficSpeedBands(mykey)
#' }
#' @import httr
#' @export getTrafficSpeedBands

getTrafficSpeedBands <- function(api_key) {
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/TrafficSpeedBandsv2",
                httr::add_headers(AccountKey = api_key))

  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for traffic speed bands. Please check your parameters or connection.")
    return(result)
  } else {
    message("Successful API call for traffic speed bands. Processing page 1...")
  }

  output <- data.frame(matrix(nrow = 0, ncol = 4))
  trafficspeedband_list <- httr::content(result)[[2]]
  for (i in 1:length(trafficspeedband_list)) {
    interim <- trafficspeedband_list[[i]]
    interim[sapply(interim, is.null)] <- 0
    output <- rbind(output, t(unlist(interim)))
  }
  names(output) <- names(unlist(trafficspeedband_list[[1]]))

  num <- 2
  while(length(trafficspeedband_list) > 499) {
    result <- httr::GET(paste("http://datamall2.mytransport.sg/ltaodataservice/TrafficSpeedBandsv2", "?$skip=", (num-1)*500, sep=""),
                  httr::add_headers(AccountKey = api_key)
    )
    if (httr::http_status(result)$category != "Success") {
      message("Unsuccessful API call for traffic speed bands. Please check your parameters or connection.")
      return(result)
    } else {
      message("Successful API call for traffic speed bands. Processing page ", num, "...")
    }
    output0 <- data.frame(matrix(nrow = 0, ncol = 12))
    trafficspeedband_list <- httr::content(result)[[2]]
    for (i in 1:length(trafficspeedband_list)) {
      interim <- trafficspeedband_list[[i]]
      interim[sapply(interim, is.null)] <- 0
      output0 <- rbind(output0, t(unlist(interim)))
    }
    output <- rbind(output, output0)
    num <- num + 1
  }

  for (col in colnames(output)) {
    output[[col]] <- as.character(output[[col]])
  }

  message("API call complete. Number of traffic speed bands found: ", nrow(output))
  return(output)
}
