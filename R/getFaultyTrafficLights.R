#' @title Faulty Traffic Lights
#'
#' @description Returns alerts of traffic lights that are either currently faulty or are undergoing scheduled maintenance.
#'
#' @param api_key API key for LTA's Datamall
#' @return A dataframe containing the alerts for each traffic light fault, such as the type of failure and start date-time
#' @examples
#' \donttest{
#' getFaultyTrafficLights(mykey)
#' }
#' @import httr
#' @export getFaultyTrafficLights

getFaultyTrafficLights <- function(api_key) {
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/FaultyTrafficLights",
                httr::add_headers(AccountKey = api_key))
  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for faulty traffic lights alerts. Please check your parameters or connection.")
    return(result)
  } else {
    message("Successful API call for faulty traffic lights alerts. Processing page 1...")
  }

  if (length(httr::content(result)$value) == 0) {
    return(message("API call successful. No faulty traffic lights alert at the present time."))
  }
  else {
    output <- data.frame(matrix(nrow = 0, ncol = 6))
    trafficlight_list <- httr::content(result)[[2]]
    for (i in 1:length(trafficlight_list)) {
      interim <- trafficlight_list[[i]]
      interim[sapply(interim, is.null)] <- 0
      output <- rbind(output, t(unlist(interim)))
    }
    names(output) <- names(unlist(trafficlight_list[[1]]))

    num <- 2
    while(length(trafficlight_list) > 499) {
      result <- httr::GET(paste("http://datamall2.mytransport.sg/ltaodataservice/FaultyTrafficLights", "?$skip=", (num-1)*500, sep=""),
                    httr::add_headers(AccountKey = api_key)
      )
      if (httr::http_status(result)$category != "Success") {
        message("Unsuccessful API call for faulty traffic lights alerts. Please check your parameters or connection.")
        return(result)
      } else {
        message("Successful API call for faulty traffic lights alerts. Processing page ", num, "...")
      }
      output0 <- data.frame(matrix(nrow = 0, ncol = 12))
      trafficlight_list <- httr::content(result)[[2]]
      for (i in 1:length(trafficlight_list)) {
        interim <- trafficlight_list[[i]]
        interim[sapply(interim, is.null)] <- 0
        output0 <- rbind(output0, t(unlist(interim)))
      }
      output <- rbind(output, output0)
      num <- num + 1
    }

    for (col in colnames(output)) {
      output[[col]] <- as.character(output[[col]])
    }

    message("API call successful. Number of alerts for faulty traffic lights: ", nrow(output))
    return(output)
  }
}
