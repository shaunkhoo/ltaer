#' @title Train Service Alerts
#'
#' @description Returns alerts on train service unavailability during scheduled operating hours.
#'
#' @param api_key API key for LTA's Datamall
#' @return If there is a train service disruption, it will return a dataframe containing the alerts and their respective details.
#' @examples
#' \donttest{
#' getTrainAlerts(Sys.getenv('LTA_DATAMALL_KEY'))
#' }
#' @import httr
#' @export getTrainAlerts

## NOTE THAT THIS FUNCTION REMAINS UNTESTED DUE TO LACK OF ALERTS

getTrainAlerts <- function(api_key) {
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/TrainServiceAlerts",
                httr::add_headers(AccountKey = api_key))
  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for train service alerts data. Please check your parameters or connection.")
    return(result)
  } else {
    message("Successful API call for train service alerts data. Processing...")
  }
  if (httr::content(result)$value$Status == 1) {
    return(message("Normal train service reported. No service alerts available."))
  } else {
    response_list <- httr::content(result)$value
    output <- data.frame(matrix(nrow = 0, ncol = 8))
    for (i in 1:length(response_list)) {
      interim <- response_list[[i]]
      interim[sapply(interim, is.null)] <- 0
      output <- rbind(output, t(unlist(interim)))
    }
    names(output) <- names(unlist(response_list[[1]]))
  }
  return(output)
}
