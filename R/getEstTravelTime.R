#' @title Estimated Travel Times
#'
#' @description Provides key details and available lots for HDB, LTA, and URA carparks.
#'
#' @param api_key API key for LTA's Datamall
#' @return A dataframe containing each carpark's information and number of available lots
#' @examples
#' \donttest{
#' getCarparkAvail(Sys.getenv('LTA_DATAMALL_KEY'))
#' }
#' @import httr
#' @export getEstTravelTime

getEstTravelTime <- function(api_key) {
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/EstTravelTimes",
                httr::add_headers(AccountKey = api_key))

  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for estimated travel times of expressways. Please check your parameters or connection.")
    return(result)
  } else {
    message("Successful API call for estimated travel times of expressways. Processing...")
  }

  output <- data.frame(matrix(nrow = 0, ncol = 6))
  carpark_list <- httr::content(result)[[2]]
  for (i in 1:length(carpark_list)) {
    interim <- carpark_list[[i]]
    interim[sapply(interim, is.null)] <- 0
    output <- rbind(output, t(unlist(interim)))
  }
  names(output) <- names(unlist(carpark_list[[1]]))
  message("API call successful. Number of estimated travel times found: ", nrow(output))
  return(output)
}
