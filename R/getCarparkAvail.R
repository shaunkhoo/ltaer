#' @title Carpark Availability
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
#' @import dplyr
#' @import stringr
#' @export getCarparkAvail

getCarparkAvail <- function(api_key) {
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/CarParkAvailabilityv2",
                httr::add_headers(AccountKey = api_key))
  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for carpark availability data. Please check your parameters or connection.")
    return(result)
  } else {
    message("Successful API call for carpark availability data. Processing page 1")
  }

  output <- data.frame(matrix(nrow = 0, ncol = 7))
  carpark_list <- httr::content(result)[[2]]
  for (i in 1:length(carpark_list)) {
    interim <- carpark_list[[i]]
    interim[sapply(interim, is.null)] <- 0
    output <- rbind(output, t(unlist(interim)))
  }
  names(output) <- names(unlist(carpark_list[[1]]))

  num <- 2
  while(length(carpark_list) > 499) {
    result <- httr::GET(paste("http://datamall2.mytransport.sg/ltaodataservice/CarParkAvailabilityv2", "?$skip=", (num-1)*500, sep=""),
                  httr::add_headers(AccountKey = api_key)
    )
    if (httr::http_status(result)$category != "Success") {
      message("Unsuccessful API call for carpark availability data. Please check your parameters or connection.")
      return(result)
    } else {
      message("Successful API call for carpark availability data. Processing page ", num, "...")
    }
    output0 <- data.frame(matrix(nrow = 0, ncol = 7))
    carpark_list <- httr::content(result)[[2]]
    for (i in 1:length(carpark_list)) {
      interim <- carpark_list[[i]]
      interim[sapply(interim, is.null)] <- 0
      output0 <- rbind(output0, t(unlist(interim)))
    }
    output <- rbind(output, output0)
    num <- num + 1
  }
  output_final <- output %>%
    dplyr::mutate(lat = as.numeric(stringr::str_split(output$Location, "\\s", simplify=TRUE)[,1])) %>%
    dplyr::mutate(lng = as.numeric(stringr::str_split(output$Location, "\\s", simplify=TRUE)[,2]))
  output_final$Development  <- as.character(output_final$Development)
  output_final$AvailableLots <- as.numeric(as.character(output_final$AvailableLots))
  output_final$Agency <- as.character(output_final$Agency)
  output_final$LotType <- as.character(output_final$LotType)

  message('API call complete. Number of carparks returned: ', nrow(output_final))
  return(output_final)
}
