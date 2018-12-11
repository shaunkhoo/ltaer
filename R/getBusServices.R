#' @title Bus Routes
#'
#' @description Returns information on all bus routes in Singapore that are currently in operation.
#'
#' @param api_key API key for LTA's Datamall
#' @return A dataframe containing all the outputs from the Bus Routes API, such as the operator, stop sequence, bus stop code, and distance travelled.
#' @examples
#' getBusRoutes(Sys.getenv('LTA_DATAMALL_KEY'))
#' @import httr
#' @import dplyr
#' @export getBusServices

getBusServices <- function(api_key) {
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/BusServices",
                httr::add_headers(AccountKey = api_key))
  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for bus services. Please check your parameters or connection.")
    return(result)
  } else {
    message("Successful API call for all bus services. Processing page 1...")
  }
  output <- data.frame(matrix(nrow = 0, ncol = 12))
  busservices_list <- content(result)[[2]]
  for (i in 1:length(busservices_list)) {
    interim <- busservices_list[[i]]
    interim[sapply(interim, is.null)] <- 0
    output <- rbind(output, t(unlist(interim)))
  }
  names(output) <- names(unlist(busservices_list[[1]]))
  num <- 2
  while(length(busservices_list) > 499) {
    result <- httr::GET(paste("http://datamall2.mytransport.sg/ltaodataservice/BusServices", "?$skip=", (num-1)*500, sep=""),
                  httr::add_headers(AccountKey = api_key)
    )
    if (httr::http_status(result)$category != "Success") {
      message("Unsuccessful API call for bus services. Please check your parameters or connection.")
      return(result)
    } else {
      message("Successful API call for all bus services. Processing page ", num, "...")
    }
    output0 <- data.frame(matrix(nrow = 0, ncol = 12))
    busservices_list <- httr::content(result)[[2]]
    for (i in 1:length(busservices_list)) {
      interim <- busservices_list[[i]]
      interim[sapply(interim, is.null)] <- 0
      output0 <- rbind(output0, t(unlist(interim)))
    }
    output <- rbind(output, output0)
    num <- num + 1
  }
  message('API call complete. Number of bus services returned: ', output %>% distinct(output$ServiceNo) %>% nrow())
  return(output)
}
