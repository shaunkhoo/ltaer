#' @title Bus Stops
#'
#' @description Returns information on all bus stops in Singapore that are currently in operation.
#'
#' @param api_key API key for LTA's Datamall
#' @return A dataframe containing all the outputs from the Bus Stops API, such as the bus stop code, road name, description, and coordinates.
#' @examples
#' \donttest{
#' getBusStops(Sys.getenv('LTA_DATAMALL_KEY'))
#' }
#' @import httr
#' @export getBusStops

getBusStops <- function(api_key) {
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/BusStops",
                      httr::add_headers(AccountKey = api_key)
  )
  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for bus stops. Please check your parameters or connection.")
    return(NA)
  } else {
    message("Successful API call for all bus stops. Processing page 1...")
  }
  output <- data.frame(matrix(nrow = 0, ncol = 12))
  busstops_list <- httr::content(result)[[2]]
  for (i in 1:length(busstops_list)) {
    interim <- busstops_list[[i]]
    interim[sapply(interim, is.null)] <- 0
    output <- rbind(output, t(unlist(interim)))
  }
  names(output) <- names(unlist(busstops_list[[1]]))
  num <- 2
  while(length(busstops_list) > 499) {
    result <- httr::GET(paste("http://datamall2.mytransport.sg/ltaodataservice/BusStops", "?$skip=", (num-1)*500, sep=""),
                        httr::add_headers(AccountKey = api_key)
    )
    if (httr::http_status(result)$category != "Success") {
      message("Unsuccessful API call for bus stops. Please check your parameters or connection.")
      return(NA)
    } else {
      message("Successful API call for all bus stops. Processing page ", num, "...")
    }
    output0 <- data.frame(matrix(nrow = 0, ncol = 12))
    busstops_list <- httr::content(result)[[2]]
    for (i in 1:length(busstops_list)) {
      interim <- busstops_list[[i]]
      interim[sapply(interim, is.null)] <- 0
      output0 <- rbind(output0, t(unlist(interim)))
    }
    output <- rbind(output, output0)
    num <- num + 1
  }

  output$BusStopCode <- as.character(output$BusStopCode)
  output$Latitude <- as.numeric(as.character(output$Latitude))
  output$Longitude <- as.numeric(as.character(output$Longitude))
  output$Description <- as.character(output$Description)
  output$RoadName <- as.character(output$RoadName)

  message('API call complete. Number of bus stops returned: ', nrow(output))
  return(output)
}
