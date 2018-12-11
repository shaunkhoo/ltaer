#' @title Bus Timings
#'
#' @description Returns real-time estimated bus arrival timings for the specified bus stops in Singapore. Note that this function may not return anything between 1.00am and 5.00am SG time (GMT+8)
#'
#' @param codes A character vector of bus stop codes
#' @param api_key API key for LTA's Datamall
#' @return A dataframe containing all the outputs from the Bus Timings API, including the service number, estimated arrival time, load, and type for the next three buses.
#' @examples
#' \donttest{
#' getBusTimings(c('11111', '22222', '33333'), Sys.getenv('LTA_DATAMALL_KEY'))
#' }
#' @import httr
#' @export getBusTimings

getBusTimings <- function(codes = c('07351', '09047'), api_key) {

  # check to ensure inputs are correctly entered by the user

  if(any(!is.character(codes))) {
    stop("Error: Bus stop codes must be enclosed in quotation marks.")
  }

  if(any(is.na(codes))) {
    stop("Error: Bus stop codes must not contain any NAs.")
  }

  ## Helper functions go here:

  json_retrieving <- function(GET_result) {
    if (length(httr::content(GET_result)[3][[1]])>0) {
      output <- httr::content(GET_result)[3][[1]]
    } else {
      code <- httr::content(GET_result)$BusStopCode
      message <- paste('No bus services at bus stop number ', code, ' at this timing.', sep='')
      message(message)
      return(NULL)
    }
    return(output)
  }

  json_processing <- function(timing_list, code) {
    output <- data.frame(matrix(NA, nrow = length(timing_list), ncol = 29))
    names(output) <- gsub(".", "_", names(unlist(timing_list[1])), fixed=TRUE)

    for (i in 1:length(timing_list)) {
      output[i,] <- unlist(timing_list[[i]], use.names=FALSE)
    }
    output[output==''] <- NA
    output$BusStopCode <- code
    output <- output[,c(30,1:29)]
    return(output)
  }

  output <- data.frame(matrix(nrow = 0, ncol = 30))
  for (bus_stop in codes) {
    params <- list(BusStopCode = bus_stop)
    result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/BusArrivalv2",
                  httr::add_headers(
                    AccountKey = api_key,
                    accept = 'application/json'),
                  query = params)
    if (httr::http_status(result)$category != "Success") {
      message("Unsuccessful API call for ", bus_stop, ". Please check your parameters or connection.")
      return(NA)
    } else {
      message("Successful API call for ", bus_stop, ".")
    }
    timings0 <- json_retrieving(result)
    if (!is.null(timings0)) {
      output0 <- json_processing(timings0, bus_stop)
      output <- rbind(output, output0)
    } else {next}
  }
  message('API call complete. Number of bus timings returned: ', nrow(output))
  return(output)
}
