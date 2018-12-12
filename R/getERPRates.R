#' @title ERP Rates
#'
#' @description Provides the ERP rates for all vehicle types across all timings and all zones.
#'
#' @param api_key API key for LTA's Datamall
#' @return A dataframe containing ERP rates for each vehicle type, ERP rate timing, and zone.
#' @examples
#' \donttest{
#' getERPRates(mykey)
#' }
#' @import httr
#' @export getERPRates

getERPRates <- function(api_key) {
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/ERPRates",
                httr::add_headers(AccountKey = api_key))
  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for ERP rates data. Please check your parameters or connection.")
    return(result)
  } else {
    message("Successful API call for ERP rates data. Processing page 1...")
  }

  output <- data.frame(matrix(nrow = 0, ncol = 7))
  erp_list <- httr::content(result)[[2]]
  for (i in 1:length(erp_list)) {
    interim <- erp_list[[i]]
    interim[sapply(interim, is.null)] <- 0
    output <- rbind(output, t(unlist(interim)))
  }
  names(output) <- names(unlist(erp_list[[1]]))

  num <- 2
  while(length(erp_list) > 499) {
    result <- httr::GET(paste("http://datamall2.mytransport.sg/ltaodataservice/ERPRates", "?$skip=", (num-1)*500, sep=""),
                  httr::add_headers(AccountKey = api_key))
    if (httr::http_status(result)$category != "Success") {
      message("Unsuccessful API call for ERP rates data. Please check your parameters or connection.")
      return(result)
    } else {
      message("Successful API call for ERP rates data. Processing page ", num, "...")
    }
    output0 <- data.frame(matrix(nrow = 0, ncol = 7))
    erp_list <- httr::content(result)[[2]]
    for (i in 1:length(erp_list)) {
      interim <- erp_list[[i]]
      interim[sapply(interim, is.null)] <- 0
      output0 <- rbind(output0, t(unlist(interim)))
    }
    output <- rbind(output, output0)
    num <- num + 1
  }

  for (col in colnames(output)) {
    output[[col]] <- as.character(output[[col]])
  }

  message('API call completed! Number of ERP rates: ', nrow(output))
  return(output)
}
