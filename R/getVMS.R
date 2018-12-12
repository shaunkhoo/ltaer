#' @title Traffic Speed Bands
#'
#' @description Returns traffic advisories about current traffic conditions that are displayed on EMAS signboards along expressways and major roads.
#'
#' @param api_key API key for LTA's Datamall
#' @return A dataframe containing each traffic advisory message and its coordinates
#' @examples
#' \donttest{
#' getVMS(mykey)
#' }
#' @import httr
#' @export getVMS

getVMS <- function(api_key) {
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/VMS",
                httr::add_headers(AccountKey = api_key))

  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for VMS/EMAS messages. Please check your parameters or connection.")
    return(result)
  } else {
    message("Successful API call for VMS/EMAS messages. Processing page 1...")
  }

  output <- data.frame(matrix(nrow = 0, ncol = 4))
  vms_list <- httr::content(result)[[2]]
  for (i in 1:length(vms_list)) {
    interim <- vms_list[[i]]
    interim[sapply(interim, is.null)] <- 0
    output <- rbind(output, t(unlist(interim)))
  }
  names(output) <- names(unlist(vms_list[[1]]))

  num <- 2
  while(length(vms_list) > 499) {
    result <- httr::GET(paste("http://datamall2.mytransport.sg/ltaodataservice/VMS", "?$skip=", (num-1)*500, sep=""),
                  httr::add_headers(AccountKey = api_key)
    )
    if (httr::http_status(result)$category != "Success") {
      message("Unsuccessful API call for VMS/EMAS messages. Please check your parameters or connection.")
      return(result)
    } else {
      message("Successful API call for VMS/EMAS messages. Processing page ", num, "...")
    }
    output0 <- data.frame(matrix(nrow = 0, ncol = 12))
    vms_list <- httr::content(result)[[2]]
    for (i in 1:length(vms_list)) {
      interim <- vms_list[[i]]
      interim[sapply(interim, is.null)] <- 0
      output0 <- rbind(output0, t(unlist(interim)))
    }
    output <- rbind(output, output0)
    num <- num + 1
  }

  for (col in colnames(output)) {
    output[[col]] <- as.character(output[[col]])
  }

  output$Latitude <- as.numeric(output$Latitude)
  output$Longitude <- as.numeric(output$Longitude)

  message("API call successful. Number of VMS/EMAS messages found: ", nrow(output))
  return(output)
}
