#' @title Road Works
#'
#' @description Returns all road works currently being carried out or scheduled to be carried out.
#'
#' @param api_key API key for LTA's Datamall
#' @return A dataframe containing the details for each current or planned road work, such as the start date, end date, and road name
#' @examples
#' getRoadWorks(Sys.getenv('LTA_DATAMALL_KEY'))
#' @import httr
#' @export getRoadWorks

getRoadWorks <- function(api_key) {
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/RoadWorks",
                httr::add_headers(AccountKey = api_key))

  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for road works data. Please check your parameters or connection.")
    return(result)
  } else {
    message("Successful API call for road works data. Processing page 1...")
  }

  if (length(httr::content(result)$value) == 0) {
    message("API call successful. No scheduled road works at the present time.")
    return(httr::content(result))
  } else {
    output <- data.frame(matrix(nrow = 0, ncol = 6))
    roadworks_list <- httr::content(result)[[2]]
    for (i in 1:length(roadworks_list)) {
      interim <- roadworks_list[[i]]
      interim[sapply(interim, is.null)] <- 0
      output <- rbind(output, t(unlist(interim)))
    }
    names(output) <- names(unlist(roadworks_list[[1]]))

    num <- 2
    while(length(roadworks_list) > 499) {
      result <- httr::GET(paste("http://datamall2.mytransport.sg/ltaodataservice/RoadWorks", "?$skip=", (num-1)*500, sep=""),
                    httr::add_headers(AccountKey = api_key)
      )
      if (httr::http_status(result)$category != "Success") {
        message("Unsuccessful API call for road works data. Please check your parameters or connection.")
        return(result)
      } else {
        message("Successful API call for road works data. Processing page ", num, "...")
      }
      output0 <- data.frame(matrix(nrow = 0, ncol = 12))
      roadworks_list <- httr::content(result)[[2]]
      for (i in 1:length(roadworks_list)) {
        interim <- roadworks_list[[i]]
        interim[sapply(interim, is.null)] <- 0
        output0 <- rbind(output0, t(unlist(interim)))
      }
      output <- rbind(output, output0)
      num <- num + 1
    }

    message("API call successful. Number of road works found: ", nrow(output))
    return(output)
  }
}
