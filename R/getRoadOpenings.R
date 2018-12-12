#' @title Road Openings
#'
#' @description Returns all planned road openings.
#'
#' @param api_key API key for LTA's Datamall
#' @return A dataframe containing the details for each planned road opening, such as the start date, end date, and new road name
#' @examples
#' \donttest{
#' getRoadOpenings(mykey)
#' }
#' @import httr
#' @export getRoadOpenings

getRoadOpenings <- function(api_key) {
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/RoadOpenings",
                httr::add_headers(AccountKey = api_key))
  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for road openings data. Please check your parameters or connection.")
    return(result)
  } else {
    message("Successful API call for road openings data. Processing page 1...")
  }

  if (length(httr::content(result)$value) == 0) {
    return(message("API call successful. No scheduled road openings at the present time."))
  } else {
    output <- data.frame(matrix(nrow = 0, ncol = 6))
    roadopenings_list <- httr::content(result)[[2]]
    for (i in 1:length(roadopenings_list)) {
      interim <- roadopenings_list[[i]]
      interim[sapply(interim, is.null)] <- 0
      output <- rbind(output, t(unlist(interim)))
    }
    names(output) <- names(unlist(roadopenings_list[[1]]))

    num <- 2
    while(length(roadopenings_list) > 499) {
      result <- httr::GET(paste("http://datamall2.mytransport.sg/ltaodataservice/RoadOpenings", "?$skip=", (num-1)*500, sep=""),
                    httr::add_headers(AccountKey = api_key)
      )
      if (httr::http_status(result)$category != "Success") {
        message("Unsuccessful API call for road openings data. Please check your parameters or connection.")
        return(result)
      } else {
        message("Successful API call for road openings data. Processing page ", num, "...")
      }
      output0 <- data.frame(matrix(nrow = 0, ncol = 12))
      roadopenings_list <- httr::content(result)[[2]]
      for (i in 1:length(roadopenings_list)) {
        interim <- roadopenings_list[[i]]
        interim[sapply(interim, is.null)] <- 0
        output0 <- rbind(output0, t(unlist(interim)))
      }
      output <- rbind(output, output0)
      num <- num + 1
    }

    for (col in colnames(output)) {
      output[[col]] <- as.character(output[[col]])
    }

    message("API call successful. Number of scheduled road openings found: ", nrow(output))
    return(output)
  }
}
