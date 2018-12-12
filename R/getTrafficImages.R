#' @title Traffic Images
#'
#' @description Returns links to images of live traffic conditions along Singapore's expressways and checkpoints with Malaysia.
#'
#' @param api_key API key for LTA's Datamall
#' @return A dataframe containing each camera's ID, location, and link to the image
#' @examples
#' \donttest{
#' getTrafficImages(mykey)
#' }
#' @import httr
#' @export getTrafficImages

getTrafficImages <- function(api_key) {
  result <- httr::GET("http://datamall2.mytransport.sg/ltaodataservice/Traffic-Images",
                httr::add_headers(AccountKey = api_key))

  if (httr::http_status(result)$category != "Success") {
    message("Unsuccessful API call for traffic images. Please check your parameters or connection.")
    return(result)
  } else {
    message("Successful API call for traffic images. Processing...")
  }

  output <- data.frame(matrix(nrow = 0, ncol = 4))
  trafficimage_list <- httr::content(result)[[2]]
  for (i in 1:length(trafficimage_list)) {
    interim <- trafficimage_list[[i]]
    interim[sapply(interim, is.null)] <- 0
    output <- rbind(output, t(unlist(interim)))
  }
  names(output) <- names(unlist(trafficimage_list[[1]]))

  num <- 2
  while(length(trafficimage_list) > 69) {
    result <- httr::GET(paste("http://datamall2.mytransport.sg/ltaodataservice/Traffic-Images", "?$skip=", (num-1)*70, sep=""),
                        httr::add_headers(AccountKey = api_key)
    )
    if (httr::http_status(result)$category != "Success") {
      message("Unsuccessful API call for traffic images. Please check your parameters or connection.")
      return(result)
    } else {
      message("Successful API call for traffic images. Processing page ", num, "...")
    }
    output0 <- data.frame(matrix(nrow = 0, ncol = 12))
    trafficimage_list <- httr::content(result)[[2]]
    for (i in 1:length(trafficimage_list)) {
      interim <- trafficimage_list[[i]]
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

  message("API call successful. Number of traffic images found: ", nrow(output))
  return(output)
}
