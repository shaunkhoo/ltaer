#' @title Explore data on an interactive map of Singapore
#'
#' @description A simple Leaflet wrapper which enables users to plot the data retrieved from the LTA APIs on an interactive map of Singapore. Some simple options are provided for ease of use. Note that this function only works automatically with outputs from getTaxiAvail, getCarparkAvail, getTrafficIncidents, getVMS, and getTrafficImages from this package. For all others, please rename the latitude and longitude columns to 'Latitude' and 'Longitude' respectively.
#'
#' @param dataframe The dataframe containing the latitude/longitude data
#' @param popup Name of the variable in the dataframe to be displayed when a marker is clicked on - default is NA
#' @param cluster Whether to cluster the markers or not - default is FALSE
#' @param colour Colour of the markers - default is 'red'
#' @param size Size of the markers - default is 5
#' @param alpha Opacity of the markers - default is 0.5
#' @return A dataframe containing each carpark's information and number of available lots
#' @examples
#' \donttest{
#' mydata <- getTrafficImages(Sys.getenv('LTA_DATAMALL_KEY'))
#' exploreSGMap(mydata, cluster = FALSE, colour = 'black', size = 7, alpha = 0.7)
#' }
#' @import leaflet
#' @import htmltools
#' @import dplyr
#' @export exploreSGMap

exploreSGMap <- function(dataframe, popup = NA, cluster = FALSE, colour = "red", size = 5, alpha = 0.5) {
  requireNamespace("leaflet", quietly = TRUE)
  requireNamespace("htmltools", quietly = TRUE)
  if (!is.na(popup) & !(popup %in% colnames(dataframe))) {
    stop("Variable ", popup, " not found in dataframe. Please specify it correctly.")
  }

  map <- dataframe %>%
    leaflet::leaflet() %>%
    leaflet::addTiles()
  if (cluster == TRUE & !is.na(popup)) {
    popup_var <- dataframe[[popup]]
    map %>%
      leaflet::addCircleMarkers(color = colour,
                     fillColor = color,
                     opacity = alpha,
                     fillOpacity = alpha,
                     radius = size,
                     weight = size,
                     clusterOptions = leaflet::markerClusterOptions(),
                     popup = ~htmltools::htmlEscape(popup_var))
  } else if (cluster == TRUE & is.na(popup)) {
    map %>%
      leaflet::addCircleMarkers(color = colour,
                       fillColor = color,
                       opacity = alpha,
                       fillOpacity = alpha,
                       radius = size,
                       weight = size,
                       clusterOptions = leaflet::markerClusterOptions())
  } else if (cluster == FALSE & !is.na(popup)) {
    popup_var <- dataframe[[popup]]
    map %>%
      leaflet::addCircleMarkers(color = colour,
                                 radius = size,
                                 weight = size,
                                 opacity = alpha,
                                 fillOpacity = alpha,
                                 popup = ~htmltools::htmlEscape(popup_var))
  } else if (cluster == FALSE & is.na(popup)) {
    map %>%
      leaflet::addCircleMarkers(color = colour,
                                opacity = alpha,
                                fillOpacity = alpha,
                                radius = size,
                                weight = size)
  } else message("Error: Inputs are not defined correctly.")
}
