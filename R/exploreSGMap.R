#' @title Explore data on an interactive map of Singapore
#'
#' @description A simple Leaflet wrapper which enables users to plot the data retrieved from the LTA APIs on an interactive map of Singapore. Some basic options are provided for ease of use. Note that this function only works automatically with outputs from getTaxiAvail, getCarparkAvail, getTrafficIncidents, getVMS, and getTrafficImages from this package. For all others, please obtain data for the coordinates and rename the respective columns to 'Latitude' and 'Longitude'.
#'
#' @param dataframe The dataframe containing the latitude/longitude data
#' @param popup Variable to be displayed when a marker is clicked on - default is NA, needs to be a string of the variable name
#' @param cluster Whether to cluster the markers or not - default is FALSE
#' @param colour Colour of the markers - default is 'red'
#' @param size Size of the markers - default is 5
#' @param alpha Opacity of the markers - default is 0.5
#' @return An interactive map of Singapore with the coordinates plotted as circular markers on the map
#' @examples
#' \donttest{
#' mydata <- getTrafficImages(mykey)
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
                     fillColor = colour,
                     opacity = alpha,
                     fillOpacity = alpha,
                     radius = size,
                     weight = size,
                     clusterOptions = leaflet::markerClusterOptions(),
                     popup = ~htmltools::htmlEscape(popup_var))
  } else if (cluster == TRUE & is.na(popup)) {
    map %>%
      leaflet::addCircleMarkers(color = colour,
                       fillColor = colour,
                       opacity = alpha,
                       fillOpacity = alpha,
                       radius = size,
                       weight = size,
                       clusterOptions = leaflet::markerClusterOptions())
  } else if (cluster == FALSE & !is.na(popup)) {
    popup_var <- dataframe[[popup]]
    map %>%
      leaflet::addCircleMarkers(color = colour,
                                fillColor = colour,
                                 radius = size,
                                 weight = size,
                                 opacity = alpha,
                                 fillOpacity = alpha,
                                 popup = ~htmltools::htmlEscape(popup_var))
  } else if (cluster == FALSE & is.na(popup)) {
    map %>%
      leaflet::addCircleMarkers(color = colour,
                                fillColor = colour,
                                opacity = alpha,
                                fillOpacity = alpha,
                                radius = size,
                                weight = size)
  } else message("Error: Inputs are not defined correctly.")
}
