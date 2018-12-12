#' @title Plot data on a static map of Singapore
#'
#' @description A simple ggmap wrapper which enables users to plot the data retrieved from the LTA APIs on a static map of Singapore. Some simple options are provided for ease of use. Note that this function only works automatically with outputs from getTaxiAvail, getCarparkAvail, getTrafficIncidents, getVMS, and getTrafficImages from this package. For all others, please rename the latitude and longitude columns to 'Latitude' and 'Longitude' respectively.
#'
#' @param dataframe The dataframe containing the latitude/longitude data
#' @param colour Colour of the markers - default is 'red'
#' @param size Size of the markers - default is 1
#' @param alpha Opacity of the markers - default is 0.5
#' @param darken How dark the base map  should be - default is 0
#' @return A static Singapore map with the coordinates plotted
#' @examples
#' \donttest{
#' mydata <- getTrafficImages(mykey)
#' plotSGMap(mydata, cluster = FALSE, colour = 'black', size = 7, alpha = 0.7)
#' }
#' @import ggmap
#' @import ggplot2
#' @export plotSGMap

plotSGMap <- function(dataframe, colour = 'red', size = 1, alpha = 0.5, darken = 0) {

  if (!any(grepl('\\blatitude\\b|\\blat\\b', colnames(dataframe), ignore.case = TRUE)) |
          !any(grepl('\\blongitude\\b|\\blng\\b', colnames(dataframe), ignore.case = TRUE))) {
    stop(message("Please label your latitude and longitude columns correctly as 'Latitude' and 'Longitude'."))
  }

  lat <- unlist(dataframe[grepl('\\blatitude\\b|\\blat\\b', colnames(dataframe), ignore.case = TRUE)], use.names=FALSE)
  lng <- unlist(dataframe[grepl('\\blongitude\\b|\\blng\\b', colnames(dataframe), ignore.case = TRUE)], use.names=FALSE)

  plot <- ggmap:: ggmap(ltaer::sg_map, darken=c(darken, "black")) +
            ggplot2:: geom_point(data = dataframe, ggplot2::aes(x = lng, y = lat), colour =  colour, size = size, alpha = alpha) +
            ggplot2:: xlab('') +
            ggplot2:: ylab('') +
            ggplot2:: theme(axis.line = ggplot2::element_blank(),
                            axis.text = ggplot2::element_blank(),
                            axis.ticks = ggplot2::element_blank(),
                            plot.margin = ggplot2::margin(0, 0, -1, -1, 'cm'))

  return(plot)
}
