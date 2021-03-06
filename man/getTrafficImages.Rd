% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getTrafficImages.R
\name{getTrafficImages}
\alias{getTrafficImages}
\title{Traffic Images}
\usage{
getTrafficImages(api_key)
}
\arguments{
\item{api_key}{API key for LTA's Datamall}
}
\value{
A dataframe containing each camera's ID, location, and link to the image
}
\description{
Returns links to images of live traffic conditions along Singapore's expressways and checkpoints with Malaysia.
}
\examples{
\donttest{
getTrafficImages(mykey)
}
}
