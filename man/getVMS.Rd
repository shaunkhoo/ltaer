% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getVMS.R
\name{getVMS}
\alias{getVMS}
\title{Traffic Speed Bands}
\usage{
getVMS(api_key)
}
\arguments{
\item{api_key}{API key for LTA's Datamall}
}
\value{
A dataframe containing each traffic advisory message and its coordinates
}
\description{
Returns traffic advisories about current traffic conditions that are displayed on EMAS signboards along expressways and major roads.
}
\examples{
\donttest{
getVMS(mykey)
}
}
