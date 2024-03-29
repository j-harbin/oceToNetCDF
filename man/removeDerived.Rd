% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removeDerived.R
\name{removeDerived}
\alias{removeDerived}
\title{Remove derived ctd and rcm data and metadata}
\usage{
removeDerived(odf, debug = 0)
}
\arguments{
\item{odf}{an odf object (oce::read.odf())}

\item{debug}{integer value indicating level of debugging.
If this is less than 1, no debugging is done. Otherwise,
some functions will print debugging information}
}
\value{
CTD object: an odf object with only data for time, conductivity, salinity,
temperature, and pressure. RCM object: an odf object with only data for horizontal_current_direction,
horizontal_current_speed, sea_water_pressure, sea_water_practical_salinity", "time",
and sea_water_temperature
}
\description{
This function removes data and metadata that is derived.
For a CTD type, the only data and metadata kept is time, conductivity, salinity,
temperature, and pressure. For an RCM type, the only data and
metadata kept is horizontal_current_direction,
horizontal_current_speed, sea_water_pressure, sea_water_practical_salinity,
time, and sea_water_temperature
}
\examples{
library(oceToNetCDF)
library(oce)
data <- getStandardData(type="ctd")
f <- system.file("extdata", "mctd.ODF", package="oceToNetCDF")
odf1 <- read.odf(f)
odf2 <- nameReplacement(odf1, data=data, unit="S/m")
odf2 <- oceSetMetadata(object=odf2,name="mooringType", value="mctd")
odf3 <- removeDerived(odf2)
}
