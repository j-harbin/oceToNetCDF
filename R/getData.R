#' Get data for specified instrument type
#'
#' This function gets the available standard names, units, codes,names, and
#' type of data for the required information for the specified instrument type
#'
#' @param type the type of data that will eventually be converted into a NetCDF.
#' The options are ctd, rcm, or adcp
#'
#' @return a data frame containing the necessary standard names, units, codes, names, and type of data
#'
#' @example
#' data <- getdata(type="core")
#' names(data)
#' @export

getData <- function(type="ctd") {
  if (!(type %in% c("ctd", "rcm", "adcp"))) {
    stop("getData can only work for data type ctd, rcm, or adcp")
  }

  if (type == "ctd") {
    DF <- data.frame("code"= c("CNDC", "POTM", "PRES", "PSAL", "SIGP", "SYTM", "TEMP"),
                     "name"= c("Electrical Conductivity","Potential Temperature","Sea Pressure (sea surface - 0)",
                               "Practical Salinity","Sigma-Theta", "PIPE Time Format DD-MMM-YYYY HH:MM:SS.ss",
                               " Sea Temperature"),
                     "units"=c("S/m", "degrees C", "dbar","1","kg/m**3","s","degree_C"),
                     "standard_name"= c("sea_water_electrical_conductivity","sea_water_potential_temperature",
                                        "sea_water_pressure", "sea_water_practical_salinity","sea_water_sigma_theta",
                                        "time", "sea_water_temperature"),
                     "type"=rep("ctd", 7))
  } else if (type == "rcm") {
    DF <- data.frame("code"= c("HCDT", "HCSP", "PRES", "PSAL", "SYTM", "TEMP"),
                     "name"= c("Horizontal Current Direction (true)","Horizontal Current Speed",
                               "Sea Pressure (sea surface - 0)","Practical Salinity",
                               "PIPE Time Format DD-MMM-YYYY HH:MM:SS.ss", "Sea Temperature"),
                     "units"=c("degrees", "m/s","dbar","1","s","degree_C"),
                     "standard_name"=c("horizontal_current_direction","barotropic_sea_water_x_velocity",
                                       "sea_water_pressure","sea_water_practical_salinity","time","sea_water_temperature"),
                     "type"=rep("rcm", 6))
  }


DF

}
