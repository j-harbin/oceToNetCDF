#' Get required data for specified instrument type
#'
#' This function gets the available standard names, units, codes,names, and
#' type of data for the required information for the specified instrument type
#' of either ctd, rcm, or adcp.
#'
#' @param type the type of data that will eventually be converted into a netCDF.
#' The options are ctd, rcm, or adcp
#'
#' @param standard parameter equal to either 'cf' (Climate or Forecast) or "bodc"
#' British Oceanographic Data Center) indicating which standard data to obtain
#'
#' @param silent logical value indicating whether to silence some CF compliant warnings. The default is to silent such indicators.
#'
#' @return a data frame containing the necessary standard names, units, codes, names, and type of data
#'
#' @examples
#' data <- getStandardData(type="ctd", standard="cf")
#' names(data)
#' @export

getStandardData <- function(type="ctd", standard="cf", silent=TRUE) {
  if (!(type %in% c("ctd", "rcm", "adcp"))) {
    stop("can only work for data type ctd, rcm, or adcp")
  }

  if (!(standard %in% c("cf", "bodc"))) {
    stop("can only obtain standards for cf and bodc")
  }

  if (type == "ctd") {
    DF <- data.frame("code"= c("CNDC", "CRAT", "POTM", "PRES", "PSAL", "SIGP", "SIGT", "SYTM", "TEMP", "TE90", "DOXY", "FLOR"),
                     "name"= c(paste("Electrical", "Conductivity"), paste("Conductivity", "Ratio"), paste("Potential", "Temperature"),paste("Sea", "Pressure", "(sea surface - 0)"),
                               paste("Practical", "Salinity"),"Sigma-Theta", "Sigma-Theta", paste("PIPE", "Time", "Format", "DD-MMM-YYYY", "HH:MM:SS.ss"),
                               paste("Sea", "Temperature"), paste("Sea", "Temperature"), paste("Dissolved", "Oxygen"), "Fluorescence"),
                     "units"=c("S/m", "", "degrees C", "dbar","1","kg/m**3", "kg/m**3","s","degree_C", "degree_C", "mL/L", "mg/m**3"),
                     "standard_name"= c("sea_water_electrical_conductivity","sea_water_electrical_conductivity_ratio", "sea_water_potential_temperature",
                                        "sea_water_pressure", "sea_water_practical_salinity","sea_water_sigma_theta", "sea_water_sigma_theta", "time", "sea_water_temperature",
                                        "sea_water_temperature", "sea_water_dissolved_oxygen", "sea_water_fluorescence"),
                     "type"=rep("ctd", 12))
  if (silent == FALSE) {
      warning("There is no DFO code or standard_name for Fluorescence. FLOR and sea_water_fluorescence are used respectively.")
  }
  if (standard == "bodc") {
      DF$bodc=c("CNDCZZ01","CRAT", "POTM", "PRESPR01",
          "PSLTZZ01", "SIGTEQ01", "SIGTEQST", "SYTM", "TEMPPR01",
          "TEMPP901", "DOXYZZ01", "FLOR")
      if (silent == FALSE) {
          warning("There were no BODC standards for CRAT, POTM, SYTM, or FLOR. DFO codes are used.")
      }

                       }

  } else if (type == "rcm") {
    DF <- data.frame("code"= c("HCDT", "HCSP", "PRES", "PSAL", "CRAT", "SYTM", "TEMP", "EWCT", "NSCT"),
                     "name"= c(paste("Horizontal", "Current", "Direction (true)"),paste("Horizontal", "Current", "Speed"),
                               paste("Sea", "Pressure", "(sea surface - 0)"),paste("Practical", "Salinity"),
                               paste("Conductivity", "Ratio"),
                               paste("PIPE", "Time", "Format", "DD-MMM-YYYY", "HH:MM:SS.ss"), paste("Sea", "Temperature"),
                               paste("East", "(true)", "Component", "of", "Current"), paste("North", "(true)", "Component", "of", "Current")),
                     "units"=c("degrees", "m/s","dbar","1","1", "s","degree_C", "m/s", "m/s"),
                     "standard_name"=c("horizontal_current_direction","horizontal_current_speed",
                                       "sea_water_pressure","sea_water_practical_salinity", "sea_water_conductivity_ratio",
                                       "time","sea_water_temperature", "eastward_sea_water_velocity", "northward_sea_water_velocity"),
                                   "type"=rep("rcm", 9))
  if (silent == FALSE) {
      warning("There is no CF standard_name for HCDT or HCSP. They have been created to be horizontal_current_direction and horizontal_current_speed respectively ")
  }

    if (standard == "bodc") {
      DF$bodc <- c("CDTASS01", "LCSAZZ01", "PRESPR01", "PSLTZZ01", "CRAT", "SYTM*", "TEMPPR01",
          "LCEWZZ01", "LCNSZZ01")
  if (silent == FALSE) {
      warning("There were no BODC standards for CRAT, or SYTM. DFO codes are used.")
  }
    }
  } else if (type == "adcp") {

    DF <- data.frame("code"= c("DEPH", "EWCT", "HGHT", "NSCT", "PRES", "PTCH",
                               "ROLL", "SVEL", "SYTM", "VCSP", "UNKN", "ERRV", "BEAM", "TE90", "PGDP", "CMAG", "HEAD", "BR", "BV", "BA", "BG", "TEMP",
                               "PSAL", "DIST", "COMA", "BQ"),
                     "name"= c(paste("Sensor", "Depth", "below", "Sea", "Surface"),paste("East", "Component", "of", "Current"),
                               paste("Height/Altitude", "above", "Sea Surface"),paste("North", "Component", "of", "Current"),
                               paste("Sea", "Pressure", "(sea surface - 0)"),paste("Pitch", "Angle"),paste("Roll", "Angle"),paste("Sound", "Velocity"),
                               paste("PIPE", "Time", "Format", "DD-MMM-YYYY", "HH:MM:SS.ss"), paste("Vertical", "Current", "Speed"),
                               "Unknown WMO Code","Error Velocity","ADCP Echo Intensity","Temperature (1990 scale)",
                               paste("Percent", "Good", "Pings"), paste("Current",  "Magnitude"), "Heading", paste("Bottom", "Range"),
                               paste("Bottom", "Speed"), paste("Bottom", "ADCP", "Echo", "Intensity"), paste("Bottom", "Percent", "Good", "Pings"),
                               paste("Sea", "Temperature"), paste("Practical", "Salinity"), "Distance", "Correlation Magnitude",
                               "Bottom Correlation Magnitude"),
                     units=c("metres","m/s","metres","m/s","decibars", "degrees","degrees","m/s",
                             "s","m/s","1","m/s","1","degrees C", "%", "m/s", "degree", "m", "m/s", "1", "%", "degrees C", "1", "m", "1",
                             "1"),
                     "standard_name"= c("sensor_depth_below_sea_surface","eastward_sea_water_velocity","height_above_mean_sea_level",
                                        "northward_sea_water_velocity","sea_water_pressure","platform_pitch_angle","platform_roll_angle",
                                        "speed_of_sound_in_sea_water","time","upward_sea_water_velocity","unknown_WMO_code",
                                        "indicative_error_from_multibeam_acoustic_doppler_velocity_profiler_in_sea_water",
                                        "signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water",
                                        "temperature_1990_scale","percent_good_ping", "current_magnitude", "heading", "bottom_range",
                                        "bottom_velocity", "bottom_signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water",
                                        "bottom_percent_good_ping", "sea_water_temperature", "sea_water_practical_salinity", "distance",
                                        "correlation_magnitude", "bottom_correlation_magnitude"),
                                    "type"=rep("adcp", 26))
  if (silent == FALSE) {
      warning("See caution section of ADCP vignette.")
  }


    if (standard == "bodc") {
      DF$bodc <- c("DEPH", "LCEWZZ01", "HGHT", "LCNSZZ01", "PRESPR01", "PTCHGP01", "ROLLGP01",
                   "SVELXXXX", "SYTM", "LRZAZZZ", "UNKN", "LERRAP01", "TNIHCE", "TEMPP901",
                   "PCGDAP01", "CMAG", "HEAD", "bottom_range", "bottom_velocity",
                   "bottom_amplitude", "bottom_percent_good", "TEMPPR01", "PSLTZZ01", "distance",
                   "correlation_magnitude", "bottom_correlation_magnitude")

  if (silent == FALSE) {
      warning("There were no BODC standards for DEPH, HGHT, SYTM, UNKN, CMAG, HEAD. DFO codes are used. There are
          also no BODC standards for bottom_range, bottom_velocity, bottom_amplitude, bottom_percent_good,
          bottom_correlation_magnitude, or distance. In these cases, created CF standards are used.")
  }
    }

  }


  DF

}
