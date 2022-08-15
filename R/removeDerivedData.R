#' Remove derived ctd data
#'
#' @param odf an odf object (oce::read.odf())
#'
#' @param debug integer value indicating level of debugging.
#' If this is less than 1, no debugging is done. Otherwise,
#' some functions will print debugging information
#'
#' @return CTD object: an odf object with only data for time, conductivity, salinity,
#' temperature, and pressure. RCM object: an odf object with only data for horizontal_current_direction,
#' barotropic_sea_water_x_velocity, sea_water_pressure, sea_water_practical_salinity", "time",
#' and sea_water_temperature

removeDerivedData <- function(odf, debug=0) {
  MCTD <- grepl("MCTD", odf[['filename']])
  RCM <- grepl("RCM", odf[['filename']])
  ADCP <- grepl("ADCP", odf[['filename']])

  if (RCM) {
  if (debug > 0) {
    message("This is an RCM type")
  }
  ctdDataNames <- c("horizontal_current_direction", paste0("horizontal_current_direction_", 1:4),"barotropic_sea_water_x_velocity", paste0("barotropic_sea_water_x_velocity_",1:4),
  "sea_water_pressure", paste0("sea_water_pressure_", 1:4),"sea_water_practical_salinity",paste0("sea_water_practical_salinity_",1:4),"time",paste0("time_", 1:4),"sea_water_temperature",paste0("sea_water_temperature_",1:4))
  ctdOriginalNames <- c("horizontal_current_direction", paste0("horizontal_current_direction_", 1:4),"barotropic_sea_water_x_velocity", paste0("barotropic_sea_water_x_velocity_",1:4),
                        "sea_water_pressure", paste0("sea_water_pressure_", 1:4),"sea_water_practical_salinity",paste0("sea_water_practical_salinity_",1:4),"time",paste0("time_", 1:4),"sea_water_temperature",paste0("sea_water_temperature_",1:4))

  } else if (MCTD) {
    if (debug > 0) {
      message("This is an MCTD type")
    }
    ctdDataNames <- c("time", paste0("time_", 1:4),"sea_water_electrical_conductivity",paste0("sea_water_electrical_conductivity_", 1:4), "sea_water_practical_salinity",paste0("sea_water_practical_salinity_", 1:4), "sea_water_temperature",paste0("sea_water_temperature_", 1:4), "sea_water_pressure",paste0("sea_water_pressure_", 1:4))
    ctdOriginalNames <- c("time", paste0("time_", 1:4),"sea_water_electrical_conductivity",paste0("sea_water_electrical_conductivity_", 1:4), "sea_water_practical_salinity",paste0("sea_water_practical_salinity_", 1:4), "sea_water_temperature",paste0("sea_water_temperature_", 1:4), "sea_water_pressure",paste0("sea_water_pressure_", 1:4))
      } else if (ADCP) {
    if (debug > 0) {
      message("This is an ADCP type")
    }
    ctdDataNames <- c("eastward_sea_water_velocity", "northward_sea_water_velocity",
      "time","upward_sea_water_velocity",
      "indicative_error_from_multibeam_acoustic_doppler_velocity_profiler_in_sea_water",
      "signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water")

    ctdOriginalNames <- c("eastward_sea_water_velocity", "northward_sea_water_velocity",
                      "time","upward_sea_water_velocity",
                      "indicative_error_from_multibeam_acoustic_doppler_velocity_profiler_in_sea_water",
                      "signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water")

  }
  # Removing data
  throwAway <- list()
  for (n in names(odf[["data"]])) {
    if (!(n %in% ctdDataNames)) {
      throwAway[i] <- n
      odf <- oceDeleteData(odf, name=n)
      message(gsub(".*M","",odf[['filename']]), ": removed data and dataNamesOriginal ", n)
    }
  }

  if (length(throwAway) == 0) {
    message("Nothing to remove for this file.")

  }

  for (i in odf[["dataNamesOriginal"]]) {
    if (!(i %in% ctdOriginalNames)) {
      b <- which(odf[['dataNamesOriginal']] == i)
      odf[['dataNamesOriginal']] <- odf[['dataNamesOriginal']][-b]
    }
  }

  odf
}
