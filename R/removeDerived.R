#' Remove derived ctd data and metadata
#'
#' This function removes data and metadata that is derived.
#' For a CTD type, the only data and metadata kept is time, conductivity, salinity,
#' temperature, and pressure. For an RCM type, the only data and
#' metadata kept is horizontal_current_direction,
#' barotropic_sea_water_x_velocity, sea_water_pressure, sea_water_practical_salinity",
#' time, and sea_water_temperature
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
#' @example
#' \dontrun{
#' library(odfToNetcdf)
#' data <- getData(type="ctd")
#' odf1 <- read.odf("MCTD_KN179-05_1533_3309_1800.ODF")
#' odf2 <- nameReplacement(odf1, data=data)
#' odf3 <- removeDerived(odf2)
#' }
#' @export

removeDerived <- function(odf, debug=0) {
  # Removing Derived Data
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
      for (i in seq_along(ctdDataNames)) {
      throwAway[[i]] <- n
      }
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


  # Removing metadata
  # Naming bad metadata
  if (RCM) {
    ctdCodeNames <- c("HCDT", "HCSP", "PRES", "PSAL", "SYTM", "TEMP")
  } else if (MCTD) {
    ctdCodeNames <- c("SYTM", "CRAT", "PSAL", "TEMP", "PRES")
  } else if (ADCP) {
    ctdCodeNames <- c("EWCT", "NSCT", "SYTM", "VCSP", "ERRV","BEAM")
  }
  header <- odf[['metadata']]$header
  k <- grep("PARAMETER_HEADER",names(odf[['metadata']]$header))
  parameters <- rep(FALSE, length(header[k]))
  for (i in seq_along(header[k])) {
    param <- header[k][[i]][[paste0("CODE_",i)]]
    param2 <- gsub("\\_.*","",param) # Removing if there is digits (ie. "_01")
    parameters[i] <- param2
  }


  bad <- which(!(parameters %in% ctdCodeNames))

  if (length(bad) > 0) {
    if (RCM) {
      message("RCM TYPE: ", gsub(".*R","",odf[['filename']]), ": removed metadata for ", paste0(parameters[bad], sep=","))
    } else if (MCTD) {
      message("MCTD TYPE: ", gsub(".*M","",odf[['filename']]), ": removed metadata for ", paste0(parameters[bad], sep=","))
    } else if (ADCP) {
      message("ADCP TYPE: ", gsub(".*A","",odf[['filename']]), ": removed metadata for ", paste0(parameters[bad], sep=","))

    }
  } else {
    message("No metadata was removed because parameters =", paste(parameters, collapse=","))
  }
  bheader <- names(odf[["metadata"]]$header[k][bad])

  for (i in bheader) {
    odf@metadata$header[i] <- NULL
    odf@metadata$header[i] <- NULL
  }
  odf
}