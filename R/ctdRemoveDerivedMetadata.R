#' Remove derived ctd metadata
#'
#' @param odf an odf object (oce::read.odf())
#'
#' @return CTD object: an odf object with only metadata for time, conductivity, salinity,
#' temperature, and pressure. RCM object: an odf object with only metadata for horizontal_current_direction,
#' barotropic_sea_water_x_velocity, sea_water_pressure, sea_water_practical_salinity", "time",
#' and sea_water_temperature

ctdRemoveDerivedMetadata <- function(odf) { # oce object
  MCTD <- grepl("MCTD", odf[['filename']])
  RCM <- grepl("RCM", odf[['filename']])
  ADCP <- grepl("ADCP", odf[['filename']])

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
