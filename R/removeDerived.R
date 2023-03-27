#' Remove derived ctd and rcm data and metadata
#'
#' This function removes data and metadata that is derived.
#' For a CTD type, the only data and metadata kept is time, conductivity, salinity,
#' temperature, and pressure. For an RCM type, the only data and
#' metadata kept is horizontal_current_direction,
#' horizontal_current_speed, sea_water_pressure, sea_water_practical_salinity,
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
#' horizontal_current_speed, sea_water_pressure, sea_water_practical_salinity", "time",
#' and sea_water_temperature
#' @importFrom oce oceDeleteData
#' @examples
#' library(oceToNetCDF)
#' library(oce)
#' data <- getStandardData(type="ctd")
#' f <- system.file("extdata", "mctd.ODF", package="oceToNetCDF")
#' odf1 <- read.odf(f)
#' odf2 <- nameReplacement(odf1, data=data, unit="S/m")
#' odf2 <- oceSetMetadata(object=odf2,name="mooringType", value="mctd")
#' odf3 <- removeDerived(odf2)
#' @export

removeDerived <- function(odf, debug=0) {
    # Removing Derived Data
    if (grepl("MCTD", odf[['filename']]) == FALSE && grepl("mctd", odf[['filename']]) == FALSE && grepl("RCM", odf[['filename']]) == FALSE
        && grepl("rcm", odf[['filename']]) == FALSE && is.null(odf[['mooringType']])) {
        stop("Type of file not found in filename. Set your odf[['mooringType']] to be either mctd, rcm, or adcp. See help page for details")
    }

    if (grepl("MCTD", odf[['filename']]) | !(is.null(odf[['mooringType']])) && odf[['mooringType']] == "mctd") {
        MCTD <- TRUE
    } else {
        MCTD <- FALSE
    }

    if (grepl("mctd", odf[['filename']]) | !(is.null(odf[['mooringType']])) && odf[['mooringType']] == "mctd") {
        mctd <- TRUE
    } else {
        mctd <- FALSE
    }
    if (grepl("RCM", odf[['filename']]) | !(is.null(odf[['mooringType']])) && odf[['mooringType']] == "rcm") {
        RCM <- TRUE
    } else {
        RCM <- FALSE
    }
    if (grepl("rcm", odf[['filename']]) | !(is.null(odf[['mooringType']])) && odf[['mooringType']] == "rcm") {
        rcm <- TRUE
    } else {
        rcm <- FALSE
    }

    if (RCM | rcm) {
        if (debug > 0) {
            message("This is an RCM type")
        }
        dataNames <- c("horizontal_current_direction", paste0("horizontal_current_direction_", 1:4),"horizontal_current_speed", paste0("horizontal_current_speed_",1:4),
            "sea_water_pressure", paste0("sea_water_pressure_", 1:4),"sea_water_practical_salinity",paste0("sea_water_practical_salinity_",1:4),"time",paste0("time_", 1:4),"sea_water_temperature",paste0("sea_water_temperature_",1:4),
            "eastward_sea_water_velocity", "northward_sea_water_velocity")
        originalNames <- c("horizontal_current_direction", paste0("horizontal_current_direction_", 1:4),"horizontal_current_speed", paste0("horizontal_current_speed_",1:4),
            "sea_water_pressure", paste0("sea_water_pressure_", 1:4),"sea_water_practical_salinity",paste0("sea_water_practical_salinity_",1:4),"time",paste0("time_", 1:4),"sea_water_temperature",paste0("sea_water_temperature_",1:4),
            "eastward_sea_water_velocity", "northward_sea_water_velocity")

    } else if (MCTD | mctd) {
        if (debug > 0) {
            message("This is an MCTD type")
        }
        dataNames <- c("time", paste0("time_", 1:4),"sea_water_electrical_conductivity",paste0("sea_water_electrical_conductivity_", 1:4), "sea_water_practical_salinity",paste0("sea_water_practical_salinity_", 1:4), "sea_water_temperature",paste0("sea_water_temperature_", 1:4), "sea_water_pressure",paste0("sea_water_pressure_", 1:4), "sea_water_dissolved_oxygen", paste0("sea_water_dissolved_oxygen_", 1:4), "sea_water_fluorescence", paste0("sea_water_fluorescence_", 1:4))
        originalNames <- c("time", paste0("time_", 1:4),"sea_water_electrical_conductivity",paste0("sea_water_electrical_conductivity_", 1:4), "sea_water_practical_salinity",paste0("sea_water_practical_salinity_", 1:4), "sea_water_temperature",paste0("sea_water_temperature_", 1:4), "sea_water_pressure",paste0("sea_water_pressure_", 1:4), "sea_water_dissolved_oxygen", paste0("sea_water_dissolved_oxygen_", 1:5), "sea_water_fluorescence", paste0("sea_water_fluorescence_", 1:4))
    }
    # Removing data
    throwAway <- list()

    for (n in names(odf[["data"]])) {
        if (!(n %in% dataNames)) {
            for (i in seq_along(dataNames)) {
                throwAway[[i]] <- n
            }
            odf <- oce::oceDeleteData(odf, name=n)
            message("removed data and dataNamesOriginal: ", n)
        }
    }
    if (length(throwAway) == 0) {
        message("Nothing to remove for this file.")

    }

    #  for (i in odf[["dataNamesOriginal"]]) {
    #      if (!(i %in% originalNames)) {
    #          b <- which(odf[['dataNamesOriginal']] == i)
    #          odf[['dataNamesOriginal']] <- odf[['dataNamesOriginal']][-b]
    #      }
    #  }


    # Removing metadata
    # Naming bad metadata
    if (RCM | rcm) {
        codeNames <- c("HCDT", "HCSP", "PRES", "PSAL", "SYTM", "TEMP", "EWCT", "NSCT")
    } else if (MCTD | mctd) {
        codeNames <- c("SYTM", "CRAT", "PSAL", "TEMP", "PRES", "DOXY", "sal11", "t090C", "prDM", "o2ML.L", "yday", "temperature", "salinity", "pressure", "datenum", "FLOR", "c0mS/cm", "sal00", "tv290C", "flECO-AFL", "timeJ", "prdM", "time", "conductivity")
    }
    if (!(is.null(odf[['fileType']])) && odf[["fileType"]] == "matlab") {
        matlabfile <- TRUE
        parameters <- unlist(unname(odf[["dataNamesOriginal"]]))
        bad <- which(!(parameters %in% codeNames))
        if (debug > 0) {
            message("matlab type: parameters = ", paste0(parameters, sep=","), " and codeNames= ", paste0(codeNames, sep=","))

        }

    } else {
        if (debug > 0) {
            message("A matlab file was NOT identified")
        }
        matlabFile <- FALSE
        header <- odf[['metadata']]$header
        k <- grep("PARAMETER_HEADER",names(odf[['metadata']]$header))
        if (length(k) == 0) {
            parameters <- unlist(unname(odf[['dataNamesOriginal']]))
            bad <- which(!(parameters %in% codeNames))
        } else {
            parameters <- rep(FALSE, length(header[k]))
        for (i in seq_along(header[k])) {
            param <- header[k][[i]][[paste0("CODE_",i)]]
            param2 <- gsub("\\_.*","",param) # Removing if there is digits (ie. "_01")
            parameters[i] <- param2
        }
        parameters <- gsub(".*'","",parameters)

        bad <- which(!(parameters %in% codeNames))
        #message("parameters = ", paste0(parameters, sep=","), " and codeNames = ", paste0(codeNames, sep=","))
    }
    }
    if (length(bad) > 0) {
        if (RCM | rcm) {
            message("RCM TYPE: removed metadata for ", paste0(parameters[bad], sep=","))
        } else if (MCTD | mctd) {
            message("MCTD TYPE: ", gsub(".*M","",odf[['filename']]), ": removed metadata for ", paste0(parameters[bad], sep=","))
        }
    } else {
        message("No metadata was removed because parameters =", paste(parameters, collapse=","))
    }
    if (is.null(odf[['fileType']])) {
        bheader <- names(odf[["metadata"]]$header[k][bad])

        for (i in bheader) {
            odf@metadata$header[i] <- NULL
            odf@metadata$header[i] <- NULL
        }
    }
    odf
}
