#' Replace DFO codes with CF standard names
#'
#' This function replaces DFO codes for dataNamesOriginal, data, and units
#' with CF standards. For ctd types, if conductivity ratio (CRAT)
#' exists, the values are converted to sea_water_electrical_conductivity
#' values to abide by CF standards, and the unit is changed to the
#' specified unit. For rcm types, if horizontal_current_speed and
#' horizontal_current_direction exist, eastward_sea_water_velocity
#' and north_ward_sea_water_velocity are calculated if they do not
#' already exist.
#'
#' @param odf an odf object [oce::read.odf()]
#' @param data a data frame of standard name, name, units, and GF3 codes likely from [getStandardData()]
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#' @param institute an optional name to add as the institute name
#' @param unit specified unit of 'S/m' or 'mS/cm' to convert CRAT to
#' sea_water_electrical_conductivty for CF standard. This is only
#' used for ctd type
#' @return an odf file
#' @importFrom oce oceSetMetadata
#' @importFrom oce oceDeleteData
#' @importFrom oce oceDeleteMetadata
#' @examples
#' library(oceToNetCDF)
#' library(oce)
#' data <- getStandardData(type="ctd")
#' f <- system.file("extdata", "mctd.ODF", package="oceToNetCDF")
#' odf1 <- read.odf(f)
#' odf2 <- nameReplacement(odf1, data=data, unit="S/m")
#' @export

nameReplacement <- function(odf, data=NULL, debug=0, institute=NULL, unit=NULL) {

    if (is.null(data)) {
        stop("In nameReplacement, must provide a data frame for data")
    }
    if (!(class(data) == "data.frame")) {
        stop("In nameReplacement, data must be a data.frame class, not ", class(data))
    }

    if (!(is.null(odf[['fileType']])) && odf@metadata$fileType == "matlab") {
      # Matlab origin
        matlabOrigin <- TRUE
        if (debug > 0) {
            message("Matlab type has been identified")
        }
        dataNamesOriginal <- unlist(unname(odf[['dataNamesOriginal']]))
        if (debug > 0) {
            message("dataNamesOriginal= ", paste0(dataNamesOriginal, sep=","), " with length =", length(dataNamesOriginal))
        }
        end <- NULL
        for (i in seq_along(dataNamesOriginal)) {
            end[[i]] <- standardName(dataNamesOriginal[[i]], data=data)$standard_name
        }
        if (debug > 0) {
            message("end= ", paste0(end, sep=","), " with length = ",length(end))
        }
        #dataNamesOriginal
        names(odf[['dataNamesOriginal']]) <- end
        #names of data
        b <- which(names(odf[['data']]) == "scan")
        names(odf@data)[-b] <- names(odf@metadata$dataNamesOriginal)

        # names of units
        names(odf@metadata$units) <- names(odf@metadata$dataNamesOriginal)


    } else if ((!(is.null(odf[['fileType']])) && odf@metadata$fileType == "rdi") | unique(data$type) == "adcp") {
      # RDI origin
      matlabOrigin <- FALSE
        if (debug > 0) {
            message("rdi type has been identified")
        }
       namesData <- names(odf[['data']])
       keep <- which(namesData %in% c("v", "q", "g", "a", "bv", "ba", "br", "bg", "bc", "bq", "roll", "pitch", "heading", "temperature",
                                      "salinity", "depth", "soundSpeed", "time", "distance"))
       dataNamesOriginal <- namesData[keep]
           badData <- namesData[which(!(namesData %in% dataNamesOriginal))]
           if (!(length(badData) == 0)) {
           ADP <- NULL
           for (i in seq_along(badData)) {
               odf <- oceDeleteData(odf, name=badData[i])
               if (i == max(seq_along(badData))) {
                   ADP[[1]] <- odf
               }
           }

           odf <- ADP[[1]]
       }


       if (debug > 0) {
         message("dataNamesOriginal= ", paste0(dataNamesOriginal, sep=","), " with length =", length(dataNamesOriginal))
       }
       end <- NULL
       for (i in seq_along(dataNamesOriginal)) {
         end[[i]] <- standardName(dataNamesOriginal[[i]], data=data)$standard_name
       }
       if (debug > 0) {
         message("end= ", paste0(end, sep=","), " with length = ",length(end))
       }
       #dataNamesOriginal
       odf <- oceSetMetadata(odf, name="dataNamesOriginal", value=as.list(dataNamesOriginal))
       names(odf[['dataNamesOriginal']]) <- end
       #names of data
       names(odf@data) <- names(odf@metadata$dataNamesOriginal)
       # names of units
       unitNames <- names(odf[['units']])
       CFunits <- unlist(lapply(unitNames, function(x) standardName(x, data=data)$standard_name))
       names(odf[['units']]) <- CFunits

    } else if (is.null(odf[['fileType']]) && !(unique(data$type) == "adcp")) {
      # ODF (NOT ADP)
        matlabOrigin <- FALSE
        header <- odf[['metadata']]$header
        if (is.null(header)) {
          stop("must set fileType of either 'matlab' or 'rdi' using oceSetMetadata()")
        }
        if (is.null(header)) {
            stop("Must set fileType to be either 'rdi' or 'matlab' using oceSetMetadata()")
        }
        k <- grep("PARAMETER_HEADER",names(odf[['metadata']]$header))
        if (length(k) == 0) {
            parameters <- unlist(unname(odf[['dataNamesOriginal']]))
            parameters <- parameters[-which(parameters == "flag")]
            dataNames <- names(odf[['data']])

            if (debug > 0) {
                message("parameters= ", paste0(parameters, sep=","), " with length= ", length(parameters))
            }

            standardNames <- NULL
            for (i in seq_along(dataNames)) {
                sN <- standardName(dataNames[i], data=data)$standard_name
                standardNames[[i]] <- sN
            }
            if (debug > 0) {
                message("standardNames= ", paste0(standardNames, sep=","), "with  length ", length(standardNames))
            }
            odf <- oceDeleteMetadata(odf, name="dataNamesOriginal")
            odf <- oceSetMetadata(odf, name="dataNamesOriginal", value=as.list(parameters))
            names(odf@metadata$dataNamesOriginal) <- standardNames
            names(odf@data) <- standardNames
            if ("flags" %in% names(odf@metadata$units)) {
            names(odf@metadata$units)[-which(names(odf@metadata$units) == "flag")] <- standardNames[-which(standardNames == 'time')]
            }

        } else {
            parameters <- rep(FALSE, length(header[k]))
            raw <- rep(FALSE, length(header[k]))
            for (i in seq_along(header[k])) {
                param <- header[k][[i]][[paste0("CODE_",i)]]
                param2 <- gsub("\\_.*","",param) # Removing if there is digits (ie. "_01")
                parameters[i] <- param2
                raw[i] <- param
            }
            parameters <- gsub("'", "", parameters)
            raw <- gsub("'", "", raw)
            if (length(which(parameters == "CRAT")) != 0) {
                message('Warning: Found CRAT instead of CNDC')
                ##parameters[which(parameters == "CRAT")] <- "CNDC"
            }
            if (!(length(parameters) == length(unique(parameters)))) {
                message("Warning: One of the parameters has a duplicate (ie. more than one sensor)")
            }
            if (debug > 0) {
                message("parameters= ", paste0(parameters, sep=","))
                message("raw= ", paste0(raw, sep=","))
            }

            # Getting standard names of the GF3 CODE
            t <- unlist(lapply(parameters, function(x) standardName(x, data=data)$standard_name))
            end <- list()
            for (i in seq_along(raw)) {
                if (grepl("01", paste0("_",gsub(".*_","",raw[i])))) {
                    #end[i] <- paste0(t[i], "_1")
                    end[i] <- t[i]
                } else if (grepl("02", paste0("_",gsub(".*_","",raw[i])))) {
                    end[i] <- paste0(t[i], "_2")
                } else if (grepl("03", paste0("_",gsub(".*_","",raw[i])))) {
                    end[i] <- paste0(t[i], "_3")
                } else if (grepl("04", paste0("_",gsub(".*_","",raw[i])))) {
                    end[i] <- paste0(t[i], "_4")
                } else {
                    end[i] <- t[i]
                }
            }

            if (debug > 0) {
                message("end= ", paste0(end, sep=","))
            }

            if (debug > 0) {
                message("t= ", paste0(t, sep=","))
            }
            # Replacing NAME with standard name
            tk <- which(!(t %in% ""))
            if (debug > 0) {
                message("tk= ", tk)
            }
            dataNamesOriginal <- unname(unlist(odf[['dataNamesOriginal']]))

            for (i in seq_along(t)) {
                if (i %in% tk) {
                    if (debug > 0) {
                        message(dataNamesOriginal[i], "  is being replaced with ", end[i])
                    }
                    dataNamesOriginal[i] <- end[i]
                } else {
                    message("standard names are not known for", parameters[i])
                }
            }

            names(odf@metadata$dataNamesOriginal) <- end
            names(odf@data) <- end
            names(odf@metadata$units) <- end
        }
    }

    if (!(is.null(institute))) {
        odf <- oce::oceSetMetadata(odf, name="institute", value=institute)
    }

    # Converting CRAT to conductivity for CF standards
    if (matlabOrigin) {
        parameters <- dataNamesOriginal
    }
    if (unique(data$type) == "ctd" && length(which(parameters == "CRAT") != 0)) {
        if (is.null(unit)) {
            stop("must provide a unit of either 'S/m' or 'mS/cm' for CTD type to convert CRAT to sea_water_electrical_conducitivity for CF standards")
        } else {
            if (!(unit %in% c("S/m", "mS/cm"))) {
                message("unit must be 'S/m' or'mS/cm', not ", unit)
            } else {
                names <- names(odf[['data']])
                keep <- which(grepl("sea_water_electrical_conductivity", names) == TRUE)
                number <- grepl("\\_[0-9]$", names[keep])

                if (number) {
                    crat <- unlist(unname(odf@data[names[keep]]))
                } else {
                    crat <- odf@data$sea_water_electrical_conductivity
                }
                if (unit == 'S/m') {
                    if (!(number)) {
                        odf@metadata$units$sea_water_electrical_conductivity$unit <- "S/m"
                        odf <- oce::oceSetData(odf, name="sea_water_electrical_conductivity", value=(4.2914*crat))
                        odf <- oce::oceDeleteData(odf, name="sea_water_electrical_conductivity_ratio")
                        cr <- which(names(odf[['dataNamesOriginal']]) == "sea_water_electrical_conductivity_ratio")
                        names(odf[['dataNamesOriginal']])[cr] <- "sea_water_electrical_conductivity"
                        odf@metadata$dataNamesOriginal$sea_water_electrical_conductivity <- "CNDC"
                    } else {
                        eval(parse(text=paste0("odf@metadata$units$", names[keep], "$unit <- 'S/m'")))
                        odf <- oce::oceSetData(odf, name=names[keep], value=(4.2914*crat))
                    }
                } else if (unit == 'mS/cm') {
                    if (!(number)) {
                        odf@metadata$units$sea_water_electrical_conductivity$unit <- "mS/cm"
                        odf <- oce::oceSetData(odf, name="sea_water_electrical_conductivity", value=(crat*42.914))
                        odf <- oce::oceDeleteData(odf, name="sea_water_electrical_conductivity_ratio")
                        cr <- which(names(odf[['dataNamesOriginal']]) == "sea_water_electrical_conductivity_ratio")
                        names(odf[['dataNamesOriginal']])[cr] <- "sea_water_electrical_conductivity"
                        odf@metadata$dataNamesOriginal$sea_water_electrical_conductivity <- "CNDC"
                    } else  {
                        eval(parse(text=paste0("odf@metadata$units$", names[keep], "$unit <- 'mS/cm'")))
                        odf <- oce::oceSetData(odf, name=names[keep], value=(crat*42.914))
                    }
                }
            }
        }
    }

    ## Adding eastward and northward velocity for RCM data types (from magnitude and heading)
    if (unique(data$type) == 'rcm' && "horizontal_current_speed" %in% dataNamesOriginal && "horizontal_current_direction" %in% dataNamesOriginal) {
        spd <- odf[['horizontal_current_speed']]
        dir <- odf[['horizontal_current_direction']]
        v <- spd*cos(dir * pi/180)
        u <- spd*sin(dir *pi/180)
        if (!("eastward_sea_water_velocity" %in% dataNamesOriginal)) {
            odf <- oce::oceSetData(odf, standardName('EWCT', data)$standard_name, u,
                unit=list(unit=expression(m/s), scale=''), originalName = standardName('EWCT', data)$code)
            odf@metadata$dataNamesOriginal$eastward_sea_water_velocity <- "EWCT"
            message(names(odf[['dataNamesOriginal']]))
        }
        if (!("northward_sea_water_velocity" %in% dataNamesOriginal)) {
            odf <- oce::oceSetData(odf, standardName('NSCT', data)$standard_name, v,
                unit=list(unit=expression(m/s), scale=''), originalName =standardName('NSCT', data)$code)
            odf@metadata$dataNamesOriginal$northward_sea_water_velocity <- "NSCT"


        }
    }

    odf

}
