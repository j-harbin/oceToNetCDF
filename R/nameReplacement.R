#' Replace DFO codes with CF standard names
#'
#' This function replaces DFO codes for dataNamesOriginal and data
#' with CF standards. For ctd types, if conductivity ratio (CRAT)
#' exists, the values are converted to sea_water_electrical_conductivity
#' values to abide by CF standards, and the unit is changed to the
#' specified unit.
#'
#' @param odf an odf object [oce::read.odf()]
#' @param data a data frame of standard name, name, units, and GF3 codes likely from [getCFData()]
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#' @param institute an optional name to add as the institute name
#' @param unit specified unit of 'S/m' or 'mS/cm' to convert CRAT to
#' sea_water_electrical_conductivty for CF standard. This is only
#' used for ctd type
#' @return an odf file
#' @importFrom oce oceSetMetadata
#' @examples
#' library(odfToNetCDF)
#' library(oce)
#' data <- getCFData(type="ctd")
#' f <- system.file("extdata", "mctd.ODF", package="odfToNetCDF")
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

  header <- odf[['metadata']]$header
  k <- grep("PARAMETER_HEADER",names(odf[['metadata']]$header))
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
  ##dataNamesOriginal[which(dataNamesOriginal == "CRAT_01")] <- "CNDC_01"
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

  odf <- oce::oceSetMetadata(odf, name="dataNamesOriginal", value=dataNamesOriginal)
  if (!(is.null(institute))) {
  odf <- oce::oceSetMetadata(odf, name="institute", value=institute)
  }
  names(odf@data) <- odf@metadata$dataNamesOriginal
  names(odf@metadata$units) <- odf@metadata$dataNamesOriginal

  # Converting CRAT to conductivity for CF standards
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
          } else {
            eval(parse(text=paste0("odf@metadata$units$", names[keep], "$unit <- 'S/m'")))
            odf <- oce::oceSetData(odf, name=names[keep], value=(4.2914*crat))
          }
        } else if (unit == 'mS/cm') {
          if (!(number)) {
            odf@metadata$units$sea_water_electrical_conductivity$unit <- "mS/cm"
            odf <- oce::oceSetData(odf, name="sea_water_electrical_conductivity", value=(crat*42.914))
          } else  {
            eval(parse(text=paste0("odf@metadata$units$", names[keep], "$unit <- 'mS/cm'")))
            odf <- oce::oceSetData(odf, name=names[keep], value=(crat*42.914))
          }
        }
      }
    }
  }

  ## Adding eastward and northward velocity for RCM data types (from magnitude and heading)
  if (unique(data$type) == 'rcm') {
    spd <- odf[['horizontal_current_speed']]
    dir <- odf[['horizontal_current_direction']]
    v <- spd*cos(dir * pi/180)
    u <- spd*sin(dir *pi/180)
    odf <- oce::oceSetData(odf, standardName('EWCT', data)$standard_name, u,
                           unit=list(unit=expression(m/s), scale=''), originalName ="EWCT")
    odf <- oce::oceSetData(odf, standardName('NSCT', data)$standard_name, v,
                           unit=list(unit=expression(m/s), scale=''), originalName ="NSCT")
  }

  odf

}
