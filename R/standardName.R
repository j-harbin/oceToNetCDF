#' Obtain standard name, name, and units from GF3 code
#'
#' This function reveals the standard name, name, and units in climate and
#' forecast (CF) standards from a General Formatting (GF3) standard
#'code parameter.
#'
#' @param gf3 a character indicating a GF3 (General Formatting) standard code parameter
#' @param data a data frame of standard name, name, units, and GF3 codes likely from getStandardData
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#' @return A list containing the standard name and unit of the GF3 code in CF standard
#' @export
#'
#' @examples
#' library(oceToNetCDF)
#' data <- getStandardData(type="ctd")
#' standardName("TEMP", data=data)
standardName <- function(gf3, data=NULL, debug=0) {

  if (is.null(data)) {
    stop("In standardName, must provide a data frame for data")
  }

  if (!inherits(data, "data.frame")) {
    stop("In standardName, data must be a data.frame class, not ", class(data))
  }

  # Adding in a test when code is in Matlab format

  # This is for matlab origin files
  matnames <- c("prDM","t090C","sal00","t190C","sal11","o2ML.L", "yday", "salinity", "temperature", "pressure", "datenum")
  namesRDI <- c("v", "q","g", "a", "bv", "ba", "br", "bg", "bc", "bq", "roll", "pitch", "heading", "temperature",
      "salinity", "depth", "soundSpeed", "time", "distance", "headingStd", "pitchStd", "rollStd") # This is not all of them
  namesLIA <- c("conductivity", "fluorescence")
  specialNames <- unique(c(matnames, namesRDI, namesLIA))

  if (gf3 %in% specialNames) {
      if (gf3 %in% c("prDM", "pressure")) {
          gf3 <- "PRES"
      }
    if (gf3 == "conductivity") {
      gf3 <- "CNDC"
    }
    if (gf3 == "fluorescence") {
      gf3 <- "FLOR"
    }
      if (gf3 %in% c("t090C", "t190C", "temperature")) {
          gf3 <- "TEMP"
      }
      if (gf3 %in% c("sal00", "sal11", "salinity")) {
          gf3 <- "PSAL"
      }
      if (gf3 %in% c("o2ML.L")) {
          gf3 <- "DOXY"
      }
      if (gf3 %in% c("yday", "datenum", "time")) {
          gf3 <- "SYTM"
      }

      if (gf3 %in% c("distance")) {
          gf3 <- "DIST"
      }

      # This is for RDI origin files
      #FIXME: This doesn't include all of them

      if ("v" %in% gf3) {
          gf3 <- "NSCT"

      }
      if ("a" %in% gf3) {
          gf3 <- "BEAM"

      }

      if ("g" %in% gf3) {
          gf3 <- "PGDP"
      }
      if ("q" %in% gf3) {
          gf3 <- "COMA"
      }
      if ("bv" %in% gf3) {
          gf3 <- "BV"
      }

      if ("ba" %in% gf3) {
          gf3 <- "BA"
      }

      if (gf3 %in% c("bg")) {
          gf3 <- "BG"
      }
      if (gf3 %in% c("bg", "bc")) {
          gf3 <- "BQ"
      }

      if ("br" %in% gf3) {
          gf3 <- "BR"
      }

      if ("depth" %in% gf3) {
          gf3 <- "DEPH"
      }

      if ("soundSpeed" %in% gf3) {
          gf3 <- "SVEL"
      }

      if (gf3 %in% c("pitch", "pitchStd")) {
          gf3 <- "PTCH"
      }

      if (gf3 %in% c("roll", "rollStd")) {
          gf3 <- "ROLL"
      }
      if (gf3 %in% c("heading", "headingStd")) {
          gf3 <- "HEAD"
      }


  }

  line <- grep(data$code, pattern = gf3, ignore.case = TRUE)

  if (length(line) == 0) {
    yn <- list()
    for (i in 1:length(data$code)) {
      yn[[i]] <- grep( pattern = data$code[[i]], x = gf3, value = TRUE)
      if(length(yn[[i]] != 0)) {
        line <- i
      }
    }

  }
  if (length(line) == 0) {
    stop(gf3, " is not recognized in the codes for data type =", unique(data$type), ". Make sure you used the proper 'type' argument in getStandardData()")
  }

  gf3 <- list(gf3 = gf3)
  gf3$standard_name <- as.character(data$standard_name[[line]])
  gf3$units <- as.character(data$units[[line]])
  gf3$name <- as.character(data$name[[line]])


  return(gf3)
}
