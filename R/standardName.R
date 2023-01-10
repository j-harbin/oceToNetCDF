#' Obtain standard name, name, and units from GF3 code
#'
#' This function reveals the standard name, name, and units in climate and
#' forecast (CF) standards from a General Formatting (GF3) standard
#'code parameter.
#'
#' @param gf3 a character indicating a GF3 (General Formatting) standard code parameter
#' @param data a data frame of standard name, name, units, and GF3 codes likely from getCFData
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#' @return A list containing the standard name and unit of the GF3 code in CF standard
#' @export
#'
#' @examples
#' library(odfToNetCDF)
#' data <- getCFData(type="ctd")
#' standardName("TEMP", data=data)
standardName <- function(gf3, data=NULL, debug=0) {

  if (is.null(data)) {
    stop("In standardName, must provide a data frame for data")
  }
  if (!(class(data) == "data.frame")) {
    stop("In standardName, data must be a data.frame class, not ", class(data))
  }

  # Adding in a test when code is in Matlab format

  matnames <- c("prDM","t090C","sal00","t190C","sal11","o2ML.L", "yday", "salinity", "temperature", "pressure", "datenum")
  if (gf3 %in% matnames) {
    if (debug > 0) {
      message("matlab format has been identified")
    }
    # FIXME: This may not be correct
    if (gf3 %in% c("prDM", "pressure")) {
      gf3 <- "PRES"
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
    if (gf3 %in% c("yday", "datenum")) {
      gf3 <- "SYTM"
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
    stop(gf3, " is not recognized in the codes for data type =", unique(data$type), ". Make sure you used the proper 'type' argument in getCFData()")
  }

  gf3 <- list(gf3 = gf3)
  gf3$standard_name <- as.character(data$standard_name[[line]])
  gf3$units <- as.character(data$units[[line]])
  gf3$name <- as.character(data$name[[line]])


  return(gf3)
}
