#' Clean data and metadata for a CTD and RCM object
#'
#' This function ensures the proper units are associated with each variable
#' and adds place holders for flags if they do not already exist.
#' For a CTD type, this function also converts conductivity ratio to
#' conducitivity if the proper unit is provided to abide by CF standards.
#'
#' @param odf an odf object (oce::read.odf())
#' @param data a data frame of standard name, name, units, and GF3 codes likely from getData
#' @param unit standardized unit S/m by default
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#'
#' @return an odf object
#' @example
#' \dontrun{
#' library(odfToNetcdf)
#' data <- getData(type="ctd")
#' odf1 <- read.odf("MCTD_KN179-05_1533_3309_1800.ODF")
#' odf2 <- nameReplacement(odf1, data=data)
#' odf3 <- removeDerived(odf2)
#' odf4 <- polishODF(odf3, data=data, unit='S/m')
#' }
#' @export

polishODF <- function(odf, debug=0, data=NULL, unit=NULL) {

  if (is.null(data)) {
    stop("must provide a dataframe data, likely from getData()")
  }

  if (!(class(data) == "data.frame")) {
    stop("the data must be of class data.frame, not ", class(data))
  }

  if (is.null(unit)) {
    message("conductivity ratio is not being converted to conductivity because no unit was provided")
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
          odf <- oceSetData(odf, name="sea_water_electrical_conductivity", value=(4.2914*crat))
        } else {
          eval(parse(text=paste0("odf@metadata$units$", names[keep], "$unit <- 'S/m'")))
          odf <- oceSetData(odf, name=names[keep], value=(4.2914*crat))
        }
      } else if (unit == 'mS/cm') {
        if (!(number)) {
          odf@metadata$units$sea_water_electrical_conductivity$unit <- "mS/cm"
          odf <- oceSetData(odf, name="sea_water_electrical_conductivity", value=(crat*42.914))
        } else  {
          eval(parse(text=paste0("odf@metadata$units$", names[keep], "$unit <- 'mS/cm'")))
          odf <- oceSetData(odf, name=names[keep], value=(crat*42.914))
        }
      }
    }
  }
  # Polishing metadata
  units <- odf@metadata$units
  DATA <- names(odf@data)
  l <- vector(mode="list", length=length(DATA))
  names(l) <- DATA

  for (i in seq_along(DATA)) {
    keep <- data$units[which(data$standard_name == gsub("_[0-9]$.*","",DATA[i])
)]
    unit <- odf@metadata$units[DATA[i]][[1]][1]$unit
    if (debug > 0) {
    message(keep, " is the units in dataframe for ", DATA[i], " and ", unit, " is what is in the odf")
    }
    odf@metadata$units[DATA[i]][[1]][1]$unit <- keep

    ## Populating flags
    if (length(odf@metadata$flags) == 0) {
    L<- rep(NA_real_, length(unlist(odf@data[DATA[i]])))
    l[[i]] <- L
    } else {
      message("Flags already exist for this profile.")
    }

  }
  odf@metadata$flags <- l

  odf

}
