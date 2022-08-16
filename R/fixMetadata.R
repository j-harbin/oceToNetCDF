#' Clean metadata for a CTD and RCM object
#'
#' This function ensures the proper units are associated with each variable
#' and adds place holders for flags if they do not already exist.
#'
#' @param odf an odf object (oce::read.odf())
#' @param data a data frame of standard name, name, units, and GF3 codes likely from getData
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#'
#' @return an odf object with proper units and flags

fixMetadata <- function(odf, debug=0, data=NULL) {

  if (is.null(data)) {
    stop("must provide a dataframe data, likely from getData()")
  }

  if (!(class(data) == "data.frame")) {
    stop("the data must be of class data.frame, not ", class(data))
  }

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
