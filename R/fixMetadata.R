#' Clean metadata for a CTD and RCM object
#'
#' This function ensures the proper units are associated with each variable
#' and adds place holders for flags if they do not already exist.
#'
#' @param odf an odf object (oce::read.odf())
#'
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#'
#' @return an odf object with proper units and flags

fixMetadata <- function(odf, debug=0) {

  units <- odf@metadata$units
  data <- names(odf@data)
  l <- vector(mode="list", length=length(data))
  names(l) <- data

  for (i in seq_along(data)) {
    keep <- DF$units[which(DF$standard_name == gsub("_[0-9]$.*","",data[i])
)]
    unit <- odf@metadata$units[data[i]][[1]][1]$unit
    if (debug > 0) {
    message(keep, " is the units in dataframe for ", data[i], " and ", unit, " is what is in the odf")
    }
    odf@metadata$units[data[i]][[1]][1]$unit <- keep

    ## Populating flags
    if (length(odf@metadata$flags) == 0) {
    L<- rep(NA_real_, length(unlist(odf@data[data[i]])))
    l[[i]] <- L
    } else {
      message("Flags already exist for this profile.")
    }

  }
  odf@metadata$flags <- l

  odf

}
