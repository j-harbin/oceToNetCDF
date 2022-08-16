#' Turn conductivity ratio to sea_water_electrical_conductivity
#'
#' @param odf a odf object (oce::read.odf())
#' @param units standardized unit S/m by default
#'
#' @return an odf object with conductivity ratio converted to sea_water_electrical_conductivity in specified unit
#' @export
#'
#' @examples

cratToConductivity <- function(odf, unit='S/m') {
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
    } else {
        stop('Unrecognized unit')
    }
  odf
}
