#' Convert conductivity ratio to conductivity
#'
#' @param crat a conductivity ratio number
#'
#' @param unit the specified unit of conductivity to convert to
#' (Either 'S/m' or 'mS/cm')
#'
#' @return a conductivity value in the specified unit

convertConductivityRatio <- function(crat, unit='S/m') { # crat is a number
  if (!(unit %in% c("S/m", "mS/cm"))) {
    stop("unit argument must be either 'S/m', 'mS/cm', not ", unit)
  }
  if (!(class(crat) %in% c("integer", "numeric"))) {
    stop("The class of crat must be integer or numeric, not ", class(crat))
  }
  if (unit == 'S/m') {
    return(crat*4.2914)
  } else if (unit == 'mS/cm') {
    return(crat*42.914)
  }
}
