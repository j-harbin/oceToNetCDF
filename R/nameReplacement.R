#' Replace DFO Codes with CF standard names
#'
#' @param odf an odf object (oce::read.odf())
#' @param data a data frame of standard name, name, units, and GF3 codes likely from getData
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#'
#' @return an odf file with the dataOringinalNames as standard CF
#' names and the institute as UW/DFO
#' @importFrom oce
#' @example
#'\dontrun{
#' library(odfToNetcdf)
#' data <- getData(type="ctd")
#' odf1 <- read.odf("MCTD_KN179-05_1533_3309_1800.ODF")
#' odf2 <- nameReplacement(odf1, data=data)
#' }
#' @export

nameReplacement <- function(odf, data=NULL, debug=0) {

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
  parameters[which(parameters == "CRAT")] <- "CNDC"
  if (!(length(parameters) == length(unique(parameters)))) {
    message("Warning: One of the parameters has a duplicate (ie. more than one sensor)")
  }
  if (debug > 0) {
    message("parameters= ", parameters)
    message("raw= ", raw)
  }

  # Getting standard names of the GF3 CODE
  t <- unlist(lapply(parameters, function(x) standardName(x, data=data)$standard_name))
  end <- list()
  for (i in seq_along(raw)) {
    if (grepl("01", paste0("_",gsub(".*_","",raw[i])))) {
      end[i] <- paste0(t[i], "_1")
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
    message("end= ", end )
  }

  if (debug > 0) {
    message("t= ", t)
  }
  # Replacing NAME with standard name
  tk <- which(!(t %in% ""))
  if (debug > 0) {
    message("tk= ", tk)
  }
  dataNamesOriginal <- unname(unlist(odf[['dataNamesOriginal']]))
  dataNamesOriginal[which(dataNamesOriginal == "CRAT_01")] <- "CNDC_01"
  for (i in seq_along(t)) {
    if (i %in% tk) {
      if (debug > 0) {
      message(dataNamesOriginal[i], "what is being replaced with", end[i])
      }
      dataNamesOriginal[i] <- end[i]
    } else {
      message("standard names are not known for", parameters[i])
    }
  }

  odf <- oce::oceSetMetadata(odf, name="dataNamesOriginal", value=dataNamesOriginal)
  odf <- oce::oceSetMetadata(odf, name="institute", value="UW/BIO")
  names(odf@data) <- odf@metadata$dataNamesOriginal
  names(odf@metadata$units) <- odf@metadata$dataNamesOriginal
  odf

}
