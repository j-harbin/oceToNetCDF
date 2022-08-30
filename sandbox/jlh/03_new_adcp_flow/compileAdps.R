#' Compile multiple adp ODF files to a single adp object
#'
#' This function reads in a set of adp odf files and compiles them
#' into a single adp object. It is capable of recognizing missing bins
#' or missing odf files and inserts NA values in appropriate slot of
#' data variables.
#'
#' @param adps list of adp objects [oce::read.oce()]
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information
#' @importFrom oce read.oce read.odf as.adp oceSetMetadata processingLogAppend
#' @examples
#' library(odfToNetCDF)
#' f1 <- system.file("extdata", "adcp1.ODF", package="odfToNetCDF")
#' f2 <- system.file("extdata", "adcp2.ODF", package="odfToNetCDF")
#' files <- c(f1,f2)
#' adp <- compileOdfToAdp(files)
#'
#' @export

compileAdps <- function(adps, debug=0) {
  if (!requireNamespace("oce", quietly=TRUE))
    stop("must install.packages(\"oce\") for compileOdfToAdp() to work")

  if (!requireNamespace("abind", quietly=TRUE))
    stop("must install.packages(\"abind\") for plot() to work")

  if (length(adps) < 2) {
    stop("Must provide more than one adp to compile")
  }

  nd <- length(adps)
  d <- adps[[1]]
  #d <- nameReplacement(d, data=data)
  nt <- length(d[['time']])
  vars <- names(d@data)
  vars <- vars[-which(grepl("time", vars))]

  if (debug > 0) {
  message("vars= ", paste0(vars, sep=""))
  }

  u <- v <- w <- errorVelocity <- a <- unknown <- NULL

  # Creating array
  for (vr in vars) {
    assign(vr, array(NA, dim=c(nt, nd)))
  }

  depth <- NULL
  for (i in 1:length(files)) {
    d <- adps[[i]]
    t <- d[['time']]
    depth[i] <- d[['depthMin']]
    for (vr in vars) {
      eval(parse(text=paste0(vr, "[, i] <- d[['", vr, "']]")))
    }
  }

  ## need to sort the depths because of file sorting ...
  # prevent compiler warning
  o <- order(depth, decreasing = TRUE)
  depth <- depth[o]
  for (vr in vars) {
    eval(parse(text=paste0(vr, "<- ", vr, "[, o]")))
  }
  distance <- max(depth) - depth
  names <- lapply(d[["dataNamesOriginal"]], function(x) standardName(x, data=data)$standard_name)
message(length(names))
  adp <- oce::as.adp(t, distance, v=abind::abind(u, v, w, errorVelocity, along=3), a=a, q=unknown)
  #adp <- nameReplacement(adp, data=data)
  for (m in names(d@metadata)) {
    if (m != 'units' & m != 'flags' & m != 'dataNamesOriginal') {
      adp <- oce::oceSetMetadata(adp, m, d[[m]], note = NULL)
    }
  }

  ## depthMinMax
  adp <- oce::oceSetMetadata(adp, 'depthMin', min(depth))
  adp <- oce::oceSetMetadata(adp, 'depthMax', max(depth))
  adp@metadata$source <- 'odf'
  adp <- oce::oceSetMetadata(adp, "dataNamesOriginal", names)
  adp@processingLog <- oce::processingLogAppend(adp@processingLog, 'Creation : Data and metadata read into adp object from ODF file')

  return(adp)
}

