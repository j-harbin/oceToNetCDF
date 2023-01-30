#' Compile multiple adp ODF files to a single adp object
#'
#' This function reads in a set of adp odf files and compiles them
#' into a single adp object. It is capable of recognizing missing bins
#' or missing odf files and inserts NA values in appropriate slot of
#' data variables.
#'
#' @param files list of adp odf files
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information
#' @importFrom oce read.oce read.odf as.adp oceSetMetadata processingLogAppend
#' @examples
#' library(odfToNetCDF)
#' data <- getCFData(type="adcp")
#' f1 <- system.file("extdata", "adcp1.ODF", package="odfToNetCDF")
#' f2 <- system.file("extdata", "adcp2.ODF", package="odfToNetCDF")
#' files <- c(f1,f2)
#' adp <- compileOdfToAdp(files)
#'
#' @export

compileOdfToAdp <- function(files, debug=0) {
  if (!requireNamespace("oce", quietly=TRUE))
    stop("must install.packages(\"oce\") for compileOdfToAdp() to work")

  if (!requireNamespace("abind", quietly=TRUE))
    stop("must install.packages(\"abind\") for plot() to work")

  files <- if (is.list(files)) unlist(files) else files

  nd <- length(files)
  ## read the first one to get length of time:
  d <- oce::read.oce(files[1])
  nt <- length(d[['time']])
  vars <- names(d@data)
  vars <- vars[-which(vars == 'time')]
  if (debug > 0) {
    message("Before renaming, vars = c('", paste0(vars, collapse="', '"), "')")
  }
  u <- v <- w <- errorVelocity <- a <- unknown <- NULL

  # Create array
  if (debug > 0) {
    message("Creating an array below for all vars= ", paste0(vars, sep=","), " with dim =", paste0(c(nt,nd), sep=","))
  }
  for (vr in vars) {
    assign(vr, array(NA, dim=c(nt, nd)))
  }
  ## Some ADCP ODF datasets actually contain a field for "depth", presumably because the ADCP had a pressure sensor.
  ## For these cases, you can't use 'depthMin' to get the bin depth (to convert to 'distance') but
  ## we need to determine the distance and then add the "depth" or "pressure" field back into the adp
  ## object so that it can be preserved.
  if ('depth' %in% vars) {
    if (debug > 0) {
        message("Field 'depth' found in ADP object")
    }
    vars <- vars[-which(vars == 'depth')]
    depth_array <- array(NA, dim=c(nt, length(files)))
    for (f in 1:length(files)) {
      d <- oce::read.odf(files[f])
      t <- d[['time']]
      depth_array[, f] <- d[['depth']]
      for (vr in vars) {
        eval(parse(text=paste0(vr, "[, f] <- d[['", vr, "']]")))
      }
    }
    min_depth <- apply(depth_array, 2, min)
    o <- order(min_depth, decreasing = TRUE)
    min_depth <- min_depth[o]
    depth_array <- depth_array[,o]
    for (vr in vars) {
      eval(parse(text=paste0(vr, "<- ", vr, "[, o]")))
    }
    distance <- max(min_depth) - min_depth
    adp <- oce::as.adp(t, distance, v=abind::abind(u, v, w, errorVelocity, along=3), a=a, q=unknown)
    adp <- oce::oceSetData(adp, 'depth', depth_array[,1], list(unit=expression(m), scale=''))
    for (m in names(d@metadata)) {
      if (m != 'units' & m != 'flags' & m != 'dataNamesOriginal') {
        adp <- oce::oceSetMetadata(adp, m, d[[m]], note = NULL)
      }
    }
    ## depthMinMax
    adp <- oce::oceSetMetadata(adp, 'depthMin', min(min_depth))
    adp <- oce::oceSetMetadata(adp, 'depthMax', max(min_depth))
    adp@metadata$source <- 'odf'
    adp@processingLog <- oce::processingLogAppend(adp@processingLog, 'Creation : Data and metadata read into adp object from ODF file')

    return(adp)

  } else {
    if (debug > 0) {
      message("No depth was identified")
    }
    depth <- NULL
    for (f in 1:length(files)) {
      d <- oce::read.odf(files[f])
      t <- d[['time']]
      depth[f] <- d[['depthMin']]
      for (vr in vars) {
        eval(parse(text=paste0(vr, "[, f] <- d[['", vr, "']]")))
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
    if (debug > 0) {
      message("dim(u)= ", paste0(dim(u), sep=","), " and dim(a)= ", paste0(dim(a), sep=","))

    }
    adp <- oce::as.adp(t, distance, v=abind::abind(u, v, w, errorVelocity, along=3), a=a, q=unknown)
    for (m in names(d@metadata)) {
      if (m != 'units' & m != 'flags' & m != 'dataNamesOriginal') {
        adp <- oce::oceSetMetadata(adp, m, d[[m]], note = NULL)
      }
    }

    ## depthMinMax
    adp <- oce::oceSetMetadata(adp, 'depthMin', min(depth))
    adp <- oce::oceSetMetadata(adp, 'depthMax', max(depth))
    adp@metadata$source <- 'odf'
    adp@processingLog <- oce::processingLogAppend(adp@processingLog, 'Creation : Data and metadata read into adp object from ODF file')

    return(adp)
  }
}
