#' Compile multiple adp ODF files to a single adp object
#'
#' This function reads in a set of adp odf files and compiles them
#' into a single adp object. It is capable of recognizing missing bins
#' or missing odf files and inserts NA values in appropriate slot of
#' data variables.
#'
#' @param files list of adp odf files
#' @importFrom oce read.oce read.odf as.adp oceSetMetadata processingLogAppend
#' @examples
#' \dontrun{
#' library(odfToNetCDF)
#' files <- list.files(pattern="ODF")
#' adp <- compileOdfToAdp(files)
#' }
#'
#' @export

compileOdfToAdp <- function(files) {
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
  u <- v <- w <- errorVelocity <- a <- unknown <- NULL
  for (vr in vars) {
    assign(vr, array(NA, dim=c(nt, nd)))
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

