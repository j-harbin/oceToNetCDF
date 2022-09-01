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

  # Step one: Determine length of time
  nd <- length(adps)
  d <- adps[[1]]
  #d <- nameReplacement(d, data=data)
  nt <- length(d[['time']])

  if (debug > 0) {
    message("Step 1: Determined the length of time in first adp. All of the times are the same for each file. length(t)= ", nt)
  }

  vars <- names(d@data)
  vars <- vars[-which(grepl("time", vars))]

  if (debug > 0) {
    message("Step 2: Just determined names of variables. All of these are the same for each file. vars= ", paste0(vars, sep=","))
  }

  u <- v <- w <- errorVelocity <- a <- unknown <- NULL

  # Creating array
  for (vr in vars) {
    assign(vr, array(NA, dim=c(nt, nd)))
  }

  if (debug > 0) {
    message("Step 3: Just created array")
  }

  depth <- NULL
  ADCP <- list()
  for (i in seq_along(adps)) {
    A <- adps[[i]]
    t <- A[['time']]
    depth[i] <- A[['depthMin']]
    for (vr in vars) {
      eval(parse(text=paste0(vr, "[, i] <- A[['", vr, "']]")))
    }
    ADCP[i] <- A
  }

  if (debug > 0) {
    message("Step 4: Just assigned each variables from ", paste0(vars, sep=","), " in the correct location of array.")
    message("adps are still individual object. length(ADCP)= ", length(ADCP), " and length(ADCP[[1]][['v']])= ", length(ADCP[[1]][['v']]))
  }

  ## need to sort the depths because of file sorting ...
  # prevent compiler warning
  o <- order(depth, decreasing = TRUE)
  depth <- depth[o]
  for (vr in vars) {
    eval(parse(text=paste0(vr, "<- ", vr, "[, o]")))
  }

  if (debug > 0) {
    message("Step 5: Just rearranged variables to be with decreasing depth")
    message("The length of depth is ", length(depth))
  }
  distance <- max(depth) - depth
  names <- lapply(d[["dataNamesOriginal"]], function(x) standardName(x, data=data)$standard_name)

if (debug > 0) {
  message("Step 6: Determined the dataNamesOriginal =", paste0(d[["dataNamesOriginal"]], sep=","))
}
  adp <- oce::as.adp(t, distance, v=abind::abind(u, v, w, errorVelocity, along=3), a=a, q=unknown)


  if (debug > 0) {
    message("Step 7: Created new adp object with time, distance, a, and q, with northward_sea_water_velocity (v) as an array of u (eastward_sea_water_velocity), v, and w (upward_sea_water_velocity), and errorVelocity (indicative_error_from_multibeam_acoustic_doppler_velocity_profiler_in_sea_water)=", dim(adp[['v']]))
    #message("The names of data in the new adp are ", paste0(names(adp[['data']]), sep=","))
    #message("The names of metadata in the new adp are ", paste0(names(adp[['metadata']]), sep=","))
    #message("The names of data in the OLD adp are ", paste0(names(d[['metadata']]), sep=","))

  }

  # FIXME START

  adp <- oceSetMetadata(adp, name="dataNamesOriginal", value=d[["dataNamesOriginal"]])

  message("JAIM the new do=", adp[['dataNamesOriginal']], " and the names data are ", names(adp[['data']]))


  # END FIXME
  for (m in names(d@metadata)) {
    if (m != 'units' & m != 'flags' & m != 'dataNamesOriginal') {
      adp <- oce::oceSetMetadata(adp, name=m, value=d[[m]], note = NULL)
    }
  }

  if (debug > 0) {
    message("Step 8: Just set the metadata units and flags. The names of units are ", names(adp@metadata$units), " the name of flags are ", names(adp@metadata$flags))
  }

  ## depthMinMax
  adp <- oce::oceSetMetadata(adp, 'depthMin', min(depth))
  adp <- oce::oceSetMetadata(adp, 'depthMax', max(depth))
  adp@metadata$source <- 'odf'
  adp@processingLog <- oce::processingLogAppend(adp@processingLog, 'Creation : Data and metadata read into adp object from ODF file')

  return(adp)
}

