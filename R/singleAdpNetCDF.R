#' Convert adp object to netCDF
#'
#' This function exports an adp object to a netCDF using variables
#' and metadata within adp combined. Data variables names and units
#' are named inserted in CF standards using the [standardName()] function.
#'
#'@param adp an adp object from the oce class
#'@param data a data frame of standard name, name, units, and GF3 codes likely from getStandardData
#'@param name name of the netCDF file (not including the extension) to be produced
#'@param destination the specified location to save the NetCDF. By default this is set
#' to the local directory
#'@param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#' @importFrom ncdf4 nc_close ncdim_def ncvar_def nc_create ncvar_put ncatt_put
#' @importFrom rlist list.append
#'@examples
#' \dontrun{
#' library(oceToNetCDF)
#' data <- getStandardData(type="adcp")
#' f1 <- system.file("extdata", "adcp1.ODF", package="oceToNetCDF")
#' f2 <- system.file("extdata", "adcp2.ODF", package="oceToNetCDF")
#' files <- c(f1,f2)
#' adp <- compileOdfToAdp(files)
#' adp2 <- nameReplacement(adp, data=data)
#' adp3 <- structureAdp(adp2)
#' singleAdpNetCDF(adp3, name="test", debug=1, data=data)
#' }
#'
#'@export

singleAdpNetCDF <- function(adp, name, debug=0, data=NULL, destination="."){
  if (is.null(data)) {
    stop("must provide a data frame data, likely from getStandardData()")
  }

  if (!(class(data) =="data.frame")) {
    stop("data provided must be of class data.frame, not ", class(data))
  }

  if (!inherits(adp, "adp")){
    stop("method is only for obects of class '", "adp", "'")
  }
  # file name and path
  ncpath <- destination
  ncname <- name
  ncfname <- paste(ncpath,"/", ncname, ".nc", sep = "")

  # Added 10-SEP-2018 R.Pettipas
  # If the function exits due to an error, close the open netCDF file.
  # Otherwise, the file can't be deleted until the R session is exited.
  on.exit(expr=ncdf4::nc_close(ncout))

  ####setting dimensions and definitions####
  #dimension variables from adp adpect

  if (debug > 0) {
    message("Step 1: About to set dimension")
  }
  time <- as.POSIXct(adp[['time']], tz = 'UTC', origin = '1970-01-01 00:00:00')
  dist <- adp[['distance', 'numeric']]
  lon <- adp[['longitude']]
  lat <- adp[['latitude']]

  #create dimensions
  timedim <- ncdf4::ncdim_def(name="time", units="seconds since 1970-01-01T00:00:00Z", vals=as.double(time))
  distdim <- ncdf4::ncdim_def(name="distance", units="metres", vals=as.double(dist))
  stationdim <- ncdf4::ncdim_def(name="station", units="", vals=as.numeric(length(adp[['station']])))
  dimnchar <- ncdf4::ncdim_def(name='nchar', units='', vals=1:23, create_dimvar = FALSE)

  #set fill value
  FillValue <- 1e35

  #define variables
  if (debug > 0) {
    message("Step 2: About to define variables")
  }

  dlname <- 'longitude'
  lon_def <- ncdf4::ncvar_def(longname= "longitude", units = 'degrees_east', dim = stationdim, name = dlname, prec = 'double')

  dlname <- 'latitude'
  lat_def <- ncdf4::ncvar_def( longname = 'latitude', units = 'degrees_north', dim =  stationdim, name = dlname, prec = 'double')

  dlname <- "time_string"
  ts_def <- ncdf4::ncvar_def("DTUT8601", units = "",dim =  list(dimnchar, timedim), missval = NULL, name =  dlname, prec = "char")

  namesData <- names(adp[['data']])[-which(names(adp[['data']]) %in% c("time", "distance"))]
  DEFS <- NULL
  #browser()
  for (i in seq_along(namesData)) {
    dlname <- namesData[i]
    uName <- gsub("(.+?)(\\_*[0-9].*)", "\\1", namesData[i])
    uName <- gsub(".*average_","",uName)
    longName <- data$name[which(data$standard_name == uName)]
    message("uName =", uName, " for ", i, " and longname =", longName)
    if (longName %in% c("East Component of Current", "North Component of Current", "Vertical Current Speed", "Error Velocity", "Heading")) {
      if (!(is.null(adp[['north']])) && adp[['north']] == "geographic") {
        longName <- paste0(longName, "(geographic)")
      } else {
        longName <- paste0(longName, "(true)", collapse=" ")

      }


    }

    if (length(adp[[namesData[[i]]]]) == timedim$len) {
      DEFS[[i]] <- ncdf4::ncvar_def(name=dlname, units=data$units[which(data$standard_name == uName)], dim=list(timedim), missval=FillValue, longname=longName, prec = "float")


    } else if (dim(adp[[namesData[[i]]]])[2] == distdim$len) {
      DEFS[[i]] <- ncdf4::ncvar_def(name=dlname, units=data$units[which(data$standard_name == uName)], dim=list(timedim, distdim), missval=FillValue, longname=longName, prec = "float")

    }


  }

  defs <- rlist::list.append(DEFS, lon_def, lat_def, ts_def)
  names(defs) <- c(namesData, "latitude", "longitude", "time_string")

  ####writing net CDF####
  #write out definitions to new nc file

  if (debug > 0) {
    message("Step 3: About to write out definitions to nc file using ncdf4::nc_create")
  }

  # vars = an object of class ncvar4 describing the variable to be created, or a vector
  # (or list of such objects to be created)
  #browser()
  ncout <- ncdf4::nc_create(filename=ncfname, vars=defs, force_v4 = TRUE)


  #insert variables into nc file
  if (debug > 0) {
    message("Step 4: About to insert variables to nc file")
  }
  ## varid is an object of class ncvar4. It is the variable to write the data to
  ncdf4::ncvar_put(ncout, ts_def, as.POSIXct(adp[['time']], tz = 'UTC', origin = '1970-01-01 00:00:00'))
  keepDefs <- defs[-(which(names(defs) == "time_string"))]
  for (i in seq_along(keepDefs)) {
    ncdf4::ncvar_put(nc=ncout, varid=keepDefs[[i]], vals=adp[[names(keepDefs)[i]]])

  }

  ####metadata####
  if (debug > 0) {
    message("Step 6: About to write metadata into existing netCDF using ncdf4::ncatt_put")
  }

  bad <- which(names(adp[['metadata']]) %in% c("longitude", "latitude", "units", "flags", "header", "sampleInterval", "codes", "tiltUsed", "threeBeamUsed", "binMappingUsed", "haveBinaryFixedAttitudeHeader",
                                               "haveActualData", "oceBeamUnspreaded", "dataNamesOriginal", "transformationMatrix", "ensembleFile",
                                               "ensembleNumber", "ensembleInFile", "cpuBoardSerialNumber", "dataOffset", "fileType", "north"))
  namesMeta <- names(adp[['metadata']])[-bad]
  #browser()
  for (i in seq_along(namesMeta)) {
    #message("This is for namesMeta = ", namesMeta[i])
    ncdf4::ncatt_put(ncout, 0, namesMeta[i], adp[[namesMeta[i]]])
  }

}
