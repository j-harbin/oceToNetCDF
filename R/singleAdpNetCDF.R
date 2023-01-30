#' Convert adp object to netCDF
#'
#' This function exports an adp object to a netCDF using variables
#' and metadata within adp combined. Data variables names and units
#' are named inserted in CF standards using the [standardName()] function.
#'
#'@param adp an adp object from the oce class
#'@param data a data frame of standard name, name, units, and GF3 codes likely from getCFData
#'@param name name of the netCDF file (not including the extension) to be produced
#'@param destination the specified location to save the NetCDF. By default this is set
#' to the local directory
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#' @importFrom ncdf4 nc_close ncdim_def ncvar_def nc_create ncvar_put ncatt_put
#'@examples
#' \dontrun{
#' library(odfToNetCDF)
#' data <- getCFData(type="adcp")
#' f1 <- system.file("extdata", "adcp1.ODF", package="odfToNetCDF")
#' f2 <- system.file("extdata", "adcp2.ODF", package="odfToNetCDF")
#' files <- c(f1,f2)
#' adp <- compileOdfToAdp(files)
#' singleAdpNetCDF(adp, name="test", debug=1, data=data)
#' }
#'
#'@export

singleAdpNetCDF <- function(adp, name, debug=0, data=NULL, destination="."){
  if (is.null(data)) {
    stop("must provide a data frame data, likely from getCFData()")
  }

  if (!(class(data) =="data.frame")) {
    stop("data provided must be of class data.frame, not ", class(data))
  }

  if (!inherits(adp, "adp")){
    stop("method is only for obects of class '", "adp", "'")
  }
  #file name and path
  ncpath <- destination
  ncname <- name
  ncfname <- paste(ncpath,"/", ncname, ".nc", sep = "")

  ## Sometimes adp objects from ODF files don't store error velocity
  has_err <- ifelse(dim(adp[['v']])[3] > 3, TRUE, FALSE)

  ## Sometimes adp objects from ODF files don't have echo amplitude (a) or percent good (q)
  ## Has a and q?
  has_a <- ifelse('a' %in% names(adp[['data']]), TRUE, FALSE)
  has_q <- ifelse('q' %in% names(adp[['data']]), TRUE, FALSE)
  has_d <- ifelse('depth' %in% names(adp[['data']]), TRUE, FALSE)


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
  #stationdim <- ncdf4::ncdim_def(name="station", units="", vals=1538)
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

    dlname <- "eastward_sea_water_velocity"
    u_def <- ncdf4::ncvar_def(standardName("EWCT",data=data)$standard_name, standardName("EWCT",data=data)$units, list(timedim, distdim), FillValue, dlname, prec = "float")

    dlname <- "northward_sea_water_velocity"
    v_def <- ncdf4::ncvar_def(standardName("NSCT",data=data)$standard_name, standardName("NSCT",data=data)$units, list(timedim, distdim), FillValue, dlname, prec = "float")

    dlname <- "upward_sea_water_velocity"
    w_def <- ncdf4::ncvar_def(standardName("VCSP",data=data)$standard_name, standardName("VCSP",data=data)$units, list(timedim, distdim), FillValue, dlname, prec = "float")

    if (has_err) {
      dlname <- "error_velocity_in_sea_water"
      e_def <- ncdf4::ncvar_def(standardName("ERRV",data=data)$standard_name, standardName("ERRV",data=data)$units, list(timedim, distdim), FillValue, dlname, prec = "float")
    }
    if (has_a) {
       dlname <- "ADCP_echo_intensity_beam_1"
       b1_def <- ncdf4::ncvar_def(paste0(standardName("BEAM",data=data)$standard_name, "_1"), standardName("BEAM", data=data)$units, list(timedim, distdim), FillValue, dlname, prec = "float")
    }
    if (has_q) {
      dlname <- "percent_good_beam_1"
      pg1_def <- ncdf4::ncvar_def(paste0(standardName("PGDP",data=data)$standard_name, "_1"), standardName("PGDP", data=data)$units, list(timedim, distdim), FillValue, dlname, prec = "float")
    }

    if (has_d) {
      dlname <- "sensor_depth_below_sea_surface"
      d_def <- ncdf4::ncvar_def(standardName("DEPH",data=data)$standard_name, standardName("DEPH", data=data)$units, list(timedim), FillValue, dlname, prec = "float")
    }

    dlname <- "time_string"
    ts_def <- ncdf4::ncvar_def("DTUT8601", units = "",dim =  list(dimnchar, timedim), missval = NULL, name =  dlname, prec = "char")

    ####writing net CDF####
    #write out definitions to new nc file

    if (debug > 0) {
      message("Step 3: About to write out definitions to nc file using ncdf4::nc_create")
    }
    if (has_a & has_q & has_err & has_d) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, e_def, b1_def,  pg1_def, lon_def, lat_def, ts_def, d_def), force_v4 = TRUE)
    } else if (has_a & has_q & has_err) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, e_def, b1_def,  pg1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_err & has_a) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, e_def, b1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_d & has_err & has_a) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, e_def, d_def, b1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_err & has_q) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, e_def, pg1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_err & has_q & has_d) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, e_def, d_def, pg1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_a & has_q) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, b1_def,  pg1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_a & has_q & has_d) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, b1_def,  pg1_def, lon_def, lat_def, ts_def, d_def), force_v4 = TRUE)
    } else if (has_a & has_d) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, d_def, b1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_q & has_d) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, d_def, pg1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_err & has_d) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, e_def, d_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_a) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, b1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_q) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, pg1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_err) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, e_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_d) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, d_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    }

  #insert variables into nc file
  if (debug > 0) {
    message("Step 4: About to insert variables to nc file")
  }

  #browser()

  ncdf4::ncvar_put(ncout, u_def, adp[['v']][,,1])
  ncdf4::ncvar_put(ncout, v_def, adp[['v']][,,2])
  ncdf4::ncvar_put(ncout, w_def, adp[['v']][,,3])
  if (has_err) ncdf4::ncvar_put(ncout, e_def, adp[['v']][,,4])
  ncdf4::ncvar_put(ncout, ts_def, as.POSIXct(adp[['time']], tz = 'UTC', origin = '1970-01-01 00:00:00'))
  ncdf4::ncvar_put(ncout, lon_def, adp[['longitude']])
  ncdf4::ncvar_put(ncout, lat_def, adp[['latitude']])
  if (has_d) ncdf4::ncvar_put(ncout, d_def, adp[['depth']])

    if (debug > 0) {
      message("Step 5: About to write data into existing Netcdf using ncdf4::ncvar_put")
    }
  if (has_a) ncdf4::ncvar_put(ncout, b1_def, adp[['a', 'numeric']])
  if (has_q) ncdf4::ncvar_put(ncout, pg1_def, adp[['q', 'numeric']])
    ncdf4::ncvar_put(ncout, ts_def, adp[['time']])

  ####metadata####
  if (debug > 0) {
  message("Step 6: About to write metadata into existing netCDF using ncdf4::ncatt_put")
  }

  bad <- which(names(adp[['metadata']]) %in% c("longitude", "latitude", "units", "flags"))
  namesMeta <- names(adp[['metadata']])[-bad]
  for (i in seq_along(namesMeta)) {
      ncdf4::ncatt_put(ncout, 0, namesMeta[i], adp[[namesMeta[i]]])
  }

}
