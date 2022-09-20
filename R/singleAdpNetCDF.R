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
#' library(oce)
#' data <- getCFData(type="adcp")
#' files <- list.files(pattern="ODF")
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
    stop("method is only for adpects of class '", "adp", "'")
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
  stationdim <- ncdf4::ncdim_def(name="station", units="", vals=as.numeric(adp[['station']]))
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
    u_def <- ncdf4::ncvar_def(standardName("EWCT",data=data)$standard_name, standardName("EWCT",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "northward_sea_water_velocity"
    v_def <- ncdf4::ncvar_def(standardName("NSCT",data=data)$standard_name, standardName("NSCT",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "upward_sea_water_velocity"
    w_def <- ncdf4::ncvar_def(standardName("VCSP",data=data)$standard_name, standardName("VCSP",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    if (has_err) {
      dlname <- "error_velocity_in_sea_water"
      e_def <- ncdf4::ncvar_def(standardName("ERRV",data=data)$standard_name, standardName("ERRV",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")
    }
    if (has_a) {
       dlname <- "ADCP_echo_intensity_beam_1"
       b1_def <- ncdf4::ncvar_def(paste0(standardName("BEAM",data=data)$standard_name, "_1"), standardName("BEAM", data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")
    }
    if (has_q) {
      dlname <- "percent_good_beam_1"
      pg1_def <- ncdf4::ncvar_def(paste0(standardName("PGDP",data=data)$standard_name, "_1"), standardName("PGDP", data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")
    }

    dlname <- "time_string"
    ts_def <- ncdf4::ncvar_def("DTUT8601", units = "",dim =  list(dimnchar, timedim), missval = NULL, name =  dlname, prec = "char")

    ####writing net CDF####
    #write out definitions to new nc file

    if (debug > 0) {
      message("Step 3: About to write out definitions to nc file using ncdf4::nc_create")
    }
    if (has_a & has_q & has_err) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, e_def, b1_def,  pg1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_err & has_a) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, e_def, b1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_err & has_q) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, e_def, pg1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_a & has_q) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, b1_def,  pg1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_a) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, b1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_q) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, pg1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
    } else if (has_err) {
      ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, e_def, lon_def, lat_def, ts_def), force_v4 = TRUE)
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
  ncdf4::ncatt_put(ncout, 'station', attname = 'cf_role',attval =  'timeseries_id')
  ncdf4::ncatt_put(ncout, 'time', attname = 'cf_role', attval = 'profile_id')
  ncdf4::ncatt_put(ncout, 'station', 'standard_name', 'platform_name')
  ncdf4::ncatt_put(ncout, 'time' , 'calendar', 'gregorian')
  ncdf4::ncatt_put(ncout, 'time_string', 'note', 'time values as ISO8601 string, YY-MM-DD hh:mm:ss')
  ncdf4::ncatt_put(ncout, 'time_string', 'time_zone', 'UTC')
  ncdf4::ncatt_put(ncout, 0, 'processing_history',adp[['processing_history']])
  ncdf4::ncatt_put(ncout, 0, "time_coverage_duration", (tail(adp[['time']], n = 1) - adp[['time']][[1]]))
  # deprecated M. Oulliet 4/11/2019
  # ncdf4::ncatt_put(ncout, 0, "time_coverage_duration_units", "days")
  ncdf4::ncatt_put(ncout, 0, "cdm_data_type", "station")
  ncdf4::ncatt_put(ncout, 0, "alternate_pressure_values", adp[['alternate_pressure_values']])
  ncdf4::ncatt_put(ncout, 0, "alternate_pressure_file", adp[['alternate_pressure_file']])
  ncdf4::ncatt_put(ncout, 0, "vertical_separation", adp[['vertical_separation']])
  ncdf4::ncatt_put(ncout, 0, "title", adp[['title']])


    ncdf4::ncatt_put(ncout, 0, "mooring_number", adp[['mooring_number']])
    ncdf4::ncatt_put(ncout, 0, "firmware_version", adp[['firmwareVersion']])
    ncdf4::ncatt_put(ncout, 0, "frequency", adp[['frequency']])
    ncdf4::ncatt_put(ncout, 0, "beam_pattern", adp[['beamPattern']])
    ncdf4::ncatt_put(ncout, 0, "janus", adp[['numberOfBeams']])
    ncdf4::ncatt_put(ncout, 0, "pings_per_ensemble", adp[['pingsPerEnsemble']])
    ncdf4::ncatt_put(ncout, 0, "valid_correlation_range", adp[['lowCorrThresh']])
    ncdf4::ncatt_put(ncout, 0, "minmax_percent_good", adp[['percentGdMinimum']])
    ncdf4::ncatt_put(ncout, 0,"minmax_percent_good", "100")
    ncdf4::ncatt_put(ncout, 0, "error_velocity_threshold", paste(adp[['errorVelocityMaximum']], 'm/s'))
    ncdf4::ncatt_put(ncout, 0, "transmit_pulse_length_cm", adp[['xmitPulseLength']])
    ncdf4::ncatt_put(ncout, 0, "false_target_reject_values", adp[['falseTargetThresh']])
    ncdf4::ncatt_put(ncout, 0, "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, 0, "data_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, 0, "data_subtype", adp[['model']])
    ncdf4::ncatt_put(ncout, 0, "coord_system", adp[['oceCoordinate']])
    ncdf4::ncatt_put(ncout, 0, "longitude", adp[['longitude']])
    ncdf4::ncatt_put(ncout, 0, "latitude", adp[['latitude']])
    ncdf4::ncatt_put(ncout, 0, "magnetic_variation", adp[['magneticVariation']])
    ncdf4::ncatt_put(ncout, 0, "platform", adp[['platform']])
    ncdf4::ncatt_put(ncout, 0, "sounding", adp[['sounding']])
    ncdf4::ncatt_put(ncout, 0, "scientist", adp[['scientist']])
    ncdf4::ncatt_put(ncout, 0, "water_depth", adp[['water_depth']])
    ncdf4::ncatt_put(ncout, 0, "delta_t_sec", as.double(adp[['sampling_interval']]))
    ncdf4::ncatt_put(ncout, 0, "pred_accuracy", adp[['velocityResolution']])
    ncdf4::ncatt_put(ncout, 0, "cell_size", adp[['cellSize']])
    ncdf4::ncatt_put(ncout, 0, "deployment_type", adp[['deploymentType']])
    ncdf4::ncatt_put(ncout, 0, "filename", gsub(".*M","",adp[['filename']]))
    ncdf4::ncatt_put(ncout, 0, "model", gsub(".*M","",adp[['model']]))
    ncdf4::ncatt_put(ncout, 0, "orientation", gsub(".*M","",adp[['orientation']]))
    ncdf4::ncatt_put(ncout, 0, "serialNumber", gsub(".*M","",adp[['serialNumber']]))
    ncdf4::ncatt_put(ncout, 0, "water_depth", gsub(".*M","",adp[['waterDepth']]))
    ncdf4::ncatt_put(ncout, 0, "country_institute_code", gsub(".*M","",adp[['countryInstituteCode']]))
    ncdf4::ncatt_put(ncout, 0, "institute", gsub(".*M","",adp[['institute']]))
    ncdf4::ncatt_put(ncout, 0, "number_of_beams", gsub(".*M","",adp[['numberOfBeams']]))
    ncdf4::ncatt_put(ncout, 0, "sample_interval", gsub(".*M","",adp[['sampleInterval']]))
    ncdf4::ncatt_put(ncout, 0, "ship", gsub(".*M","",adp[['ship']]))
    ncdf4::ncatt_put(ncout, 0, "station", gsub(".*M","",adp[['station']]))
    ncdf4::ncatt_put(ncout, 0, "cruise", gsub(".*M","",adp[['cruise']]))
    ncdf4::ncatt_put(ncout, 0, "type", gsub(".*M","",adp[['type']]))
    ncdf4::ncatt_put(ncout, 0, "cruise_number", gsub(".*M","",adp[['cruiseNumber']]))
    ncdf4::ncatt_put(ncout, 0, "depth_off_bottom", gsub(".*M","",adp[['depthOffBottom']]))
    ncdf4::ncatt_put(ncout, 0, "source", gsub(".*M","",adp[['source']]))

    #FIXME: should be pulled from odf...not in adpect... issue with oce read.odf
    ncdf4::ncatt_put(ncout, "distance", "xducer_offset_from_bottom", adp[['depth_off_bottom']])

    ncdf4::ncatt_put(ncout, "distance", "bin_size", adp[['cellSize']])
    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "serial_number", adp[['serialNumber']])
    if (has_err) {
      ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "sensor_type", adp[['instrumentType']])
      ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "sensor_depth", adp[['sensor_depth']])
      ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "serial_number", adp[['serialNumber']])
    }
    if (has_a) {
      ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "sensor_type", adp[['instrumentType']])
      ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "sensor_depth", adp[['sensor_depth']])
      ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "serial_number", adp[['serialNumber']])
    }
    if (has_q) {
      ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "sensor_type", adp[['instrumentType']])
      ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "sensor_depth", adp[['sensor_depth']])
      ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "serial_number", adp[['serialNumber']])
    }
    #ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "generic_name", "u")
    #ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "generic_name", "v")
    #ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "generic_name", "w")
    #ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "generic_name", standardName("ERRV", data=data)$name)       #issue in current NC protocol
    #ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "generic_name", "AGC")
    #ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "generic_name", "PGd")
    #CF

    ncdf4::ncatt_put(ncout, 0, 'Conventions', 'CF-1.6')
    ncdf4::ncatt_put(ncout, 0, "creator_type", "person")
    ncdf4::ncatt_put(ncout, 0, "program", adp[['program']])
    ncdf4::ncatt_put(ncout, 0, "sea_name", adp[['sea_name']])
    ncdf4::ncatt_put(ncout, 0, "time_coverage_start", adp[['deployment_time']])
    ncdf4::ncatt_put(ncout, 0, "time_coverage_end", adp[['recovery_time']])
    ncdf4::ncatt_put(ncout, 0, "geospatial_lat_min", adp[['latitude']])
    ncdf4::ncatt_put(ncout, 0, "geospatial_lat_max", adp[['latitude']])
    ncdf4::ncatt_put(ncout, 0, "geospatial_lat_units", "degrees_north")
    ncdf4::ncatt_put(ncout, 0, "geospatial_lon_min", adp[['longitude']])
    ncdf4::ncatt_put(ncout, 0, "geospatial_lon_max", adp[['longitude']])
    ncdf4::ncatt_put(ncout, 0, "geospatial_lon_units", "degrees_east")
    if (adp[['orientation']] == 'up'){
      ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_min", adp[['sensor_depth']] + max(adp[['distance']], na.rm = TRUE))
      ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_max", adp[['sensor_depth']] + min(adp[['distance']], na.rm = TRUE))
    }
    if (adp[['orientation']] == 'down'){
      ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_min", adp[['sensor_depth']] + min(adp[['distance']], na.rm = TRUE))
      ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_max", adp[['sensor_depth']] + max(adp[['distance']], na.rm = TRUE))
    }
    ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_units", "metres")
    ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_positive", adp[['orientation']])     #eg up or down
    ncdf4::ncatt_put(ncout, 0, "institution", adp[['institution']])
    ncdf4::ncatt_put(ncout, 0, "creator_name", adp[['creator_name']])
    ncdf4::ncatt_put(ncout, 0, "creator_url", adp[['creator_url']])
    ncdf4::ncatt_put(ncout, 0, "creator_email", adp[['creator_email']])
    ncdf4::ncatt_put(ncout, 0, "project", adp[['project']])
    ncdf4::ncatt_put(ncout, 0, "processing_history", adp[['processing_history']])
    ncdf4::ncatt_put(ncout, 0 , "flag_meanings", adp[['flag_meaning']])
    ncdf4::ncatt_put(ncout, 0 , "flag_values", c(0:9))
    ncdf4::ncatt_put(ncout, 0, "source", "R code: adcpProcess, github:")
    ncdf4::ncatt_put(ncout, 0, "date_modified", as.character(as.POSIXct(Sys.time(), format = '%Y-%m-%d %H:%M:%sZ', tz = 'UTC')))
    ncdf4::ncatt_put(ncout,0, "_FillValue", "1e35")
    ncdf4::ncatt_put(ncout, 0, "featureType", "timeSeriesProfile") #link to oce adpect? ..... if adp == timeSeriesProfile

    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "generic_parameter_name", "Eastward current velocity (Eulerian) in the water body by moored acoustic doppler current profiler (ADCP)")
    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "generic_parameter_name", "Northward current velocity (Eulerian) in the water body by moored acoustic doppler current profiler (ADCP)")
    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "generic_parameter_name", "Upward current velocity in the water body by moored acoustic doppler current profiler (ADCP)")
    if (has_err) ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "generic_parameter_name", "Current velocity error in the water body by moored acoustic doppler current profiler (ADCP)")
    if (has_a) ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "generic_parameter_name", "Echo intensity from the water body by moored acoustic doppler current profiler (ADCP) beam 1")
    if (has_q) ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "generic_parameter_name", "Acceptable proportion of signal returns by moored acoustic doppler current profiler (ADCP) beam 1")
    ncdf4::ncatt_put(ncout, "longitude", "generic_parameter_name", "Longitude east")
    ncdf4::ncatt_put(ncout, "latitude", "generic_parameter_name", "Latitude north")



  if(!is.null(adp[['publisher_name']])){
    ncdf4::ncatt_put(ncout, 0, "publisher_name", adp[['publisher_name']])
  }
  if(!is.null(adp[['publisher_url']])){
    ncdf4::ncatt_put(ncout, 0, "publisher_url", adp[['publisher_url']])
  }
  if(!is.null(adp[['publisher_email']])){
    ncdf4::ncatt_put(ncout, 0, "publisher_email", adp[['publisher_email']])
  }

  ####
  ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "data_max", max(adp[['v']][,,1], na.rm = TRUE))
  ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "data_min", min(adp[['v']][,,1], na.rm = TRUE))
  ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "valid_max", 1000)
  ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "valid_min", -1000)

  ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "data_max", max(adp[['v']][,,2], na.rm = TRUE))
  ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "data_min", min(adp[['v']][,,2], na.rm = TRUE))
  ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "valid_max", 1000)
  ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "valid_min", -1000)

  ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "data_max", max(adp[['v']][,,3], na.rm = TRUE))
  ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "data_min", min(adp[['v']][,,3], na.rm = TRUE))
  ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "valid_max", 1000)
  ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "valid_min", -1000)

  if (has_err) {
    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "data_max", max(adp[['v']][,,4], na.rm = TRUE))
    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "data_min", min(adp[['v']][,,4], na.rm = TRUE))
    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "valid_max", 2000)
    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "valid_min", -2000)
  }
  if (has_a) {
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "data_min", min(adp[['a', 'numeric']], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "data_max", max(adp[['a', 'numeric']], na.rm= TRUE))
  }
  if (has_q) {
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "data_min", min(adp[['q', 'numeric']], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "data_max", max(adp[['q', 'numeric']], na.rm= TRUE))
  }

}
