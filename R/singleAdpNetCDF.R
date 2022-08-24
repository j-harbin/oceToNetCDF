#' Convert adp object to netCDF
#'
#' This function xxports an adp object to a netCDF using variables
#' and metadata within adp combined
#'
#'@param adp an adp adpect from the oce class
#'@param data a data frame of standard name, name, units, and GF3 codes likely from getData
#'@param name name of the netCDF file to be produced

#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#' @importFrom ncdf4 nc_close ncdim_def ncvar_def nc_create ncvar_put ncatt_put
#'@examples
#' \dontrun{
#' library(odfToNetCDF)
#' library(oce)
#' data <- getData(type="adcp")
#' files <- list.files(pattern="ODF")
#' adp <- compileOdfToAdp(files)
#' singleAdpNetCDF(adp, name="test", debug=1, data=data)
#' }
#'
#'@export

singleAdpNetCDF <- function(adp, name, debug=0, data=NULL){
  if (is.null(data)) {
    stop("must provide a data frame data, likely from getData()")
  }

  if (!(class(data) =="data.frame")) {
    stop("data provided must be of class data.frame, not ", class(data))
  }

  if (!inherits(adp, "adp")){
    stop("method is only for adpects of class '", "adp", "'")
  }
  #file name and path
  ncpath <- "./"
  ncname <- name
  ncfname <- paste(ncpath, ncname, ".nc", sep = "")

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
  timedim <- ncdf4::ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", as.double(time))    #time formatting FIX
  distdim <- ncdf4::ncdim_def("distance", "metres", as.double(dist))
  stationdim <- ncdf4::ncdim_def("station", "", as.numeric(adp[['mooring_number']]))
  londim <- ncdf4::ncdim_def("lon", "degrees_east" , as.double(lon))
  latdim <- ncdf4::ncdim_def("lat", "degrees_north", as.double(lat))
  dimnchar <- ncdf4::ncdim_def('nchar', '', 1:23, create_dimvar = FALSE)


  #set fill value
  FillValue <- 1e35

  if (adp@metadata$source == 'raw'){
    if (debug > 0) {
      message("The metadata source is raw")
    }
    #define variables

    dlname <- 'lon'
    lon_def <- ncdf4::ncvar_def(longname= "longitude", units = 'degrees_east', dim = stationdim, name = dlname, prec = 'double')

    dlname <- 'lat'
    lat_def <- ncdf4::ncvar_def( longname = 'latitude', units = 'degrees_north', dim =  stationdim, name = dlname, prec = 'double')

    dlname <- "eastward_sea_water_velocity"
    u_def <- ncdf4::ncvar_def(standardName("EWCT", data=data)$standard_name, standardName("EWCT",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "northward_sea_water_velocity"
    v_def <- ncdf4::ncvar_def(standardName("NSCT",data=data)$standard_name, standardName("NSCT",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "upward_sea_water_velocity"
    w_def <- ncdf4::ncvar_def(standardName("VCSP",data=data)$standard_name, standardName("VCSP",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "error_velocity_in_sea_water"
    e_def <- ncdf4::ncvar_def(standardName("ERRV",data=data)$standard_name, standardName("ERRV",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_echo_intensity_beam_1"
    b1_def <- ncdf4::ncvar_def(paste0(standardName("BEAM",data=data)$standard_name, "_1"), standardName("BEAM",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_echo_intensity_beam_2"
    b2_def <- ncdf4::ncvar_def(paste0(standardName("BEAM",data=data)$standard_name, "_2"), standardName("BEAM",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_echo_intensity_beam_3"
    b3_def <- ncdf4::ncvar_def(paste0(standardName("BEAM",data=data)$standard_name, "_3"), standardName("BEAM",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_echo_intensity_beam_4"
    b4_def <- ncdf4::ncvar_def(paste0(standardName("BEAM",data=data)$standard_name, "_4"), standardName("BEAM",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_correlation_magnitude_beam_1"
    cm1_def <- ncdf4::ncvar_def("CMAG_01", "counts", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_correlation_magnitude_beam_2"
    cm2_def <- ncdf4::ncvar_def("CMAG_02", "counts", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_correlation_magnitude_beam_3"
    cm3_def <- ncdf4::ncvar_def("CMAG_03", "counts", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_correlation_magnitude_beam_4"
    cm4_def <- ncdf4::ncvar_def("CMAG_04", "counts", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "percent_good_beam_1"
    pg1_def <- ncdf4::ncvar_def(paste0(standardName("PGDP",data=data)$standard_name, "_1"), standardName("PGDP",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "percent_good_beam_2"
    pg2_def <- ncdf4::ncvar_def(paste0(standardName("PGDP",data=data)$standard_name, "_2"), standardName("PGDP",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "percent_good_beam_3"
    pg3_def <- ncdf4::ncvar_def(paste0(standardName("PGDP",data=data)$standard_name, "_3"), standardName("PGDP",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "percent_good_beam_4"
    pg4_def <- ncdf4::ncvar_def(paste0(standardName("PGDP",data=data)$standard_name, "_4"), standardName("PGDP",data=data)$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "pitch"
    p_def <- ncdf4::ncvar_def(standardName("PTCH",data=data)$standard_name, standardName("PTCH",data=data)$units, list( timedim,  stationdim), FillValue, dlname, prec = "float")

    dlname <- "roll"
    r_def <- ncdf4::ncvar_def(standardName("ROLL",data=data)$standard_name, standardName("ROLL",data=data)$units, list(  timedim, stationdim ), FillValue, dlname, prec = "float")

    dlname <- "height of sea surface"
    hght_def <- ncdf4::ncvar_def(standardName("HGHT",data=data)$standard_name, standardName("HGHT",data=data)$units, list(  distdim, stationdim ), FillValue, dlname, prec = "float")

    dlname <- "ADCP Transducer Temp."
    te90_def <- ncdf4::ncvar_def(standardName("TE90",data=data)$standard_name, standardName("TE90",data=data)$units, list( timedim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "instrument depth"
    D_def <- ncdf4::ncvar_def(standardName("DEPH",data=data)$standard_name, standardName("DEPH",data=data)$units, list(timedim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "heading"
    head_def <- ncdf4::ncvar_def("HEAD", "degrees", list(timedim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "pressure"
    pres_def <- ncdf4::ncvar_def(standardName("PRES",data=data)$standard_name, standardName("PRES",data=data)$units, list(timedim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "speed of sound"
    svel_def <- ncdf4::ncvar_def(standardName("SVEL",data=data)$standard_name, standardName("DVEL",data=data)$units, list(timedim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "time_string"
    ts_def <- ncdf4::ncvar_def("DTUT8601", units = "",dim =  list( dimnchar, timedim), missval = NULL, name =  dlname, prec = "char")


    FillValue <- 0

    dlname <- "quality_flag u"
    qc_u_def <- ncdf4::ncvar_def("eastward_sea_water_velocity_QC", "", list(timedim, distdim, stationdim), FillValue, dlname, prec = "integer")

    dlname <- "quality_flag v"
    qc_v_def <- ncdf4::ncvar_def("northward_sea_water_velocity_QC", "", list(timedim, distdim, stationdim), FillValue, dlname, prec = "integer")

    dlname <- "quality_flag w"
    qc_w_def <- ncdf4::ncvar_def("upward_sea_water_velocity_QC", "", list(timedim, distdim, stationdim), FillValue, dlname, prec = "integer")

    ####writing net CDF####
    #write out definitions to new nc file
    ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, e_def, b1_def, b2_def, b3_def, b4_def, pg1_def, pg2_def, pg3_def, pg4_def, p_def, r_def, hght_def, te90_def, D_def, qc_u_def, qc_v_def, qc_w_def, lon_def, lat_def, head_def, pres_def, svel_def, ts_def, cm1_def, cm2_def, cm3_def, cm4_def), force_v4 = TRUE)
  }

  if (adp@metadata$source == 'odf'){
    if (debug > 0) {
      message("The metadata source is odf")
    }
    #define variables

    if (debug > 0) {
      message("Step 2: About to define variables")
    }

    dlname <- 'longitude'
    lon_def <- ncdf4::ncvar_def(longname= "longitude", units = 'degrees_east', dim = stationdim, name = dlname, prec = 'double')

    dlname <- 'latitude'
    lat_def <- ncdf4::ncvar_def( longname = 'latitude', units = 'degrees_north', dim =  stationdim, name = dlname, prec = 'double')

    dlname <- "eastward_sea_water_velocity"
    u_def <- ncdf4::ncvar_def(standardName("EWCT",data=data)$standard_name, "m/sec", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "northward_sea_water_velocity"
    v_def <- ncdf4::ncvar_def(standardName("NSCT",data=data)$standard_name, "m/sec", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "upward_sea_water_velocity"
    w_def <- ncdf4::ncvar_def(standardName("VCSP",data=data)$standard_name, "m/sec", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "error_velocity_in_sea_water"
    e_def <- ncdf4::ncvar_def(standardName("ERRV",data=data)$standard_name, "m/sec", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_echo_intensity_beam_1"

    b1_def <- ncdf4::ncvar_def(paste0(standardName("BEAM",data=data)$standard_name, "_1"), "counts", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")


    dlname <- "percent_good_beam_1"
    pg1_def <- ncdf4::ncvar_def(paste0(standardName("PGDP",data=data)$standard_name, "_1"), "percent", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "time_string"
    ts_def <- ncdf4::ncvar_def("DTUT8601", units = "",dim =  list(dimnchar, timedim), missval = NULL, name =  dlname, prec = "char")

    ####writing net CDF####
    #write out definitions to new nc file

    if (debug > 0) {
      message("Step 3: About to write out definitions to nc file using ncdf4::nc_create")
    }
    ncout <- ncdf4::nc_create(ncfname, list(u_def, v_def, w_def, e_def, b1_def,  pg1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)


  }


  #insert variables into nc file
  if (debug > 0) {
    message("Step 4: About to insert variables to nc file")
  }

  ncdf4::ncvar_put(ncout, u_def, adp[['v']][,,1])
  ncdf4::ncvar_put(ncout, v_def, adp[['v']][,,2])
  ncdf4::ncvar_put(ncout, w_def, adp[['v']][,,3])
  ncdf4::ncvar_put(ncout, e_def, adp[['v']][,,4])
  ncdf4::ncvar_put(ncout, ts_def, as.POSIXct(adp[['time']], tz = 'UTC', origin = '1970-01-01 00:00:00'))
  ncdf4::ncvar_put(ncout, lon_def, adp[['longitude']])
  ncdf4::ncvar_put(ncout, lat_def, adp[['latitude']])

  if (adp@metadata$source == 'raw'){

    if (debug > 0) {
      message("Metadata source is raw")
    }

    ncdf4::ncvar_put(ncout, b1_def, adp[['a', 'numeric']][,,1])
    ncdf4::ncvar_put(ncout, b2_def, adp[['a', 'numeric']][,,2])
    ncdf4::ncvar_put(ncout, b3_def, adp[['a', 'numeric']][,,3])
    ncdf4::ncvar_put(ncout, b4_def, adp[['a', 'numeric']][,,4])

    ncdf4::ncvar_put(ncout, cm1_def, adp[['q', 'numeric']][,,1])
    ncdf4::ncvar_put(ncout, cm2_def, adp[['q', 'numeric']][,,2])
    ncdf4::ncvar_put(ncout, cm3_def, adp[['q', 'numeric']][,,3])
    ncdf4::ncvar_put(ncout, cm4_def, adp[['q', 'numeric']][,,4])
    ncdf4::ncvar_put(ncout, pg1_def, adp[['g', 'numeric']][,,1])
    ncdf4::ncvar_put(ncout, pg2_def, adp[['g', 'numeric']][,,2])
    ncdf4::ncvar_put(ncout, pg3_def, adp[['g', 'numeric']][,,3])
    ncdf4::ncvar_put(ncout, pg4_def, adp[['g', 'numeric']][,,4])
    ncdf4::ncvar_put(ncout, p_def, adp[['pitch']])
    ncdf4::ncvar_put(ncout, r_def, adp[['roll']]*(180/pi))
    ncdf4::ncvar_put(ncout, hght_def, (adp[['distance']] - adp[['sensor_depth']]))
    ncdf4::ncvar_put(ncout, te90_def, adp[['temperature']])
    ncdf4::ncvar_put(ncout, D_def, adp[['depth']])
    ncdf4::ncvar_put(ncout, qc_u_def, adp@metadata$flags$v[,,1])
    ncdf4::ncvar_put(ncout, qc_v_def, adp@metadata$flags$v[,,2])
    ncdf4::ncvar_put(ncout, qc_w_def, adp@metadata$flags$v[,,3])
    ncdf4::ncvar_put(ncout, head_def, adp[['heading']])
    ncdf4::ncvar_put(ncout, pres_def, adp[['pressure']])
    ncdf4::ncvar_put(ncout, svel_def, adp[['soundSpeed']])
    ncdf4::ncvar_put(ncout, ts_def, adp[['time']])
  }
  if (adp@metadata$source == 'odf'){
    if (debug > 0) {
    message("Metadata source is odf")
    }

    if (debug > 0) {
      message("Step 5: About to write data into existing Netcdf using ncdf4::ncvar_put")
    }
    ncdf4::ncvar_put(ncout, b1_def, adp[['a', 'numeric']])
    ncdf4::ncvar_put(ncout, pg1_def, adp[['q', 'numeric']])
    ncdf4::ncvar_put(ncout, ts_def, adp[['time']])

  }

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

  if (adp@metadata$source == 'raw'){
    if (debug > 0) {
      message("Metadata source is raw")
    }

    ##QC VARIABLE
    ncdf4::ncatt_put(ncout, 'standardName("EWCT",data=data)$standard_name', 'ancillary_variables', 'eastward_sea_water_velocity_QC')
    ncdf4::ncatt_put(ncout, 'NSCT', 'ancillary_variables', 'northward_sea_water_velocity_QC')
    ncdf4::ncatt_put(ncout, 'VCSP', 'ancillary_variables', 'upward_sea_water_velocity_QC')

    ####pulled from adp adpect####
    ncdf4::ncatt_put(ncout, 0, "mooring_number", adp[['mooring_number']])

    #       deprecated --- Diana Cardoso 06/01/2018
    #ncdf4::ncatt_put(ncout, 0, "deployment_date", adp[['deployment_time']])
    #ncdf4::ncatt_put(ncout, 0, "recovery_date", adp[['recovery_time']])


    ncdf4::ncatt_put(ncout, 0, "firmware_version", adp[['firmwareVersion']])
    ncdf4::ncatt_put(ncout, 0, "frequency", adp[['frequency']])
    ncdf4::ncatt_put(ncout, 0, "beam_pattern", adp[['beamPattern']])
    ncdf4::ncatt_put(ncout, 0, "janus", adp[['numberOfBeams']])
    ncdf4::ncatt_put(ncout, 0, "pings_per_ensemble", adp[['pingsPerEnsemble']])
    ncdf4::ncatt_put(ncout, 0, "valid_correlation_range", adp[['lowCorrThresh']])
    ncdf4::ncatt_put(ncout, 0, "minmax_percent_good", adp[['percentGdMinimum']])
    ncdf4::ncatt_put(ncout, 0,"minmax_percent_good", "100")
    ncdf4::ncatt_put(ncout, 0, "error_velocity_threshold", paste(adp[['errorVelocityMaximum']], 'm/s'))
    ncdf4::ncatt_put(ncout, 0, "transmit_pulse_length_cm", adp[['xmitPulseLength']]*100)
    ncdf4::ncatt_put(ncout, 0, "false_target_reject_values", adp[['falseTargetThresh']])
    ncdf4::ncatt_put(ncout, 0, "serial_number", adp[['serialNumber']])

    #     deprecated --- Diana Cardoso 06/01/2018
    #ncdf4::ncatt_put(ncout, 0, "transform", adp[['oceCoordinate']])


    ncdf4::ncatt_put(ncout, 0, "data_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, 0, "data_subtype", adp[['data_subtype']])
    ncdf4::ncatt_put(ncout, 0, "coord_system", adp[['oceCoordinate']])
    ncdf4::ncatt_put(ncout, 0, "longitude", adp[['longitude']])
    ncdf4::ncatt_put(ncout, 0, "latitude", adp[['latitude']])
    ncdf4::ncatt_put(ncout, 0, "magnetic_variation", adp[['magnetic_variation']])
    ncdf4::ncatt_put(ncout, 0, "platform", adp[['platform']])
    ncdf4::ncatt_put(ncout, 0, "sounding", adp[['sounding']])
    ncdf4::ncatt_put(ncout, 0, "scientist", adp[['scientist']])
    ncdf4::ncatt_put(ncout, 0, "water_depth", adp[['sounding']])
    ncdf4::ncatt_put(ncout, 0, "delta_t_sec",as.double(adp[['sampling_interval']]))
    ncdf4::ncatt_put(ncout, 0, "pred_accuracy", adp[['velocityResolution']]*1000)
    ncdf4::ncatt_put(ncout, "station", 'longitude', adp[['longitude']])
    ncdf4::ncatt_put(ncout, "station", 'latitude', adp[['latitude']])
    ncdf4::ncatt_put(ncout, "DEPH", "xducer_offset_from_bottom", as.numeric(adp[['sounding']]) - adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, "DEPH", "bin_size", adp[['cellSize']])
    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_2"), "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_2"), "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_2"), "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_3"), "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_3"), "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_3"), "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_4"), "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_4"), "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_4"), "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, "CMAG_01", "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, "CMAG_01", "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, "CMAG_01", "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, "CMAG_02", "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, "CMAG_02", "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, "CMAG_02", "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, "CMAG_03", "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, "CMAG_03", "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, "CMAG_03", "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, "CMAG_04", "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, "CMAG_04", "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, "CMAG_04", "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_2"), "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_2"), "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_2"), "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_3"), "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_3"), "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_3"), "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_4"), "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_4"), "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_4"), "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, "HEAD", "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, "HEAD", "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, "HEAD", "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, "PRES", "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, "PRES", "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, "PRES", "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, "SVEL", "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, "SVEL", "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, "SVEL", "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "generic_name", "u")
    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "generic_name", "v")
    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "generic_name", "w")
    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "generic_name", "w")       #issue in current NC protocol
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM")$standard_name, "_1"), "generic_name", "AGC")
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_2"), "generic_name", "AGC")
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_3"), "generic_name", "AGC")
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_4"), "generic_name", "AGC")
    ncdf4::ncatt_put(ncout, "CMAG_01", "generic_name", "CM")
    ncdf4::ncatt_put(ncout, "CMAG_02", "generic_name", "CM")
    ncdf4::ncatt_put(ncout, "CMAG_03", "generic_name", "CM")
    ncdf4::ncatt_put(ncout, "CMAG_04", "generic_name", "CM")
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "generic_name", "PGd")
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_2"), "generic_name", "PGd")
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_3"), "generic_name", "PGd")
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_4"), "generic_name", "PGd")
    ncdf4::ncatt_put(ncout, "hght", "generic_name", "height")
    ncdf4::ncatt_put(ncout, "hght", "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, "hght", "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, "hght", "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, "DEPH", "generic_name", "depth")
    ncdf4::ncatt_put(ncout, "DEPH", "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, "DEPH", "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, "DEPH", "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, "te90", "generic_name", "temp")
    ncdf4::ncatt_put(ncout, "te90", "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, "te90", "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, "te90", "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, "eastward_sea_water_velocity_QC", "comment", "Quality flag resulting from quality control")
    ncdf4::ncatt_put(ncout, "eastward_sea_water_velocity_QC", "flag_meanings",adp[['flag_meaning']])
    ncdf4::ncatt_put(ncout, "eastward_sea_water_velocity_QC", "flag_values",c(0:9))
    ncdf4::ncatt_put(ncout, "eastward_sea_water_velocity_QC", "References", adp[['flag_references']])
    ncdf4::ncatt_put(ncout, "northward_sea_water_velocity_QC", "comment", "Quality flag resulting from quality control")
    ncdf4::ncatt_put(ncout, "northward_sea_water_velocity_QC", "flag_meanings", adp[['flag_meaning']])
    ncdf4::ncatt_put(ncout, "northward_sea_water_velocity_QC", "flag_values",c(0:9))
    ncdf4::ncatt_put(ncout, "northward_sea_water_velocity_QC", "References", adp[['flag_references']])
    ncdf4::ncatt_put(ncout, "upward_sea_water_velocity_QC", "comment", "Quality flag resulting from quality control")
    ncdf4::ncatt_put(ncout, "upward_sea_water_velocity_QC", "flag_meanings", adp[['flag_meaning']])
    ncdf4::ncatt_put(ncout, "upward_sea_water_velocity_QC", "flag_values",c(0:9))
    ncdf4::ncatt_put(ncout, "upward_sea_water_velocity_QC", "References", adp[['flag_references']])

    #CF conventions

    ncdf4::ncatt_put(ncout, 0, 'Conventions', 'CF-1.7')
    ncdf4::ncatt_put(ncout, 0, "creator_type", "person")
    ncdf4::ncatt_put(ncout, 0, "program", adp[['program']])
    ncdf4::ncatt_put(ncout, 0, "sea_name", adp[['sea_name']])
    ncdf4::ncatt_put(ncout, 0, "time_coverage_start", adp[['time_coverage_start']])
    ncdf4::ncatt_put(ncout, 0, "time_coverage_end", adp[['time_coverage_end']])
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
    ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_positive", 'down')
    ncdf4::ncatt_put(ncout, 0, "institution", adp[['institution']])
    ncdf4::ncatt_put(ncout, 0, "project", adp[['project']])
    ncdf4::ncatt_put(ncout, 0, "history", adp[['history']])
    ncdf4::ncatt_put(ncout, 0 , "flag_meanings", adp[['flag_meaning']])
    ncdf4::ncatt_put(ncout, 0 , "flag_values", c(0:9))
    ncdf4::ncatt_put(ncout, 0, "source", "R code: adcpProcess, github:")
    ncdf4::ncatt_put(ncout, 0, "date_modified", as.character(as.POSIXct(Sys.time(), format = '%Y-%m-%d %H:%M:%sZ', tz = 'UTC')))
    ncdf4::ncatt_put(ncout,0, "_FillValue", "1e35")
    ncdf4::ncatt_put(ncout, 0, "featureType", "timeSeriesProfile") #link to oce adpect? ..... if adp == timeSeriesProfile
    ncdf4::ncatt_put


    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "sdn_parameter_name", "Eastward current velocity (Eulerian) in the water body by moored acoustic doppler current profiler (ADCP)")
    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "sdn_parameter_name", "Northward current velocity (Eulerian) in the water body by moored acoustic doppler current profiler (ADCP)")
    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "sdn_parameter_name", "Upward current velocity in the water body by moored acoustic doppler current profiler (ADCP)")
    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "sdn_parameter_name", "Current velocity error in the water body by moored acoustic doppler current profiler (ADCP)")
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "sdn_parameter_name", "Echo intensity from the water body by moored acoustic doppler current profiler (ADCP) beam 1")
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_2"), "sdn_parameter_name", "Echo intensity from the water body by moored acoustic doppler current profiler (ADCP) beam 2")
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_3"), "sdn_parameter_name", "Echo intensity from the water body by moored acoustic doppler current profiler (ADCP) beam 3")
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_4"), "sdn_parameter_name", "Echo intensity from the water body by moored acoustic doppler current profiler (ADCP) beam 4")
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "sdn_parameter_name", "Acceptable proportion of signal returns by moored acoustic doppler current profiler (ADCP) beam 1")
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_2"), "sdn_parameter_name", "Acceptable proportion of signal returns by moored acoustic doppler current profiler (ADCP) beam 2")
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_3"), "sdn_parameter_name", "Acceptable proportion of signal returns by moored acoustic doppler current profiler (ADCP) beam 3")
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_4"), "sdn_parameter_name", "Acceptable proportion of signal returns by moored acoustic doppler current profiler (ADCP) beam 4")
    ncdf4::ncatt_put(ncout, "DEPH", "sdn_parameter_name", "Depth below surface of the water body")
    ncdf4::ncatt_put(ncout, "te90", "sdn_parameter_name", "Temperature of the water body")
    ncdf4::ncatt_put(ncout, "PTCH", "sdn_parameter_name", "Orientation (pitch) of measurement platform by inclinometer")
    ncdf4::ncatt_put(ncout, "ROLL", "sdn_parameter_name", "Orientation (roll angle) of measurement platform by inclinometer (second sensor)")
    ncdf4::ncatt_put(ncout, "longitude", "sdn_parameter_name", "Longitude east")
    ncdf4::ncatt_put(ncout, "latitude", "sdn_parameter_name", "Latitude north")
    ncdf4::ncatt_put(ncout, "HEAD", "sdn_parameter_name", "Orientation (horizontal relative to true north) of measurement device {heading}")
    ncdf4::ncatt_put(ncout, "PRES", "sdn_parameter_name", "Pressure (spatial co-ordinate) exerted by the water body by profiling pressure sensor and corrected to read zero at sea level")
    ncdf4::ncatt_put(ncout, "SVEL", "sdn_parameter_name", "Sound velocity in the water body by computation from temperature and salinity by unspecified algorithm")
    #ncdf4::ncatt_put(ncout, 'ELTMEP01', "sdn_parameter_name", "Elapsed time (since 1970-01-01T00:00:00Z)")
    ncdf4::ncatt_put(ncout, 'time_string', "sdn_parameter_name", "String corresponding to format 'YYYY-MM-DDThh:mm:ss.sssZ' or other valid ISO8601 string")

  }

  if (adp@metadata$source == 'odf'){
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
    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "sensor_type", adp[['instrumentType']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "sensor_depth", adp[['sensor_depth']])
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "serial_number", adp[['serialNumber']])
    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "generic_name", "u")
    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "generic_name", "v")
    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "generic_name", "w")
    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "generic_name", "w")       #issue in current NC protocol
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "generic_name", "AGC")
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "generic_name", "PGd")
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

    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "sdn_parameter_name", "Eastward current velocity (Eulerian) in the water body by moored acoustic doppler current profiler (ADCP)")
    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "sdn_parameter_name", "Northward current velocity (Eulerian) in the water body by moored acoustic doppler current profiler (ADCP)")
    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "sdn_parameter_name", "Upward current velocity in the water body by moored acoustic doppler current profiler (ADCP)")
    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "sdn_parameter_name", "Current velocity error in the water body by moored acoustic doppler current profiler (ADCP)")
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "sdn_parameter_name", "Echo intensity from the water body by moored acoustic doppler current profiler (ADCP) beam 1")
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "sdn_parameter_name", "Acceptable proportion of signal returns by moored acoustic doppler current profiler (ADCP) beam 1")
    ncdf4::ncatt_put(ncout, "longitude", "sdn_parameter_name", "Longitude east")
    ncdf4::ncatt_put(ncout, "latitude", "sdn_parameter_name", "Latitude north")



  }
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

  ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "data_max", max(adp[['v']][,,4], na.rm = TRUE))
  ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "data_min", min(adp[['v']][,,4], na.rm = TRUE))
  ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "valid_max", 2000)
  ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "valid_min", -2000)

  if(adp@metadata$source == 'raw'){
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "data_min", min(adp[['a', 'numeric']][,,1], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "data_max", max(adp[['a', 'numeric']][,,1], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_2"), "data_min", min(adp[['a' ,'numeric']][,,2], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_2"), "data_max", max(adp[['a', 'numeric']][,,2], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_3"), "data_min", min(adp[['a', 'numeric']][,,3], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_3"), "data_max", max(adp[['a', 'numeric']][,,3], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_4"), "data_min", min(adp[['a', 'numeric']][,,4], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_4"), "data_max", max(adp[['a', 'numeric']][,,4], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, "CMAG_01", "data_min", min(adp[['q', 'numeric']][,,1], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, "CMAG_01", "data_max", max(adp[['q', 'numeric']][,,1], na.rm= TRUE))

    ncdf4::ncatt_put(ncout, "CMAG_02", "data_min", min(adp[['q' ,'numeric']][,,2], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, "CMAG_02", "data_max", max(adp[['q', 'numeric']][,,2], na.rm= TRUE))

    ncdf4::ncatt_put(ncout, "CMAG_03", "data_min", min(adp[['q', 'numeric']][,,3], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, "CMAG_03", "data_max", max(adp[['q', 'numeric']][,,3], na.rm= TRUE))

    ncdf4::ncatt_put(ncout, "CMAG_04", "data_min", min(adp[['q', 'numeric']][,,4], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, "CMAG_04", "data_max", max(adp[['q', 'numeric']][,,4], na.rm= TRUE))

    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "data_min", min(adp[['g', 'numeric']][,,1], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "data_max", max(adp[['g', 'numeric']][,,1], na.rm= TRUE))# eg min 25 % good
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_2"), "data_min", min(adp[['g', 'numeric']][,,2], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_2"), "data_max", max(adp[['g' ,'numeric']][,,2], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_3"), "data_min", min(adp[['g' ,'numeric']][,,3], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_3"), "data_max", max(adp[['g', 'numeric']][,,3], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_4"), "data_min", min(adp[['g', 'numeric']][,,4], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_4"), "data_max", max(adp[['g', 'numeric']][,,4], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, "hght", "data_min", min(adp[['depth', 'data']]))
    ncdf4::ncatt_put(ncout, "hght", "data_max", max(adp[['depth', 'data']]))
    ncdf4::ncatt_put(ncout, "DEPH", "data_min", min(adp[['depth']]))
    ncdf4::ncatt_put(ncout, "DEPH", "data_max", max(adp[['depth']]))
    ncdf4::ncatt_put(ncout, "te90", "data_min", min(adp[['temperature']]))
    ncdf4::ncatt_put(ncout, "te90", "data_max", max(adp[['temperature']]))
    ncdf4::ncatt_put(ncout, "PTCH", "data_min", min(adp[['pitch']]))
    ncdf4::ncatt_put(ncout, "PTCH", "data_max", max(adp[['pitch']]))
    ncdf4::ncatt_put(ncout, "ROLL", "data_min", min(adp[['roll']]))
    ncdf4::ncatt_put(ncout, "ROLL", "data_max", max(adp[['roll']]))
    ncdf4::ncatt_put(ncout, "HEAD", "data_min", min(adp[['heading']]))
    ncdf4::ncatt_put(ncout, "HEAD", "data_max", max(adp[['heading']]))
    ncdf4::ncatt_put(ncout, "PRES", "data_min", min(adp[['pressure']]))
    ncdf4::ncatt_put(ncout, "PRES", "data_max", max(adp[['pressure']]))
    ncdf4::ncatt_put(ncout, "SVEL", "data_min", min(adp[['soundSpeed']]))
    ncdf4::ncatt_put(ncout, "SVEL", "data_max", max(adp[['soundSpeed']]))

  }
  if( adp@metadata$source == 'odf'){
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "data_min", min(adp[['a', 'numeric']], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "data_max", max(adp[['a', 'numeric']], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "data_min", min(adp[['q', 'numeric']], na.rm= TRUE))
    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "data_max", max(adp[['q', 'numeric']], na.rm= TRUE))

  }

}
