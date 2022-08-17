#'ADCP Processing step 4.1
#'@description Exports an adp object to a net cdf using variables and metadata
#'  within adp combined with optional additional metadata see details in
#'  \code{\link[ncdf4:ncdf4]{ncdf4}} package
#'
#'@param adp an adp adpect from the oce class
#'@param name name of the NetCDF file to be produced
#'@param metadata csv file listing metadata names and values to be inserted into
#'  global attributes of net CDF
#'
#'@export

singleAdpNetCDF <- function(adp, name,  metadata, debug=0){
  if (!inherits(adp, "adp")){
    stop("method is only for adpects of class '", "adp", "'")
  }
  if (missing(metadata)){
    warning('no metadata supplied')
  }
  #file name and path
  ncpath <- "./"
  ncname <- name
  ncfname <- paste(ncpath, ncname, ".nc", sep = "")

  # Added 10-SEP-2018 R.Pettipas
  # If the function exits due to an error, close the open NetCDF file.
  # Otherwise, the file can't be deleted until the R session is exited.
  on.exit(expr=nc_close(ncout))

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
  timedim <- ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", as.double(time))    #time formatting FIX
  distdim <- ncdim_def("distance", "metres", as.double(dist))
  stationdim <- ncdim_def("station", "", as.numeric(adp[['mooring_number']]))
  londim <- ncdim_def("lon", "degrees_east" , as.double(lon))
  latdim <- ncdim_def("lat", "degrees_north", as.double(lat))
  dimnchar <- ncdim_def('nchar', '', 1:23, create_dimvar = FALSE)


  #set fill value
  FillValue <- 1e35

  if (adp@metadata$source == 'raw'){
    if (debug > 0) {
      message("The metadata source is raw")
    }
    #define variables

    dlname <- 'lon'
    lon_def <- ncvar_def(longname= "longitude", units = 'degrees_east', dim = stationdim, name = dlname, prec = 'double')

    dlname <- 'lat'
    lat_def <- ncvar_def( longname = 'latitude', units = 'degrees_north', dim =  stationdim, name = dlname, prec = 'double')

    dlname <- "eastward_sea_water_velocity"
    u_def <- ncvar_def(standard_name("EWCT")$standard_name, standard_name("EWCT")$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "northward_sea_water_velocity"
    v_def <- ncvar_def(standard_name("NSCT")$standard_name, standard_name("NSCT")$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "upward_sea_water_velocity"
    w_def <- ncvar_def(standard_name("VCSP")$standard_name, standard_name("VCSP")$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    #dlname <- "time_02"
    #t_def <- ncvar_def("ELTMEP01", "seconds since 1970-01-01T00:00:00Z", list( stationdim, timedim), FillValue, dlname, prec = 'double')

    dlname <- "error_velocity_in_sea_water"
    e_def <- ncvar_def(standard_name("ERRV")$standard_name, standard_name("ERRV")$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_echo_intensity_beam_1"
    b1_def <- ncvar_def(paste0(standard_name("BEAM")$standard_name, "_1"), standard_name("BEAM")$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_echo_intensity_beam_2"
    b2_def <- ncvar_def(paste0(standard_name("BEAM")$standard_name, "_2"), standard_name("BEAM")$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_echo_intensity_beam_3"
    b3_def <- ncvar_def(paste0(standard_name("BEAM")$standard_name, "_3"), standard_name("BEAM")$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_echo_intensity_beam_4"
    b4_def <- ncvar_def(paste0(standard_name("BEAM")$standard_name, "_4"), standard_name("BEAM")$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_correlation_magnitude_beam_1"
    cm1_def <- ncvar_def("CMAG_01", "counts", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_correlation_magnitude_beam_2"
    cm2_def <- ncvar_def("CMAG_02", "counts", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_correlation_magnitude_beam_3"
    cm3_def <- ncvar_def("CMAG_03", "counts", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_correlation_magnitude_beam_4"
    cm4_def <- ncvar_def("CMAG_04", "counts", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "percent_good_beam_1"
    pg1_def <- ncvar_def(paste0(standard_name("PGDP")$standard_name, "_1"), standard_name("PGDP")$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "percent_good_beam_2"
    pg2_def <- ncvar_def(paste0(standard_name("PGDP")$standard_name, "_2"), standard_name("PGDP")$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "percent_good_beam_3"
    pg3_def <- ncvar_def(paste0(standard_name("PGDP")$standard_name, "_3"), standard_name("PGDP")$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "percent_good_beam_4"
    pg4_def <- ncvar_def(paste0(standard_name("PGDP")$standard_name, "_4"), standard_name("PGDP")$units, list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "pitch"
    p_def <- ncvar_def(standard_name("PTCH")$standard_name, standard_name("PTCH")$units, list( timedim,  stationdim), FillValue, dlname, prec = "float")

    dlname <- "roll"
    r_def <- ncvar_def(standard_name("ROLL")$standard_name, standard_name("ROLL")$units, list(  timedim, stationdim ), FillValue, dlname, prec = "float")

    dlname <- "height of sea surface"
    hght_def <- ncvar_def(standard_name("HGHT")$standard_name, standard_name("HGHT")$units, list(  distdim, stationdim ), FillValue, dlname, prec = "float")

    dlname <- "ADCP Transducer Temp."
    te90_def <- ncvar_def(standard_name("TE90")$standard_name, standard_name("TE90")$units, list( timedim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "instrument depth"
    D_def <- ncvar_def(standard_name("DEPH")$standard_name, standard_name("DEPTH")$units, list(timedim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "heading"
    head_def <- ncvar_def("HEAD", "degrees", list(timedim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "pressure"
    pres_def <- ncvar_def(standard_name("PRES")$standard_name, standard_name("PRES")$units, list(timedim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "speed of sound"
    svel_def <- ncvar_def(standard_name("SVEL")$standard_name, standard_name("DVEL")$units, list(timedim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "time_string"
    ts_def <- ncvar_def("DTUT8601", units = "",dim =  list( dimnchar, timedim), missval = NULL, name =  dlname, prec = "char")


    FillValue <- 0

    dlname <- "quality_flag u"
    qc_u_def <- ncvar_def("eastward_sea_water_velocity_QC", "", list(timedim, distdim, stationdim), FillValue, dlname, prec = "integer")

    dlname <- "quality_flag v"
    qc_v_def <- ncvar_def("northward_sea_water_velocity_QC", "", list(timedim, distdim, stationdim), FillValue, dlname, prec = "integer")

    dlname <- "quality_flag w"
    qc_w_def <- ncvar_def("upward_sea_water_velocity_QC", "", list(timedim, distdim, stationdim), FillValue, dlname, prec = "integer")

    ####writing net CDF####
    #write out definitions to new nc file
    ncout <- nc_create(ncfname, list(u_def, v_def, w_def, e_def, b1_def, b2_def, b3_def, b4_def, pg1_def, pg2_def, pg3_def, pg4_def, p_def, r_def, hght_def, te90_def, D_def, qc_u_def, qc_v_def, qc_w_def, lon_def, lat_def, head_def, pres_def, svel_def, ts_def, cm1_def, cm2_def, cm3_def, cm4_def), force_v4 = TRUE)
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
    lon_def <- ncvar_def(longname= "longitude", units = 'degrees_east', dim = stationdim, name = dlname, prec = 'double')

    dlname <- 'latitude'
    lat_def <- ncvar_def( longname = 'latitude', units = 'degrees_north', dim =  stationdim, name = dlname, prec = 'double')

    dlname <- "eastward_sea_water_velocity"
    u_def <- ncvar_def(standard_name("EWCT")$standard_name, "m/sec", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "northward_sea_water_velocity"
    v_def <- ncvar_def(standard_name("NSCT")$standard_name, "m/sec", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "upward_sea_water_velocity"
    w_def <- ncvar_def(standard_name("VCSP")$standard_name, "m/sec", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    #dlname <- "time_02"
    #t_def <- ncvar_def("ELTMEP01", "seconds since 1970-01-01T00:00:00Z", list( stationdim, timedim), FillValue, dlname, prec = "double")

    dlname <- "error_velocity_in_sea_water"
    e_def <- ncvar_def(standard_name("ERRV")$standard_name, "m/sec", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "ADCP_echo_intensity_beam_1"

    b1_def <- ncvar_def(paste0(standard_name("BEAM")$standard_name, "_1"), "counts", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")


    dlname <- "percent_good_beam_1"
    pg1_def <- ncvar_def(paste0(standard_name("PGDP")$standard_name, "_1"), "percent", list(timedim, distdim, stationdim), FillValue, dlname, prec = "float")

    dlname <- "time_string"
    ts_def <- ncvar_def("DTUT8601", units = "",dim =  list(dimnchar, timedim), missval = NULL, name =  dlname, prec = "char")

    ####writing net CDF####
    #write out definitions to new nc file

    if (debug > 0) {
      message("Step 3: About to write out definitions to nc file using nc_create")
    }
    ncout <- nc_create(ncfname, list(u_def, v_def, w_def, e_def, b1_def,  pg1_def, lon_def, lat_def, ts_def), force_v4 = TRUE)


  }


  #insert variables into nc file
  if (debug > 0) {
    message("Step 4: About to insert variables to nc file")
  }

  ncvar_put(ncout, u_def, adp[['v']][,,1])
  ncvar_put(ncout, v_def, adp[['v']][,,2])
  ncvar_put(ncout, w_def, adp[['v']][,,3])
  ncvar_put(ncout, e_def, adp[['v']][,,4])
  ncvar_put(ncout, ts_def, as.POSIXct(adp[['time']], tz = 'UTC', origin = '1970-01-01 00:00:00'))
  ncvar_put(ncout, lon_def, adp[['longitude']])
  ncvar_put(ncout, lat_def, adp[['latitude']])

  if (adp@metadata$source == 'raw'){

    if (debug > 0) {
      message("Metadata source is raw")
    }

    ncvar_put(ncout, b1_def, adp[['a', 'numeric']][,,1])
    ncvar_put(ncout, b2_def, adp[['a', 'numeric']][,,2])
    ncvar_put(ncout, b3_def, adp[['a', 'numeric']][,,3])
    ncvar_put(ncout, b4_def, adp[['a', 'numeric']][,,4])

    ncvar_put(ncout, cm1_def, adp[['q', 'numeric']][,,1])
    ncvar_put(ncout, cm2_def, adp[['q', 'numeric']][,,2])
    ncvar_put(ncout, cm3_def, adp[['q', 'numeric']][,,3])
    ncvar_put(ncout, cm4_def, adp[['q', 'numeric']][,,4])
    ncvar_put(ncout, pg1_def, adp[['g', 'numeric']][,,1])
    ncvar_put(ncout, pg2_def, adp[['g', 'numeric']][,,2])
    ncvar_put(ncout, pg3_def, adp[['g', 'numeric']][,,3])
    ncvar_put(ncout, pg4_def, adp[['g', 'numeric']][,,4])
    ncvar_put(ncout, p_def, adp[['pitch']])
    ncvar_put(ncout, r_def, adp[['roll']]*(180/pi))
    ncvar_put(ncout, hght_def, (adp[['distance']] - adp[['sensor_depth']]))
    ncvar_put(ncout, te90_def, adp[['temperature']])
    ncvar_put(ncout, D_def, adp[['depth']])
    ncvar_put(ncout, qc_u_def, adp@metadata$flags$v[,,1])
    ncvar_put(ncout, qc_v_def, adp@metadata$flags$v[,,2])
    ncvar_put(ncout, qc_w_def, adp@metadata$flags$v[,,3])
    ncvar_put(ncout, head_def, adp[['heading']])
    ncvar_put(ncout, pres_def, adp[['pressure']])
    ncvar_put(ncout, svel_def, adp[['soundSpeed']])
    ncvar_put(ncout, ts_def, adp[['time']])
  }
  if (adp@metadata$source == 'odf'){
    if (debug > 0) {
    message("Metadata source is odf")
    }

    if (debug > 0) {
      message("Step 5: About to write data into existing Netcdf using ncvar_put")
    }
    ncvar_put(ncout, b1_def, adp[['a', 'numeric']])
    ncvar_put(ncout, pg1_def, adp[['q', 'numeric']])
    ncvar_put(ncout, ts_def, adp[['time']])

  }

  ####metadata####
  if (debug > 0) {
  message("Step 6: About to write metadata into existing netCDF using ncatt_put")
  }
  ncatt_put(ncout, 'station', attname = 'cf_role',attval =  'timeseries_id')
  ncatt_put(ncout, 'time', attname = 'cf_role', attval = 'profile_id')
  ncatt_put(ncout, 'station', 'standard_name', 'platform_name')
  ncatt_put(ncout, 'time' , 'calendar', 'gregorian')
  ncatt_put(ncout, 'time_string', 'note', 'time values as ISO8601 string, YY-MM-DD hh:mm:ss')
  ncatt_put(ncout, 'time_string', 'time_zone', 'UTC')
  ncatt_put(ncout, 0, 'processing_history',adp[['processing_history']])
  ncatt_put(ncout, 0, "time_coverage_duration", (tail(adp[['time']], n = 1) - adp[['time']][[1]]))
  # deprecated M. Oulliet 4/11/2019
  # ncatt_put(ncout, 0, "time_coverage_duration_units", "days")
  ncatt_put(ncout, 0, "cdm_data_type", "station")
  ncatt_put(ncout, 0, "alternate_pressure_values", adp[['alternate_pressure_values']])
  ncatt_put(ncout, 0, "alternate_pressure_file", adp[['alternate_pressure_file']])
  ncatt_put(ncout, 0, "vertical_separation", adp[['vertical_separation']])
  ncatt_put(ncout, 0, "title", adp[['title']])

  if (adp@metadata$source == 'raw'){
    if (debug > 0) {
      message("Metadata source is raw")
    }

    ##QC VARIABLE
    ncatt_put(ncout, 'standard_name("EWCT")$standard_name', 'ancillary_variables', 'eastward_sea_water_velocity_QC')
    ncatt_put(ncout, 'NSCT', 'ancillary_variables', 'northward_sea_water_velocity_QC')
    ncatt_put(ncout, 'VCSP', 'ancillary_variables', 'upward_sea_water_velocity_QC')

    ####pulled from adp adpect####
    ncatt_put(ncout, 0, "mooring_number", adp[['mooring_number']])

    #       deprecated --- Diana Cardoso 06/01/2018
    #ncatt_put(ncout, 0, "deployment_date", adp[['deployment_time']])
    #ncatt_put(ncout, 0, "recovery_date", adp[['recovery_time']])


    ncatt_put(ncout, 0, "firmware_version", adp[['firmwareVersion']])
    ncatt_put(ncout, 0, "frequency", adp[['frequency']])
    ncatt_put(ncout, 0, "beam_pattern", adp[['beamPattern']])
    ncatt_put(ncout, 0, "janus", adp[['numberOfBeams']])
    ncatt_put(ncout, 0, "pings_per_ensemble", adp[['pingsPerEnsemble']])
    ncatt_put(ncout, 0, "valid_correlation_range", adp[['lowCorrThresh']])
    ncatt_put(ncout, 0, "minmax_percent_good", adp[['percentGdMinimum']])
    ncatt_put(ncout, 0,"minmax_percent_good", "100")
    ncatt_put(ncout, 0, "error_velocity_threshold", paste(adp[['errorVelocityMaximum']], 'm/s'))
    ncatt_put(ncout, 0, "transmit_pulse_length_cm", adp[['xmitPulseLength']]*100)
    ncatt_put(ncout, 0, "false_target_reject_values", adp[['falseTargetThresh']])
    ncatt_put(ncout, 0, "serial_number", adp[['serialNumber']])

    #     deprecated --- Diana Cardoso 06/01/2018
    #ncatt_put(ncout, 0, "transform", adp[['oceCoordinate']])


    ncatt_put(ncout, 0, "data_type", adp[['instrumentType']])
    ncatt_put(ncout, 0, "data_subtype", adp[['data_subtype']])
    ncatt_put(ncout, 0, "coord_system", adp[['oceCoordinate']])
    ncatt_put(ncout, 0, "longitude", adp[['longitude']])
    ncatt_put(ncout, 0, "latitude", adp[['latitude']])
    ncatt_put(ncout, 0, "magnetic_variation", adp[['magnetic_variation']])
    ncatt_put(ncout, 0, "platform", adp[['platform']])
    ncatt_put(ncout, 0, "sounding", adp[['sounding']])
    ncatt_put(ncout, 0, "scientist", adp[['scientist']])
    ncatt_put(ncout, 0, "water_depth", adp[['sounding']])
    ncatt_put(ncout, 0, "delta_t_sec",as.double(adp[['sampling_interval']]))
    ncatt_put(ncout, 0, "pred_accuracy", adp[['velocityResolution']]*1000)
    ncatt_put(ncout, "station", 'longitude', adp[['longitude']])
    ncatt_put(ncout, "station", 'latitude', adp[['latitude']])
    ncatt_put(ncout, "DEPH", "xducer_offset_from_bottom", as.numeric(adp[['sounding']]) - adp[['sensor_depth']])
    ncatt_put(ncout, "DEPH", "bin_size", adp[['cellSize']])
    ncatt_put(ncout, standard_name("EWCT")$standard_name, "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, standard_name("EWCT")$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, standard_name("EWCT")$standard_name, "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, standard_name("NSCT")$standard_name, "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, standard_name("NSCT")$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, standard_name("NSCT")$standard_name, "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, standard_name("VCSP")$standard_name, "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, standard_name("VCSP")$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, standard_name("VCSP")$standard_name, "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, standard_name("ERRV")$standard_name, "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, standard_name("ERRV")$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, standard_name("ERRV")$standard_name, "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_1"), "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_1"), "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_1"), "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_2"), "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_2"), "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_2"), "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_3"), "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_3"), "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_3"), "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_4"), "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_4"), "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_4"), "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, "CMAG_01", "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, "CMAG_01", "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, "CMAG_01", "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, "CMAG_02", "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, "CMAG_02", "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, "CMAG_02", "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, "CMAG_03", "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, "CMAG_03", "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, "CMAG_03", "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, "CMAG_04", "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, "CMAG_04", "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, "CMAG_04", "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_1"), "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_1"), "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_1"), "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_2"), "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_2"), "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_2"), "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_3"), "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_3"), "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_3"), "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_4"), "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_4"), "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_4"), "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, "HEAD", "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, "HEAD", "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, "HEAD", "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, "PRES", "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, "PRES", "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, "PRES", "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, "SVEL", "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, "SVEL", "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, "SVEL", "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, standard_name("EWCT")$standard_name, "generic_name", "u")
    ncatt_put(ncout, standard_name("NSCT")$standard_name, "generic_name", "v")
    ncatt_put(ncout, standard_name("VCSP")$standard_name, "generic_name", "w")
    ncatt_put(ncout, standard_name("ERRV")$standard_name, "generic_name", "w")       #issue in current NC protocol
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_1"), "generic_name", "AGC")
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_2"), "generic_name", "AGC")
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_3"), "generic_name", "AGC")
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_4"), "generic_name", "AGC")
    ncatt_put(ncout, "CMAG_01", "generic_name", "CM")
    ncatt_put(ncout, "CMAG_02", "generic_name", "CM")
    ncatt_put(ncout, "CMAG_03", "generic_name", "CM")
    ncatt_put(ncout, "CMAG_04", "generic_name", "CM")
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_1"), "generic_name", "PGd")
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_2"), "generic_name", "PGd")
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_3"), "generic_name", "PGd")
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_4"), "generic_name", "PGd")
    ncatt_put(ncout, "hght", "generic_name", "height")
    ncatt_put(ncout, "hght", "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, "hght", "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, "hght", "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, "DEPH", "generic_name", "depth")
    ncatt_put(ncout, "DEPH", "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, "DEPH", "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, "DEPH", "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, "te90", "generic_name", "temp")
    ncatt_put(ncout, "te90", "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, "te90", "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, "te90", "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, "eastward_sea_water_velocity_QC", "comment", "Quality flag resulting from quality control")
    ncatt_put(ncout, "eastward_sea_water_velocity_QC", "flag_meanings",adp[['flag_meaning']])
    ncatt_put(ncout, "eastward_sea_water_velocity_QC", "flag_values",c(0:9))
    ncatt_put(ncout, "eastward_sea_water_velocity_QC", "References", adp[['flag_references']])
    ncatt_put(ncout, "northward_sea_water_velocity_QC", "comment", "Quality flag resulting from quality control")
    ncatt_put(ncout, "northward_sea_water_velocity_QC", "flag_meanings", adp[['flag_meaning']])
    ncatt_put(ncout, "northward_sea_water_velocity_QC", "flag_values",c(0:9))
    ncatt_put(ncout, "northward_sea_water_velocity_QC", "References", adp[['flag_references']])
    ncatt_put(ncout, "upward_sea_water_velocity_QC", "comment", "Quality flag resulting from quality control")
    ncatt_put(ncout, "upward_sea_water_velocity_QC", "flag_meanings", adp[['flag_meaning']])
    ncatt_put(ncout, "upward_sea_water_velocity_QC", "flag_values",c(0:9))
    ncatt_put(ncout, "upward_sea_water_velocity_QC", "References", adp[['flag_references']])

    #CF conventions

    ncatt_put(ncout, 0, 'Conventions', 'CF-1.7')
    ncatt_put(ncout, 0, "creator_type", "person")
    ncatt_put(ncout, 0, "program", adp[['program']])
    ncatt_put(ncout, 0, "sea_name", adp[['sea_name']])
    ncatt_put(ncout, 0, "time_coverage_start", adp[['time_coverage_start']])
    ncatt_put(ncout, 0, "time_coverage_end", adp[['time_coverage_end']])
    ncatt_put(ncout, 0, "geospatial_lat_min", adp[['latitude']])
    ncatt_put(ncout, 0, "geospatial_lat_max", adp[['latitude']])
    ncatt_put(ncout, 0, "geospatial_lat_units", "degrees_north")
    ncatt_put(ncout, 0, "geospatial_lon_min", adp[['longitude']])
    ncatt_put(ncout, 0, "geospatial_lon_max", adp[['longitude']])
    ncatt_put(ncout, 0, "geospatial_lon_units", "degrees_east")

    if (adp[['orientation']] == 'up'){
      ncatt_put(ncout, 0, "geospatial_vertical_min", adp[['sensor_depth']] + max(adp[['distance']], na.rm = TRUE))
      ncatt_put(ncout, 0, "geospatial_vertical_max", adp[['sensor_depth']] + min(adp[['distance']], na.rm = TRUE))
    }
    if (adp[['orientation']] == 'down'){
      ncatt_put(ncout, 0, "geospatial_vertical_min", adp[['sensor_depth']] + min(adp[['distance']], na.rm = TRUE))
      ncatt_put(ncout, 0, "geospatial_vertical_max", adp[['sensor_depth']] + max(adp[['distance']], na.rm = TRUE))
    }
    ncatt_put(ncout, 0, "geospatial_vertical_units", "metres")
    ncatt_put(ncout, 0, "geospatial_vertical_positive", 'down')
    ncatt_put(ncout, 0, "institution", adp[['institution']])
    ncatt_put(ncout, 0, "project", adp[['project']])
    ncatt_put(ncout, 0, "history", adp[['history']])
    ncatt_put(ncout, 0 , "flag_meanings", adp[['flag_meaning']])
    ncatt_put(ncout, 0 , "flag_values", c(0:9))
    ncatt_put(ncout, 0, "source", "R code: adcpProcess, github:")
    ncatt_put(ncout, 0, "date_modified", as.character(as.POSIXct(Sys.time(), format = '%Y-%m-%d %H:%M:%sZ', tz = 'UTC')))
    ncatt_put(ncout,0, "_FillValue", "1e35")
    ncatt_put(ncout, 0, "featureType", "timeSeriesProfile") #link to oce adpect? ..... if adp == timeSeriesProfile
    ncatt_put


    ncatt_put(ncout, standard_name("EWCT")$standard_name, "sdn_parameter_name", "Eastward current velocity (Eulerian) in the water body by moored acoustic doppler current profiler (ADCP)")
    ncatt_put(ncout, standard_name("NSCT")$standard_name, "sdn_parameter_name", "Northward current velocity (Eulerian) in the water body by moored acoustic doppler current profiler (ADCP)")
    ncatt_put(ncout, standard_name("VCSP")$standard_name, "sdn_parameter_name", "Upward current velocity in the water body by moored acoustic doppler current profiler (ADCP)")
    ncatt_put(ncout, standard_name("ERRV")$standard_name, "sdn_parameter_name", "Current velocity error in the water body by moored acoustic doppler current profiler (ADCP)")
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_1"), "sdn_parameter_name", "Echo intensity from the water body by moored acoustic doppler current profiler (ADCP) beam 1")
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_2"), "sdn_parameter_name", "Echo intensity from the water body by moored acoustic doppler current profiler (ADCP) beam 2")
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_3"), "sdn_parameter_name", "Echo intensity from the water body by moored acoustic doppler current profiler (ADCP) beam 3")
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_4"), "sdn_parameter_name", "Echo intensity from the water body by moored acoustic doppler current profiler (ADCP) beam 4")
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_1"), "sdn_parameter_name", "Acceptable proportion of signal returns by moored acoustic doppler current profiler (ADCP) beam 1")
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_2"), "sdn_parameter_name", "Acceptable proportion of signal returns by moored acoustic doppler current profiler (ADCP) beam 2")
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_3"), "sdn_parameter_name", "Acceptable proportion of signal returns by moored acoustic doppler current profiler (ADCP) beam 3")
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_4"), "sdn_parameter_name", "Acceptable proportion of signal returns by moored acoustic doppler current profiler (ADCP) beam 4")
    ncatt_put(ncout, "DEPH", "sdn_parameter_name", "Depth below surface of the water body")
    ncatt_put(ncout, "te90", "sdn_parameter_name", "Temperature of the water body")
    ncatt_put(ncout, "PTCH", "sdn_parameter_name", "Orientation (pitch) of measurement platform by inclinometer")
    ncatt_put(ncout, "ROLL", "sdn_parameter_name", "Orientation (roll angle) of measurement platform by inclinometer (second sensor)")
    ncatt_put(ncout, "longitude", "sdn_parameter_name", "Longitude east")
    ncatt_put(ncout, "latitude", "sdn_parameter_name", "Latitude north")
    ncatt_put(ncout, "HEAD", "sdn_parameter_name", "Orientation (horizontal relative to true north) of measurement device {heading}")
    ncatt_put(ncout, "PRES", "sdn_parameter_name", "Pressure (spatial co-ordinate) exerted by the water body by profiling pressure sensor and corrected to read zero at sea level")
    ncatt_put(ncout, "SVEL", "sdn_parameter_name", "Sound velocity in the water body by computation from temperature and salinity by unspecified algorithm")
    #ncatt_put(ncout, 'ELTMEP01', "sdn_parameter_name", "Elapsed time (since 1970-01-01T00:00:00Z)")
    ncatt_put(ncout, 'time_string', "sdn_parameter_name", "String corresponding to format 'YYYY-MM-DDThh:mm:ss.sssZ' or other valid ISO8601 string")

  }

  if (adp@metadata$source == 'odf'){
    ncatt_put(ncout, 0, "mooring_number", adp[['mooring_number']])
    ncatt_put(ncout, 0, "firmware_version", adp[['firmwareVersion']])
    ncatt_put(ncout, 0, "frequency", adp[['frequency']])
    ncatt_put(ncout, 0, "beam_pattern", adp[['beamPattern']])
    ncatt_put(ncout, 0, "janus", adp[['numberOfBeams']])
    ncatt_put(ncout, 0, "pings_per_ensemble", adp[['pingsPerEnsemble']])
    ncatt_put(ncout, 0, "valid_correlation_range", adp[['lowCorrThresh']])
    ncatt_put(ncout, 0, "minmax_percent_good", adp[['percentGdMinimum']])
    ncatt_put(ncout, 0,"minmax_percent_good", "100")
    ncatt_put(ncout, 0, "error_velocity_threshold", paste(adp[['errorVelocityMaximum']], 'm/s'))
    ncatt_put(ncout, 0, "transmit_pulse_length_cm", adp[['xmitPulseLength']])
    ncatt_put(ncout, 0, "false_target_reject_values", adp[['falseTargetThresh']])
    ncatt_put(ncout, 0, "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, 0, "data_type", adp[['instrumentType']])
    ncatt_put(ncout, 0, "data_subtype", adp[['model']])
    ncatt_put(ncout, 0, "coord_system", adp[['oceCoordinate']])
    ncatt_put(ncout, 0, "longitude", adp[['longitude']])
    ncatt_put(ncout, 0, "latitude", adp[['latitude']])
    ncatt_put(ncout, 0, "magnetic_variation", adp[['magneticVariation']])
    ncatt_put(ncout, 0, "platform", adp[['platform']])
    ncatt_put(ncout, 0, "sounding", adp[['sounding']])
    ncatt_put(ncout, 0, "scientist", adp[['scientist']])
    ncatt_put(ncout, 0, "water_depth", adp[['water_depth']])
    ncatt_put(ncout, 0, "delta_t_sec", as.double(adp[['sampling_interval']]))
    ncatt_put(ncout, 0, "pred_accuracy", adp[['velocityResolution']])
    ncatt_put(ncout, 0, "cell_size", adp[['cellSize']])
    ncatt_put(ncout, 0, "deployment_type", adp[['deploymentType']])
    ncatt_put(ncout, 0, "filename", gsub(".*M","",adp[['filename']]))
    ncatt_put(ncout, 0, "model", gsub(".*M","",adp[['model']]))
    ncatt_put(ncout, 0, "orientation", gsub(".*M","",adp[['orientation']]))
    ncatt_put(ncout, 0, "serialNumber", gsub(".*M","",adp[['serialNumber']]))
    ncatt_put(ncout, 0, "water_depth", gsub(".*M","",adp[['waterDepth']]))
    ncatt_put(ncout, 0, "country_institute_code", gsub(".*M","",adp[['countryInstituteCode']]))
    ncatt_put(ncout, 0, "institute", gsub(".*M","",adp[['institute']]))
    ncatt_put(ncout, 0, "number_of_beams", gsub(".*M","",adp[['numberOfBeams']]))
    ncatt_put(ncout, 0, "sample_interval", gsub(".*M","",adp[['sampleInterval']]))
    ncatt_put(ncout, 0, "ship", gsub(".*M","",adp[['ship']]))
    ncatt_put(ncout, 0, "station", gsub(".*M","",adp[['station']]))
    ncatt_put(ncout, 0, "cruise", gsub(".*M","",adp[['cruise']]))
    ncatt_put(ncout, 0, "type", gsub(".*M","",adp[['type']]))
    ncatt_put(ncout, 0, "cruise_number", gsub(".*M","",adp[['cruiseNumber']]))
    ncatt_put(ncout, 0, "depth_off_bottom", gsub(".*M","",adp[['depthOffBottom']]))
    ncatt_put(ncout, 0, "source", gsub(".*M","",adp[['source']]))


















    #FIXME: should be pulled from odf...not in adpect... issue with oce read.odf
    ncatt_put(ncout, "distance", "xducer_offset_from_bottom", adp[['depth_off_bottom']])

    ncatt_put(ncout, "distance", "bin_size", adp[['cellSize']])
    ncatt_put(ncout, standard_name("EWCT")$standard_name, "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, standard_name("EWCT")$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, standard_name("EWCT")$standard_name, "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, standard_name("NSCT")$standard_name, "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, standard_name("NSCT")$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, standard_name("NSCT")$standard_name, "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, standard_name("VCSP")$standard_name, "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, standard_name("VCSP")$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, standard_name("VCSP")$standard_name, "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, standard_name("ERRV")$standard_name, "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, standard_name("ERRV")$standard_name, "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, standard_name("ERRV")$standard_name, "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_1"), "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_1"), "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_1"), "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_1"), "sensor_type", adp[['instrumentType']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_1"), "sensor_depth", adp[['sensor_depth']])
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_1"), "serial_number", adp[['serialNumber']])
    ncatt_put(ncout, standard_name("EWCT")$standard_name, "generic_name", "u")
    ncatt_put(ncout, standard_name("NSCT")$standard_name, "generic_name", "v")
    ncatt_put(ncout, standard_name("VCSP")$standard_name, "generic_name", "w")
    ncatt_put(ncout, standard_name("ERRV")$standard_name, "generic_name", "w")       #issue in current NC protocol
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_1"), "generic_name", "AGC")
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_1"), "generic_name", "PGd")
    #CF

    ncatt_put(ncout, 0, 'Conventions', 'CF-1.6')
    ncatt_put(ncout, 0, "creator_type", "person")
    ncatt_put(ncout, 0, "program", adp[['program']])
    ncatt_put(ncout, 0, "sea_name", adp[['sea_name']])
    ncatt_put(ncout, 0, "time_coverage_start", adp[['deployment_time']])
    ncatt_put(ncout, 0, "time_coverage_end", adp[['recovery_time']])
    ncatt_put(ncout, 0, "geospatial_lat_min", adp[['latitude']])
    ncatt_put(ncout, 0, "geospatial_lat_max", adp[['latitude']])
    ncatt_put(ncout, 0, "geospatial_lat_units", "degrees_north")
    ncatt_put(ncout, 0, "geospatial_lon_min", adp[['longitude']])
    ncatt_put(ncout, 0, "geospatial_lon_max", adp[['longitude']])
    ncatt_put(ncout, 0, "geospatial_lon_units", "degrees_east")
    if (adp[['orientation']] == 'up'){
      ncatt_put(ncout, 0, "geospatial_vertical_min", adp[['sensor_depth']] + max(adp[['distance']], na.rm = TRUE))
      ncatt_put(ncout, 0, "geospatial_vertical_max", adp[['sensor_depth']] + min(adp[['distance']], na.rm = TRUE))
    }
    if (adp[['orientation']] == 'down'){
      ncatt_put(ncout, 0, "geospatial_vertical_min", adp[['sensor_depth']] + min(adp[['distance']], na.rm = TRUE))
      ncatt_put(ncout, 0, "geospatial_vertical_max", adp[['sensor_depth']] + max(adp[['distance']], na.rm = TRUE))
    }
    ncatt_put(ncout, 0, "geospatial_vertical_units", "metres")
    ncatt_put(ncout, 0, "geospatial_vertical_positive", adp[['orientation']])     #eg up or down
    ncatt_put(ncout, 0, "institution", adp[['institution']])
    ncatt_put(ncout, 0, "creator_name", adp[['creator_name']])
    ncatt_put(ncout, 0, "creator_url", adp[['creator_url']])
    ncatt_put(ncout, 0, "creator_email", adp[['creator_email']])
    ncatt_put(ncout, 0, "project", adp[['project']])
    ncatt_put(ncout, 0, "processing_history", adp[['processing_history']])
    ncatt_put(ncout, 0 , "flag_meanings", adp[['flag_meaning']])
    ncatt_put(ncout, 0 , "flag_values", c(0:9))
    ncatt_put(ncout, 0, "source", "R code: adcpProcess, github:")
    ncatt_put(ncout, 0, "date_modified", as.character(as.POSIXct(Sys.time(), format = '%Y-%m-%d %H:%M:%sZ', tz = 'UTC')))
    ncatt_put(ncout,0, "_FillValue", "1e35")
    ncatt_put(ncout, 0, "featureType", "timeSeriesProfile") #link to oce adpect? ..... if adp == timeSeriesProfile

    ncatt_put(ncout, standard_name("EWCT")$standard_name, "sdn_parameter_name", "Eastward current velocity (Eulerian) in the water body by moored acoustic doppler current profiler (ADCP)")
    ncatt_put(ncout, standard_name("NSCT")$standard_name, "sdn_parameter_name", "Northward current velocity (Eulerian) in the water body by moored acoustic doppler current profiler (ADCP)")
    ncatt_put(ncout, standard_name("VCSP")$standard_name, "sdn_parameter_name", "Upward current velocity in the water body by moored acoustic doppler current profiler (ADCP)")
    ncatt_put(ncout, standard_name("ERRV")$standard_name, "sdn_parameter_name", "Current velocity error in the water body by moored acoustic doppler current profiler (ADCP)")
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_1"), "sdn_parameter_name", "Echo intensity from the water body by moored acoustic doppler current profiler (ADCP) beam 1")
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_1"), "sdn_parameter_name", "Acceptable proportion of signal returns by moored acoustic doppler current profiler (ADCP) beam 1")
    ncatt_put(ncout, "longitude", "sdn_parameter_name", "Longitude east")
    ncatt_put(ncout, "latitude", "sdn_parameter_name", "Latitude north")



  }
  if(!is.null(adp[['publisher_name']])){
    ncatt_put(ncout, 0, "publisher_name", adp[['publisher_name']])
  }
  if(!is.null(adp[['publisher_url']])){
    ncatt_put(ncout, 0, "publisher_url", adp[['publisher_url']])
  }
  if(!is.null(adp[['publisher_email']])){
    ncatt_put(ncout, 0, "publisher_email", adp[['publisher_email']])
  }

  ####
  ncatt_put(ncout, standard_name("EWCT")$standard_name, "data_max", max(adp[['v']][,,1], na.rm = TRUE))
  ncatt_put(ncout, standard_name("EWCT")$standard_name, "data_min", min(adp[['v']][,,1], na.rm = TRUE))
  ncatt_put(ncout, standard_name("EWCT")$standard_name, "valid_max", 1000)
  ncatt_put(ncout, standard_name("EWCT")$standard_name, "valid_min", -1000)

  ncatt_put(ncout, standard_name("NSCT")$standard_name, "data_max", max(adp[['v']][,,2], na.rm = TRUE))
  ncatt_put(ncout, standard_name("NSCT")$standard_name, "data_min", min(adp[['v']][,,2], na.rm = TRUE))
  ncatt_put(ncout, standard_name("NSCT")$standard_name, "valid_max", 1000)
  ncatt_put(ncout, standard_name("NSCT")$standard_name, "valid_min", -1000)

  ncatt_put(ncout, standard_name("VCSP")$standard_name, "data_max", max(adp[['v']][,,3], na.rm = TRUE))
  ncatt_put(ncout, standard_name("VCSP")$standard_name, "data_min", min(adp[['v']][,,3], na.rm = TRUE))
  ncatt_put(ncout, standard_name("VCSP")$standard_name, "valid_max", 1000)
  ncatt_put(ncout, standard_name("VCSP")$standard_name, "valid_min", -1000)

  ncatt_put(ncout, standard_name("ERRV")$standard_name, "data_max", max(adp[['v']][,,4], na.rm = TRUE))
  ncatt_put(ncout, standard_name("ERRV")$standard_name, "data_min", min(adp[['v']][,,4], na.rm = TRUE))
  ncatt_put(ncout, standard_name("ERRV")$standard_name, "valid_max", 2000)
  ncatt_put(ncout, standard_name("ERRV")$standard_name, "valid_min", -2000)

  if(adp@metadata$source == 'raw'){
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_1"), "data_min", min(adp[['a', 'numeric']][,,1], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_1"), "data_max", max(adp[['a', 'numeric']][,,1], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_2"), "data_min", min(adp[['a' ,'numeric']][,,2], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_2"), "data_max", max(adp[['a', 'numeric']][,,2], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_3"), "data_min", min(adp[['a', 'numeric']][,,3], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_3"), "data_max", max(adp[['a', 'numeric']][,,3], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_4"), "data_min", min(adp[['a', 'numeric']][,,4], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_4"), "data_max", max(adp[['a', 'numeric']][,,4], na.rm= TRUE))
    ncatt_put(ncout, "CMAG_01", "data_min", min(adp[['q', 'numeric']][,,1], na.rm= TRUE))
    ncatt_put(ncout, "CMAG_01", "data_max", max(adp[['q', 'numeric']][,,1], na.rm= TRUE))

    ncatt_put(ncout, "CMAG_02", "data_min", min(adp[['q' ,'numeric']][,,2], na.rm= TRUE))
    ncatt_put(ncout, "CMAG_02", "data_max", max(adp[['q', 'numeric']][,,2], na.rm= TRUE))

    ncatt_put(ncout, "CMAG_03", "data_min", min(adp[['q', 'numeric']][,,3], na.rm= TRUE))
    ncatt_put(ncout, "CMAG_03", "data_max", max(adp[['q', 'numeric']][,,3], na.rm= TRUE))

    ncatt_put(ncout, "CMAG_04", "data_min", min(adp[['q', 'numeric']][,,4], na.rm= TRUE))
    ncatt_put(ncout, "CMAG_04", "data_max", max(adp[['q', 'numeric']][,,4], na.rm= TRUE))

    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_1"), "data_min", min(adp[['g', 'numeric']][,,1], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_1"), "data_max", max(adp[['g', 'numeric']][,,1], na.rm= TRUE))# eg min 25 % good
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_2"), "data_min", min(adp[['g', 'numeric']][,,2], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_2"), "data_max", max(adp[['g' ,'numeric']][,,2], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_3"), "data_min", min(adp[['g' ,'numeric']][,,3], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_3"), "data_max", max(adp[['g', 'numeric']][,,3], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_4"), "data_min", min(adp[['g', 'numeric']][,,4], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_4"), "data_max", max(adp[['g', 'numeric']][,,4], na.rm= TRUE))
    ncatt_put(ncout, "hght", "data_min", min(adp[['depth', 'data']]))
    ncatt_put(ncout, "hght", "data_max", max(adp[['depth', 'data']]))
    ncatt_put(ncout, "DEPH", "data_min", min(adp[['depth']]))
    ncatt_put(ncout, "DEPH", "data_max", max(adp[['depth']]))
    ncatt_put(ncout, "te90", "data_min", min(adp[['temperature']]))
    ncatt_put(ncout, "te90", "data_max", max(adp[['temperature']]))
    ncatt_put(ncout, "PTCH", "data_min", min(adp[['pitch']]))
    ncatt_put(ncout, "PTCH", "data_max", max(adp[['pitch']]))
    ncatt_put(ncout, "ROLL", "data_min", min(adp[['roll']]))
    ncatt_put(ncout, "ROLL", "data_max", max(adp[['roll']]))
    ncatt_put(ncout, "HEAD", "data_min", min(adp[['heading']]))
    ncatt_put(ncout, "HEAD", "data_max", max(adp[['heading']]))
    ncatt_put(ncout, "PRES", "data_min", min(adp[['pressure']]))
    ncatt_put(ncout, "PRES", "data_max", max(adp[['pressure']]))
    ncatt_put(ncout, "SVEL", "data_min", min(adp[['soundSpeed']]))
    ncatt_put(ncout, "SVEL", "data_max", max(adp[['soundSpeed']]))

  }
  if( adp@metadata$source == 'odf'){
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_1"), "data_min", min(adp[['a', 'numeric']], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("BEAM")$standard_name, "_1"), "data_max", max(adp[['a', 'numeric']], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_1"), "data_min", min(adp[['q', 'numeric']], na.rm= TRUE))
    ncatt_put(ncout, paste0(standard_name("PGDP")$standard_name, "_1"), "data_max", max(adp[['q', 'numeric']], na.rm= TRUE))

  }


  if (!missing(metadata)) {
    metad <- read.csv(metadata, header = TRUE)

    mn <- as.character(metad[,1])
    mv <- as.character(metad[,2])


    md <- as.list(mv)
    names(md) <- mn

    for (m in seq_along(md)) {
      ncatt_put(ncout, 0, names(md)[m], md[[m]])
    }
    nc_close(ncout)


  }
}
