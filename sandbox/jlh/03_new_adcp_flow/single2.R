#' Convert adp object to netCDF
#'
#' This function exports an adp object to a netCDF using variables
#' and metadata within adp combined. Data variables names and units
#' are named inserted in CF standards using the [standardName()] function.
#'
#'@param adp an adp object from the oce class
#'@param data a data frame of standard name, name, units, and GF3 codes likely from getCFData
#'@param name name of the netCDF file to be produced

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

single2 <- function(adp, name, debug=0, data=NULL){
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
    ncpath <- "./"
    ncname <- name
    ncfname <- paste(ncpath, ncname, ".nc", sep = "")

    # Added 10-SEP-2018 R.Pettipas
    # If the function exits due to an error, close the open netCDF file.
    # Otherwise, the file can't be deleted until the R session is exited.
    on.exit(expr=ncdf4::nc_close(ncout))
    if (debug > 0) {
        message("Step 1: About to set dimension using ncdim_def()")
    }
    time <- as.POSIXct(adp[['time']], tz = 'UTC', origin = '1970-01-01 00:00:00')
    distance <- adp[['distance', 'numeric']]
    longitude <- adp[['longitude']]
    latitude <- adp[['latitude']]

    #create dimensions
    timedim <- ncdf4::ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", as.double(time))    #time formatting FIX
    distdim <- ncdf4::ncdim_def("distance", "metres", as.double(distance))
    stationdim <- ncdf4::ncdim_def("station", "", as.numeric(adp[['mooring_number']]))
    longitudedim <- ncdf4::ncdim_def("longitude", "degrees_east" , as.double(longitude))
    latitudedim <- ncdf4::ncdim_def("latitude", "degrees_north", as.double(latitude))
    dimnchar <- ncdf4::ncdim_def('nchar', '', 1:23, create_dimvar = FALSE)

    FillValue <- 1e35

    #define variables

    if (debug > 0) {
        message("Step 2: Define a netcdf variable using ncvar_def()")
    }

    dlname <- 'longitude'
    longitude_def <- ncdf4::ncvar_def(longname= "longitude", units = 'degrees_east', dim = stationdim, name = dlname, prec = 'double')

    dlname <- 'latitude'
    latitude_def <- ncdf4::ncvar_def( longname = 'latitude', units = 'degrees_north', dim =  stationdim, name = dlname, prec = 'double')

    dlname <- "time_string"
    time_string_def <- ncdf4::ncvar_def("DTUT8601", units = "",dim =  list(dimnchar, timedim), missval = NULL, name =  dlname, prec = "char")
    dataNames <- unlist(unname(adp[['dataNamesOriginal']]))

    if (debug > 0) {
        message("latitude, longitude, time_string and ", paste0(dataNames, sep=","), " are being defined")
    }

    defs <- list()
    for (i in dataNames) {
        DEF <- paste0(standardName(i, data=data)$standard_name, "_def", sep="")
        defs[i] <- DEF
    }
    defs <- unlist(defs)
    dlNames <- list()

    for (i in seq_along(dataNames)) {
        dlname <- standardName(dataNames[i], data=data)$standard_name
        dlNames[i] <- dlname
    }
    dlNames <- unlist(dlNames)

    ncvarObjects <- lapply(seq_along(dataNames), function(x) ncvar_def(longname= standardName(dataNames[x], data=data)$standard_name, units = standardName(dataNames[x], data=data)$units, dim = stationdim, name = dlNames[x], prec = 'double'))
    names(ncvarObjects) <- defs

    if (debug > 0) {
        message("Step 3: About to create a netcdf file using nc_create()")
    }
    ncout <- ncdf4::nc_create(filename=ncfname, vars=ncvarObjects, force_v4 = TRUE)

    if (debug > 0) {
        message("Step 4: About to insert variables to nc file using ncvar_put()")
    }

    names <- unlist(lapply(ncvarObjects, function(x) x$name))
    ea <- which(grepl("eastward", names))
    no <- which(grepl("northward", names))
    up <- which(grepl("upward", names))
    er <- which(grepl("error", names))
    keep <-sort(c(ea,no,up,er))

    for (i in seq_along(ncvarObjects[keep])) {
        if (debug > 0) {
            message(ncvarObjects[keep][[i]]$name, " is going in dimension ", i)
        }
        ncdf4::ncvar_put(nc=ncout, varid=ncvarObjects[keep][[i]], vals=adp[['v']][,,i])
    }

    if (debug > 0) {
      message("Now adding other variables using ncvar_put()")
    }

    #ncdf4::ncvar_put(nc=ncout, varid=time_string_def, vals=as.POSIXct(adp[['time']], tz = 'UTC', origin = '1970-01-01 00:00:00'))
    #ncdf4::ncvar_put(ncout, longitude_def, adp[['longitude']])
    #ncdf4::ncvar_put(ncout, latitude_def, adp[['latitude']])
    #ncdf4::ncvar_put(ncout, b1_def, adp[['a', 'numeric']])
    #ncdf4::ncvar_put(ncout, pg1_def, adp[['q', 'numeric']])
    #ncdf4::ncvar_put(ncout, ts_def, adp[['time']])

    if (debug > 0) {
        message("Step 6: About to write metadata (attributes) into existing netCDF using ncatt_put()")
    }

    metaNames <- unlist(names(adp@metadata))
    #message("The names of the metadata names are ", paste0(sort(metaNames), sep=","))

    ncdf4::ncatt_put(ncout, 'station', attname = 'cf_role',attval =  'timeseries_id')

 #   ncdf4::ncatt_put(ncout, 'station', attname = 'cf_role',attval =  'timeseries_id')
 #   ncdf4::ncatt_put(ncout, 'time', attname = 'cf_role', attval = 'profile_id')
 #   ncdf4::ncatt_put(ncout, 'station', 'standard_name', 'platform_name')
 #   ncdf4::ncatt_put(ncout, 'time' , 'calendar', 'gregorian')
 #   ncdf4::ncatt_put(ncout, 'time_string', 'note', 'time values as ISO8601 string, YY-MM-DD hh:mm:ss')
 #   ncdf4::ncatt_put(ncout, 'time_string', 'time_zone', 'UTC')
 #   ncdf4::ncatt_put(ncout, 0, 'processing_history',adp[['processing_history']])
 #   ncdf4::ncatt_put(ncout, 0, "time_coverage_duration", (tail(adp[['time']], n = 1) - adp[['time']][[1]]))
 #   # deprecated M. Oulliet 4/11/2019
 #   # ncdf4::ncatt_put(ncout, 0, "time_coverage_duration_units", "days")
 #   ncdf4::ncatt_put(ncout, 0, "cdm_data_type", "station")
 #   ncdf4::ncatt_put(ncout, 0, "alternate_pressure_values", adp[['alternate_pressure_values']])
 #   ncdf4::ncatt_put(ncout, 0, "alternate_pressure_file", adp[['alternate_pressure_file']])
 #   ncdf4::ncatt_put(ncout, 0, "vertical_separation", adp[['vertical_separation']])
 #   ncdf4::ncatt_put(ncout, 0, "title", adp[['title']])


 #   ncdf4::ncatt_put(ncout, 0, "mooring_number", adp[['mooring_number']])
 #   ncdf4::ncatt_put(ncout, 0, "firmware_version", adp[['firmwareVersion']])
 #   ncdf4::ncatt_put(ncout, 0, "frequency", adp[['frequency']])
 #   ncdf4::ncatt_put(ncout, 0, "beam_pattern", adp[['beamPattern']])
 #   ncdf4::ncatt_put(ncout, 0, "janus", adp[['numberOfBeams']])
 #   ncdf4::ncatt_put(ncout, 0, "pings_per_ensemble", adp[['pingsPerEnsemble']])
 #   ncdf4::ncatt_put(ncout, 0, "valid_correlation_range", adp[['lowCorrThresh']])
 #   ncdf4::ncatt_put(ncout, 0, "minmax_percent_good", adp[['percentGdMinimum']])
 #   ncdf4::ncatt_put(ncout, 0,"minmax_percent_good", "100")
 #   ncdf4::ncatt_put(ncout, 0, "error_velocity_threshold", paste(adp[['errorVelocityMaximum']], 'm/s'))
 #   ncdf4::ncatt_put(ncout, 0, "transmit_pulse_length_cm", adp[['xmitPulseLength']])
 #   ncdf4::ncatt_put(ncout, 0, "false_target_reject_values", adp[['falseTargetThresh']])
 #   ncdf4::ncatt_put(ncout, 0, "serial_number", adp[['serialNumber']])
 #   ncdf4::ncatt_put(ncout, 0, "data_type", adp[['instrumentType']])
 #   ncdf4::ncatt_put(ncout, 0, "data_subtype", adp[['model']])
 #   ncdf4::ncatt_put(ncout, 0, "coord_system", adp[['oceCoordinate']])
 #   ncdf4::ncatt_put(ncout, 0, "longitude", adp[['longitude']])
 #   ncdf4::ncatt_put(ncout, 0, "latitude", adp[['latitude']])
 #   ncdf4::ncatt_put(ncout, 0, "magnetic_variation", adp[['magneticVariation']])
 #   ncdf4::ncatt_put(ncout, 0, "platform", adp[['platform']])
 #   ncdf4::ncatt_put(ncout, 0, "sounding", adp[['sounding']])
 #   ncdf4::ncatt_put(ncout, 0, "scientist", adp[['scientist']])
 #   ncdf4::ncatt_put(ncout, 0, "water_depth", adp[['water_depth']])
 #   ncdf4::ncatt_put(ncout, 0, "delta_t_sec", as.double(adp[['sampling_interval']]))
 #   ncdf4::ncatt_put(ncout, 0, "pred_accuracy", adp[['velocityResolution']])
 #   ncdf4::ncatt_put(ncout, 0, "cell_size", adp[['cellSize']])
 #   ncdf4::ncatt_put(ncout, 0, "deployment_type", adp[['deploymentType']])
 #   ncdf4::ncatt_put(ncout, 0, "filename", gsub(".*M","",adp[['filename']]))
 #   ncdf4::ncatt_put(ncout, 0, "model", gsub(".*M","",adp[['model']]))
 #   ncdf4::ncatt_put(ncout, 0, "orientation", gsub(".*M","",adp[['orientation']]))
 #   ncdf4::ncatt_put(ncout, 0, "serialNumber", gsub(".*M","",adp[['serialNumber']]))
 #   ncdf4::ncatt_put(ncout, 0, "water_depth", gsub(".*M","",adp[['waterDepth']]))
 #   ncdf4::ncatt_put(ncout, 0, "country_institute_code", gsub(".*M","",adp[['countryInstituteCode']]))
 #   ncdf4::ncatt_put(ncout, 0, "institute", gsub(".*M","",adp[['institute']]))
 #   ncdf4::ncatt_put(ncout, 0, "number_of_beams", gsub(".*M","",adp[['numberOfBeams']]))
 #   ncdf4::ncatt_put(ncout, 0, "sample_interval", gsub(".*M","",adp[['sampleInterval']]))
 #   ncdf4::ncatt_put(ncout, 0, "ship", gsub(".*M","",adp[['ship']]))
 #   ncdf4::ncatt_put(ncout, 0, "station", gsub(".*M","",adp[['station']]))
 #   ncdf4::ncatt_put(ncout, 0, "cruise", gsub(".*M","",adp[['cruise']]))
 #   ncdf4::ncatt_put(ncout, 0, "type", gsub(".*M","",adp[['type']]))
 #   ncdf4::ncatt_put(ncout, 0, "cruise_number", gsub(".*M","",adp[['cruiseNumber']]))
 #   ncdf4::ncatt_put(ncout, 0, "depth_off_bottom", gsub(".*M","",adp[['depthOffBottom']]))
 #   ncdf4::ncatt_put(ncout, 0, "source", gsub(".*M","",adp[['source']]))

 #   #FIXME: should be pulled from odf...not in obpect... issue with oce read.odf
 #   ncdf4::ncatt_put(ncout, "distance", "xducer_offset_from_bottom", adp[['depth_off_bottom']])

 #   ncdf4::ncatt_put(ncout, "distance", "bin_size", adp[['cellSize']])
#
#    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "sensor_type", adp[['instrumentType']])
#    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "sensor_depth", adp[['sensor_depth']])
#    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "serial_number", adp[['serialNumber']])
#    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "sensor_type", adp[['instrumentType']])
#    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "sensor_depth", adp[['sensor_depth']])
#    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "serial_number", adp[['serialNumber']])
#    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "sensor_type", adp[['instrumentType']])
#    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "sensor_depth", adp[['sensor_depth']])
#    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "serial_number", adp[['serialNumber']])
#    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "sensor_type", adp[['instrumentType']])
#    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "sensor_depth", adp[['sensor_depth']])
#    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "serial_number", adp[['serialNumber']])
#    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "sensor_type", adp[['instrumentType']])
#    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "sensor_depth", adp[['sensor_depth']])
#    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "serial_number", adp[['serialNumber']])
#    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "sensor_type", adp[['instrumentType']])
#    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "sensor_depth", adp[['sensor_depth']])
#    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "serial_number", adp[['serialNumber']])
#
#
#    ncdf4::ncatt_put(ncout, 0, 'Conventions', 'CF-1.6')
#    ncdf4::ncatt_put(ncout, 0, "creator_type", "person")
#    ncdf4::ncatt_put(ncout, 0, "program", adp[['program']])
#    ncdf4::ncatt_put(ncout, 0, "sea_name", adp[['sea_name']])
#    ncdf4::ncatt_put(ncout, 0, "time_coverage_start", adp[['deployment_time']])
#    ncdf4::ncatt_put(ncout, 0, "time_coverage_end", adp[['recovery_time']])
#    ncdf4::ncatt_put(ncout, 0, "geospatial_lat_min", adp[['latitude']])
#    ncdf4::ncatt_put(ncout, 0, "geospatial_lat_max", adp[['latitude']])
#    ncdf4::ncatt_put(ncout, 0, "geospatial_lat_units", "degrees_north")
#    ncdf4::ncatt_put(ncout, 0, "geospatial_lon_min", adp[['longitude']])
#    ncdf4::ncatt_put(ncout, 0, "geospatial_lon_max", adp[['longitude']])
#    ncdf4::ncatt_put(ncout, 0, "geospatial_lon_units", "degrees_east")
#    if (adp[['orientation']] == 'up'){
#        ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_min", adp[['sensor_depth']] + max(adp[['distance']], na.rm = TRUE))
#        ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_max", adp[['sensor_depth']] + min(adp[['distance']], na.rm = TRUE))
#    }
#    if (adp[['orientation']] == 'down'){
#        ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_min", adp[['sensor_depth']] + min(adp[['distance']], na.rm = TRUE))
#        ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_max", adp[['sensor_depth']] + max(adp[['distance']], na.rm = TRUE))
#    }
#    ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_units", "metres")
#    ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_positive", adp[['orientation']])     #eg up or down
#    ncdf4::ncatt_put(ncout, 0, "institution", adp[['institution']])
#    ncdf4::ncatt_put(ncout, 0, "creator_name", adp[['creator_name']])
#    ncdf4::ncatt_put(ncout, 0, "creator_url", adp[['creator_url']])
#    ncdf4::ncatt_put(ncout, 0, "creator_email", adp[['creator_email']])
#    ncdf4::ncatt_put(ncout, 0, "project", adp[['project']])
#    ncdf4::ncatt_put(ncout, 0, "processing_history", adp[['processing_history']])
#    ncdf4::ncatt_put(ncout, 0 , "flag_meanings", adp[['flag_meaning']])
#    ncdf4::ncatt_put(ncout, 0 , "flag_values", c(0:9))
#    ncdf4::ncatt_put(ncout, 0, "source", "R code: adcpProcess, github:")
#    ncdf4::ncatt_put(ncout, 0, "date_modified", as.character(as.POSIXct(Sys.time(), format = '%Y-%m-%d %H:%M:%sZ', tz = 'UTC')))
#    ncdf4::ncatt_put(ncout,0, "_FillValue", "1e35")
#    ncdf4::ncatt_put(ncout, 0, "featureType", "timeSeriesProfile") #link to oce adpect? ..... if adp == timeSeriesProfile
#
#    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "generic_parameter_name", "Eastward current velocity (Eulerian) in the water body by moored acoustic doppler current profiler (ADCP)")
#    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "generic_parameter_name", "Northward current velocity (Eulerian) in the water body by moored acoustic doppler current profiler (ADCP)")
#    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "generic_parameter_name", "Upward current velocity in the water body by moored acoustic doppler current profiler (ADCP)")
#    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "generic_parameter_name", "Current velocity error in the water body by moored acoustic doppler current profiler (ADCP)")
#    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "generic_parameter_name", "Echo intensity from the water body by moored acoustic doppler current profiler (ADCP) beam 1")
#    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "generic_parameter_name", "Acceptable proportion of signal returns by moored acoustic doppler current profiler (ADCP) beam 1")
#    ncdf4::ncatt_put(ncout, "longitude", "generic_parameter_name", "Longitude east")
#    ncdf4::ncatt_put(ncout, "latitude", "generic_parameter_name", "Latitude north")
#
#
#
#    if(!is.null(adp[['publisher_name']])){
#        ncdf4::ncatt_put(ncout, 0, "publisher_name", adp[['publisher_name']])
#    }
#    if(!is.null(adp[['publisher_url']])){
#        ncdf4::ncatt_put(ncout, 0, "publisher_url", adp[['publisher_url']])
#    }
#    if(!is.null(adp[['publisher_email']])){
#        ncdf4::ncatt_put(ncout, 0, "publisher_email", adp[['publisher_email']])
#    }
#
#    ####
#    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "data_max", max(adp[['v']][,,1], na.rm = TRUE))
#    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "data_min", min(adp[['v']][,,1], na.rm = TRUE))
#    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "valid_max", 1000)
#    ncdf4::ncatt_put(ncout, standardName("EWCT",data=data)$standard_name, "valid_min", -1000)
#
#    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "data_max", max(adp[['v']][,,2], na.rm = TRUE))
#    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "data_min", min(adp[['v']][,,2], na.rm = TRUE))
#    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "valid_max", 1000)
#    ncdf4::ncatt_put(ncout, standardName("NSCT",data=data)$standard_name, "valid_min", -1000)
#
#    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "data_max", max(adp[['v']][,,3], na.rm = TRUE))
#    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "data_min", min(adp[['v']][,,3], na.rm = TRUE))
#    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "valid_max", 1000)
#    ncdf4::ncatt_put(ncout, standardName("VCSP",data=data)$standard_name, "valid_min", -1000)
#
#    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "data_max", max(adp[['v']][,,4], na.rm = TRUE))
#    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "data_min", min(adp[['v']][,,4], na.rm = TRUE))
#    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "valid_max", 2000)
#    ncdf4::ncatt_put(ncout, standardName("ERRV",data=data)$standard_name, "valid_min", -2000)
#
#    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "data_min", min(adp[['a', 'numeric']], na.rm= TRUE))
#    ncdf4::ncatt_put(ncout, paste0(standardName("BEAM",data=data)$standard_name, "_1"), "data_max", max(adp[['a', 'numeric']], na.rm= TRUE))
#    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "data_min", min(adp[['q', 'numeric']], na.rm= TRUE))
#    ncdf4::ncatt_put(ncout, paste0(standardName("PGDP",data=data)$standard_name, "_1"), "data_max", max(adp[['q', 'numeric']], na.rm= TRUE))
#

}
