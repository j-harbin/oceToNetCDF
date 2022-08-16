#' Template to convert odf object to NetCDF
#'
#' This function converts and odf object to a NetCDF files for CTD
#' and RCM types.
#'
#' @param odf an odf object from oce which contains mctd data
#' @param data a data frame of standard name, name, units, and GF3 codes likely from getData
#' @param metadata a csv file following the standard template which includes all
#'   necessary metadata
#' @param filename the desired name for the netCDF file produced, if left NULL
#'   the default will conform to BIO naming conventions
#' @return netCDF file with a maximum of 12 variables
#' @export
#'
#' @examples
#' file <- list.files('.', pattern = "MCTD*...*.ODF")
#' odf <- read.odf(file)
#' metadata <- 'MCTD_SAMPLE_METADATA.csv'
#' mctd_nc(odf, metadata)
#'

convertNetCDF <- function(odf, metadata, filename = NULL, debug=0, data=NULL){
  if (is.null(data)) {
    stop("In convertNetCDF(), must provide a data frame for data")
  }
  if (!(class(data) == "data.frame")) {
    stop("In convertNetCDF(), data must be a data.frame class, not ", class(data))
  }
  library(oce)
  library(ncdf4)
  MCTD <- grepl("MCTD", odf[['filename']])
  RCM <- grepl("RCM", odf[['filename']])
  ADCP <- grepl("ADCP", odf[['filename']])
  v <- names(odf@data)

  if (MCTD) {
      DF <- data[which(data$code %in% c("SYTM", "CNDC", "PSAL", "TEMP", "PRES")),]
  } else if (RCM) {
      DF <- data[which(data$code %in% c("HCDT", "HCSP", "PRES", "PSAL", "SYTM", "TEMP")),]
  } else if (ADCP) {
      DF <- data[which(data$code %in% c("SYTM", "UNKN", "BEAM", "ERRV", "VCSP", "NSCT","EWCT")),]
  } else {
      message("Unrecognizable file type.")
  }

  var <- list()
  for (i in v) {
    code <- data$code[which(data$standard_name %in% gsub("_[0-9]$.*","",i))]
    var[i] <- code
  }


  if (debug > 0) {
    message("v is ",v, " and var is= ", var)
    message("Step 1: About to remove the time.")
  }

  #remove SYTM from var list
  tr <- grep(v, pattern = 'time')
  v <- v[-tr]
  vt <- grep(var, pattern = 'SYTM')
  var <- var[-vt]

  #POPULATE VARIABLES WITH APPROPRIATE CODES
  #browser()
  VAR <- list()
  if (debug > 0) {
    message("Step 2: About to determine units and standard_name for each code.")
  }

  for (i in seq_along(var)) {
    VAR[[i]] <- standardName(var[[i]], data=data)
  }
  i <- 1

  if (debug > 0) {
    message("class of VAR IS ",class(VAR), " and the names in the first var is =", names(VAR[[1]]))
  }

  if (debug > 0) {
    message("Step 3: About to populate the variable, var, units, max,min, standard_name, and flags.")
  }

  for ( vv in VAR ){

    eval(parse(text = paste0("variable_", i, "<- '" , v[[i]], "'")))
    eval(parse(text= paste0("var",i," <-'", v[[i]],"'")))
    eval(parse(text = paste0("units", i, " <-'", vv$units, "'")))
    eval(parse(text = paste0('var', i, 'max <-', -10000)))
    eval(parse(text = paste0('var', i, 'min <-' , 10000)))
    if(!is.null(vv$std)){
      eval(parse(text = paste0("std_variable_", i, " <- '", vv$std, "'")))
    }else{
      eval(parse(text = paste0("std_variable_", i, " <- NULL")))
    }
    #check if variable also has quality flag
    if (v[[i]] %in% names(odf[['flags']])) {
      eval(parse(text = paste0("var", i, "_QC <- '", vv$gf3, "_QC'")))
      eval(parse(text = paste0("variable", i , "_QC <- 'quality flag for " , v[[i]], "'")))
    }
    i <- i+1


  }

  if (debug > 0) {
  message("Step 4: About to check number of variables.")
  }
  #CHECK LENGTH OF VARIABLES
  numvar <- length(var)

  #FILENAME
  if(missing(filename)){
    #filename <- paste("MCTD", odf[['cruiseNumber']], odf[['eventNumber']], odf[['eventQualifier']], odf[['samplingInterval']], sep = '_')
    if (MCTD) {
    f <- gsub(".*M","",odf[['filename']])
    ff <-  gsub("\\..*","",f)
    filename <- paste0("M", ff, sep="")
    } else if (RCM) {
      f <- gsub(".*M","",odf[['filename']])
      ff <-  gsub("\\..*","",f)
      filename <- paste0("MCM", ff, sep="")
    } else if (ADCP) {
      f <- gsub(".*M","",odf[['filename']])
      ff <-  gsub("\\..*","",f)
      filename <- paste0("M", ff, sep="")

    }
  }
  ncpath <- "./"
  ncfname <- paste(ncpath, filename, ".nc", sep = "")

  if (debug > 0) {
    message("Step 5: Check dimensions of time, station, lon, lat, and dimnchar.")
  }
  #DIMENSIONS
  timedim <- ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", as.double(odf[['time']]))
  stationdim <- ncdim_def("station", "counts", as.numeric(length(odf[['station']])))
  londim <- ncdim_def("longitude", "degrees_east" , as.double(odf[['longitude']]))
  latdim <- ncdim_def("latitude", "degrees_north", as.double(odf[['latitude']]))
  dimnchar <- ncdim_def('nchar', '', 1:23, create_dimvar = FALSE)

  #FILLVALUE
  FillValue <- 1e35


  #VARIABLES

  if (debug > 0) {
    message("Step 6: About to define netCDF variablesusing ncvar_def.")
  }
  dlname <- 'longitude'
  lon_def <- ncvar_def(longname= "longitude", units = 'degrees_east', dim = stationdim, name = dlname, prec = 'double')

  dlname <- 'latitude'
  lat_def <- ncvar_def( longname = 'latitude', units = 'degrees_north', dim =  stationdim, name = dlname, prec = 'double')

  dlname <- "time_string"
  ts_def <- ncvar_def("DTUT8601", units = "",dim =  list( dimnchar, timedim), missval = NULL, name =  dlname, prec = "char")

  dlname <- variable_1
  v1_def <- ncvar_def(var1, units1, list(timedim, stationdim), FillValue, dlname, prec = 'double')

  if (numvar >1){
    dlname <- variable_2
    v2_def <- ncvar_def(var2, units2, list(timedim, stationdim), FillValue, dlname, prec = 'double')

    if (numvar >2){
      dlname <- variable_3
      v3_def <- ncvar_def(var3, units3, list(timedim, stationdim), FillValue, dlname, prec = 'double')

      if (numvar >3){
        dlname <- variable_4
        v4_def <- ncvar_def(var4, units4, list(timedim, stationdim), FillValue, dlname, prec = 'double')

        if (numvar >4){
          dlname <- variable_5
          v5_def <- ncvar_def(var5, units5, list(timedim, stationdim), FillValue, dlname, prec = 'double')

          if (numvar >5){
            dlname <- variable_6
            v6_def <- ncvar_def(var6, units6, list(timedim, stationdim), FillValue, dlname, prec = 'double')

            if (numvar >6){
              dlname <- variable_7
              v7_def <- ncvar_def(var7, units7, list(timedim, stationdim), FillValue, dlname, prec = 'double')

              if (numvar >7){
                dlname <- variable_8
                v8_def <- ncvar_def(var8, units8, list(timedim, stationdim), FillValue, dlname, prec = 'double')

                if (numvar >8){
                  dlname <- variable_9
                  v9_def <- ncvar_def(var9, units9, list(timedim, stationdim), FillValue, dlname, prec = 'double')

                  if (numvar >9){
                    dlname <- variable_10
                    v10_def <- ncvar_def(var10, units10, list(timedim, stationdim), FillValue, dlname, prec = 'double')

                    if (numvar > 10){
                      dlname <- variable_11
                      v11_def <- ncvar_def(var11, units11, list(timedim, stationdim), FillValue, dlname, prec = 'double')

                      if (numvar > 11){
                        dlname <- variable_12
                        v12_def <- ncvar_def(var12, units12, list(timedim, stationdim), FillValue, dlname, prec = 'double')

                        if (numvar >12){
                          warning ("Maximum of 12 variables exceeded, not all data has been exported!")
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  #####write out definitions to new nc file####
  defs <- grep(ls(), pattern = '_def', value = TRUE)
  dd <- NULL
  for ( i in 1:length(defs)){
    eval(parse(text = paste0("dd[[i]] <- ", defs[[i]])))
  }

  if (debug > 0) {
    message("Step 7: About to create  new netCDF file on disk using nc_create.")
  }
  ncout <-
    nc_create(
      ncfname,

      dd
      #,
      #force_v4 = TRUE
    )
  if (debug > 0) {
  message("Step 8: About to insert data to an existing netCDF using ncvar_put.")
  }

  ####INSERT DATA####
  ncvar_put(ncout, ts_def, odf[['time']])
  #ncvar_put(ncout, t_def, as.POSIXct(odf[['time']], tz = 'UTC', origin = '1970-01-01 00:00:00'))
  ncvar_put(ncout, lon_def, odf[['longitude']])
  ncvar_put(ncout, lat_def, odf[['latitude']])
  ncvar_put(ncout, v1_def, odf[[variable_1]])
  if (numvar >1){
    ncvar_put(ncout, v2_def, odf[[variable_2]])
    if (numvar >2){
      ncvar_put(ncout, v3_def, odf[[variable_3]])
      if (numvar >3){
        ncvar_put(ncout, v4_def, odf[[variable_4]])
        if (numvar >4){
          ncvar_put(ncout, v5_def, odf[[variable_5]])
          if (numvar >5){
            ncvar_put(ncout, v6_def, odf[[variable_6]])
            if (numvar >6){
              ncvar_put(ncout, v7_def, odf[[variable_7]])
              if (numvar >7){
                ncvar_put(ncout, v8_def, odf[[variable_8]])
                if (numvar >8){
                  ncvar_put(ncout, v9_def, odf[[variable_9]])
                  if (numvar >9){
                    ncvar_put(ncout, v10_def, odf[[variable_10]])
                    if (numvar >10){
                      ncvar_put(ncout, v11_def, odf[[variable_11]])
                      if(numvar >11){
                        ncvar_put(ncout, v12_def, odf[[variable_12]])
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  ####metadata####

  if (debug > 0) {
    message("Step 9: About to insert attributes (metadata) into a netCDF file")
  }
  ncatt_put(ncout, 'station', 'longitude', odf[['longitude']])
  ncatt_put(ncout, 'station', 'latitiude', odf[['latitude']])
  ncatt_put(ncout, 'station', 'standard_name', 'platform_name')
  ncatt_put(ncout, 'station', 'cf_role', 'timeseries_id')
  ncatt_put(ncout, 'time' , 'calendar', 'gregorian')
  ncatt_put(ncout, 'time_string', 'note', 'time values as ISO8601 string, YY-MM-DD hh:mm:ss')
  ncatt_put(ncout, 'time_string', 'time_zone', 'UTC')

  #FROM ODF
  ncatt_put(ncout, 0, 'inst_type', odf[['type']])
  ncatt_put(ncout, 0, 'model', odf[['model']])
  ncatt_put(ncout, 0, 'sampling_interval', odf[['samplingInterval']])
  ncatt_put(ncout, 0, 'country_code', odf[['countryInstituteCode']])
  ncatt_put(ncout, 0, 'cruise_number', odf[['cruiseNumber']])
  ncatt_put(ncout, 0, "mooring_number", odf[['station']])
  ncatt_put(ncout, 0, "time_coverage_duration", (tail(odf[['time']], n = 1) - odf[['time']][[1]]))
  ncatt_put(ncout, 0, "time_coverage_duration_units", "days")
  ncatt_put(ncout, 0, "cdm_data_type", "station")
  ncatt_put(ncout, 0, "serial_number", odf[['serialNumber']])
  ncatt_put(ncout, 0, "data_type", 'MCTD')
  ncatt_put(ncout, 0, "longitude", odf[['longitude']])
  ncatt_put(ncout, 0, "latitude", odf[['latitude']])
  ncatt_put(ncout, 0, "platform", odf[['cruise']])
  ncatt_put(ncout, 0, "sounding", odf[['sounding']])
  ncatt_put(ncout, 0, "chief_scientist", odf[['scientist']])
  ncatt_put(ncout, 0, "water_depth", odf[['waterDepth']])
  ncatt_put(ncout, 0, "cruise_name", odf[['cruise']])

  ####variable ATTRIBUTES####

  ncatt_put(ncout, var1, 'reference_scale', 'IPTS-68')



  ####variables####
  #sensor type, sensor depth and serial number for each variable
  #generic nameS
  #STANDARD NAMES
  #data max and min
  #VALID MIN AND MAX
  #p01 and p06 names

  ncatt_put(ncout, var1, "sensor_type", odf[['model']])
  ncatt_put(ncout, var1, "sensor_depth", odf[['depthMin']])
  ncatt_put(ncout, var1, "serial_number", odf[['serialNumber']])
  #ncatt_put(ncout, var1, "generic_name", variable_1)
  #if (!is.null(std_variable_1)){
  #  ncatt_put(ncout, var1, "standard_name", variable_1)
  #}
  ncatt_put(ncout, var1, "data_max", max(odf[[variable_1]], na.rm = TRUE))
  ncatt_put(ncout, var1, "data_min", min(odf[[variable_1]], na.rm = TRUE))
  ncatt_put(ncout, var1, "valid_max", var1max)
  ncatt_put(ncout, var1, "valid_min", var1min)


  if (numvar > 1){
    ncatt_put(ncout, var2, "sensor_type", odf[['model']])
    ncatt_put(ncout, var2, "sensor_depth", odf[['depthMin']])
    ncatt_put(ncout, var2, "serial_number", odf[['serialNumber']])
    #ncatt_put(ncout, var2, "generic_name", variable_2)
    #if (!is.null(std_variable_2)){
    #  ncatt_put(ncout, var2, "standard_name", variable_2)
    #}
    ncatt_put(ncout, var2, "data_max", max(odf[[variable_2]], na.rm = TRUE))
    ncatt_put(ncout, var2, "data_min", min(odf[[variable_2]], na.rm = TRUE))
    ncatt_put(ncout, var2, "valid_max", var2max)
    ncatt_put(ncout, var2, "valid_min", var2min)


    if (numvar >2){
      ncatt_put(ncout, var3, "sensor_type", odf[['model']])
      ncatt_put(ncout, var3, "sensor_depth", odf[['depthMin']])
      ncatt_put(ncout, var3, "serial_number", odf[['serialNumber']])
      #ncatt_put(ncout, var3, "generic_name", variable_3)
      #if (!is.null(std_variable_3)){
      #  ncatt_put(ncout, var3, "standard_name", variable_3)
      #}
      ncatt_put(ncout, var3, "data_max", max(odf[[variable_3]], na.rm = TRUE))
      ncatt_put(ncout, var3, "data_min", min(odf[[variable_3]], na.rm = TRUE))
      ncatt_put(ncout, var3, "valid_max", var3max)
      ncatt_put(ncout, var3, "valid_min", var3min)


      if (numvar >3){
        ncatt_put(ncout, var4, "sensor_type", odf[['model']])
        ncatt_put(ncout, var4, "sensor_depth", odf[['depthMin']])
        ncatt_put(ncout, var4, "serial_number", odf[['serialNumber']])
        #ncatt_put(ncout, var4, "generic_name", variable_4)
        #if (!is.null(std_variable_4)){
        #  ncatt_put(ncout, var4, "standard_name", variable_4)
        #}
        ncatt_put(ncout, var4, "data_max", max(odf[[variable_4]], na.rm = TRUE))
        ncatt_put(ncout, var4, "data_min", min(odf[[variable_4]], na.rm = TRUE))
        ncatt_put(ncout, var4, "valid_max", var4max)
        ncatt_put(ncout, var4, "valid_min", var4min)


        if (numvar >4){
          ncatt_put(ncout, var5, "sensor_type", odf[['model']])
          ncatt_put(ncout, var5, "sensor_depth", odf[['depthMin']])
          ncatt_put(ncout, var5, "serial_number", odf[['serialNumber']])
          #ncatt_put(ncout, var5, "generic_name", variable_5)
          #if (!is.null(std_variable_5)){
          #  ncatt_put(ncout, var5, "standard_name", variable_5)
          #}
          ncatt_put(ncout, var5, "data_max", max(odf[[variable_5]], na.rm = TRUE))
          ncatt_put(ncout, var5, "data_min", min(odf[[variable_5]], na.rm = TRUE))
          ncatt_put(ncout, var5, "valid_max", var5max)
          ncatt_put(ncout, var5, "valid_min", var5min)


          if (numvar >5){
            ncatt_put(ncout, var6, "sensor_type", odf[['model']])
            ncatt_put(ncout, var6, "sensor_depth", odf[['depthMin']])
            ncatt_put(ncout, var6, "serial_number", odf[['serialNumber']])
            #ncatt_put(ncout, var6, "generic_name", variable_6)
            #if (!is.null(std_variable_6)){
            #  ncatt_put(ncout, var6, "standard_name", variable_6)
            #}
            ncatt_put(ncout, var6, "data_max", max(odf[[variable_6]], na.rm = TRUE))
            ncatt_put(ncout, var6, "data_min", min(odf[[variable_6]], na.rm = TRUE))
            ncatt_put(ncout, var6, "valid_max", var6max)
            ncatt_put(ncout, var6, "valid_min", var6min)


            if (numvar > 6){
              ncatt_put(ncout, var7, "sensor_type", odf[['model']])
              ncatt_put(ncout, var7, "sensor_depth", odf[['depthMin']])
              ncatt_put(ncout, var7, "serial_number", odf[['serialNumber']])
              #ncatt_put(ncout, var7, "generic_name", variable_7)
              #if (!is.null(std_variable_7)){
              #  ncatt_put(ncout, var7, "standard_name", variable_7)
              #}
              ncatt_put(ncout, var7, "data_max", max(odf[[variable_7]], na.rm = TRUE))
              ncatt_put(ncout, var7, "data_min", min(odf[[variable_7]], na.rm = TRUE))
              ncatt_put(ncout, var7, "valid_max", var7max)
              ncatt_put(ncout, var7, "valid_min", var7min)


              if (numvar > 7){
                ncatt_put(ncout, var8, "sensor_type", odf[['model']])
                ncatt_put(ncout, var8, "sensor_depth", odf[['depthMin']])
                ncatt_put(ncout, var8, "serial_number", odf[['serialNumber']])
                #ncatt_put(ncout, var8, "generic_name", variable_8)
                #if (!is.null(std_variable_8)){
                #  ncatt_put(ncout, var8, "standard_name", variable_8)
                #}
                ncatt_put(ncout, var8, "data_max", max(odf[[variable_8]], na.rm = TRUE))
                ncatt_put(ncout, var8, "data_min", min(odf[[variable_8]], na.rm = TRUE))
                ncatt_put(ncout, var8, "valid_max", var8max)
                ncatt_put(ncout, var8, "valid_min", var8min)


                if (numvar > 8){
                  ncatt_put(ncout, var9, "sensor_type", odf[['model']])
                  ncatt_put(ncout, var9, "sensor_depth", odf[['depthMin']])
                  ncatt_put(ncout, var9, "serial_number", odf[['serialNumber']])
                  #ncatt_put(ncout, var9, "generic_name", variable_9)
                  #if (!is.null(std_variable_9)){
                  #  ncatt_put(ncout, var9, "standard_name", variable_9)
                  #}
                  ncatt_put(ncout, var9, "data_max", max(odf[[variable_9]], na.rm = TRUE))
                  ncatt_put(ncout, var9, "data_min", min(odf[[variable_9]], na.rm = TRUE))
                  ncatt_put(ncout, var9, "valid_max", var9max)
                  ncatt_put(ncout, var9, "valid_min", var9min)


                  if (numvar >9){
                    ncatt_put(ncout, var10, "sensor_type", odf[['model']])
                    ncatt_put(ncout, var10, "sensor_depth", odf[['depthMin']])
                    ncatt_put(ncout, var10, "serial_number", odf[['serialNumber']])
                    #ncatt_put(ncout, var10, "generic_name", variable_10)
                    #if (!is.null(std_variable_10)){
                    #  ncatt_put(ncout, var10, "standard_name", variable_10)
                    #}
                    ncatt_put(ncout, var10, "data_max", max(odf[[variable_10]], na.rm = TRUE))
                    ncatt_put(ncout, var10, "data_min", min(odf[[variable_10]], na.rm = TRUE))
                    ncatt_put(ncout, var10, "valid_max", var10max)
                    ncatt_put(ncout, var10, "valid_min", var10min)


                    if (numvar >10){
                      ncatt_put(ncout, var11, "sensor_type", odf[['model']])
                      ncatt_put(ncout, var11, "sensor_depth", odf[['depthMin']])
                      ncatt_put(ncout, var11, "serial_number", odf[['serialNumber']])
                      #ncatt_put(ncout, var11, "generic_name", variable_11)
                      #if (!is.null(std_variable_11)){
                      #  ncatt_put(ncout, var11, "standard_name", variable_11)
                      #}
                      ncatt_put(ncout, var11, "data_max", max(odf[[variable_11]], na.rm = TRUE))
                      ncatt_put(ncout, var11, "data_min", min(odf[[variable_11]], na.rm = TRUE))
                      ncatt_put(ncout, var11, "valid_max", var11max)
                      ncatt_put(ncout, var11, "valid_min", var11min)


                      if (numvar >11){
                        ncatt_put(ncout, var12, "sensor_type", odf[['model']])
                        ncatt_put(ncout, var12, "sensor_depth", odf[['depthMin']])
                        ncatt_put(ncout, var12 , "serial_number", odf[['serialNumber']])
                        #ncatt_put(ncout, var12, "generic_name", variable_12)
                        #if (!is.null(std_variable_12)){
                        #  ncatt_put(ncout, var12, "standard_name", variable_12)
                        #}
                        ncatt_put(ncout, var12, "data_max", max(odf[[variable_12]], na.rm = TRUE))
                        ncatt_put(ncout, var12, "data_min", min(odf[[variable_12]], na.rm = TRUE))
                        ncatt_put(ncout, var12, "valid_max", var12max)
                        ncatt_put(ncout, var12, "valid_min", var12min)

                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }


  ####CF conventions & BODC standards####

  ncatt_put(ncout, 0, 'Conventions', 'CF-1.7')
  ncatt_put(ncout, 0, "creator_type", "person")

  ncatt_put(ncout, 0, "time_coverage_start", as.character(as.POSIXct(odf[['time']][1])))
  ncatt_put(ncout, 0, "time_coverage_end", as.character(as.POSIXct(tail(odf[['time']], n= 1))))
  ncatt_put(ncout, 0, "geospatial_lat_min", odf[['latitude']])
  ncatt_put(ncout, 0, "geospatial_lat_max", odf[['latitude']])
  ncatt_put(ncout, 0, "geospatial_lat_units", "degrees_north")
  ncatt_put(ncout, 0, "geospatial_lon_min", odf[['longitude']])
  ncatt_put(ncout, 0, "geospatial_lon_max", odf[['longitude']])
  ncatt_put(ncout, 0, "geospatial_lon_units", "degrees_east")
  ncatt_put(ncout, 0, "geospatial_vertical_max", odf[['depthMax']])
  ncatt_put(ncout, 0, "geospatial_vertical_min", odf[['depthMin']])
  ncatt_put(ncout, 0, "geospatial_vertical_units", "metres")
  ncatt_put(ncout, 0, "geospatial_vertical_positive", 'down')
  ncatt_put(ncout,0, "_FillValue", "1e35")
  ncatt_put(ncout, 0, "date_modified", date())
  ncatt_put(ncout, 0, "institution", odf[['institute']])

  ncatt_put(ncout, 0, "filename", gsub(".*M","",odf[['filename']]))
  ncatt_put(ncout, 0, "model", gsub(".*M","",odf[['model']]))
  ncatt_put(ncout, 0, "serialNumber", gsub(".*M","",odf[['serialNumber']]))
  ncatt_put(ncout, 0, "water_depth", gsub(".*M","",odf[['waterDepth']]))


  ncatt_put(ncout, 0, "country_institute_code", gsub(".*M","",odf[['countryInstituteCode']]))
  ncatt_put(ncout, 0, "institute", gsub(".*M","",odf[['institute']]))
  ncatt_put(ncout, 0, "sample_interval", gsub(".*M","",odf[['sampleInterval']]))
  ncatt_put(ncout, 0, "ship", gsub(".*M","",odf[['ship']]))

  ncatt_put(ncout, 0, "station", gsub(".*M","",odf[['station']]))
  ncatt_put(ncout, 0, "cruise", gsub(".*M","",odf[['cruise']]))
  ncatt_put(ncout, 0, "type", gsub(".*M","",odf[['type']]))
  ncatt_put(ncout, 0, "cruise_number", gsub(".*M","",odf[['cruiseNumber']]))
  ncatt_put(ncout, 0, "depth_off_bottom", gsub(".*M","",odf[['depthOffBottom']]))


  ####BODC P01 names####
  #ncatt_put(ncout, "ELTMEP01", "sdn_parameter_urn", "SDN:P01::ELTMEP01")
  #ncatt_put(ncout, "lon", "sdn_parameter_urn", "SDN:P01::ALONZZ01")
  #ncatt_put(ncout, "lat", "sdn_parameter_urn", "SDN:P01::ALATZZ01")
  #ncatt_put(ncout, "time_string", "sdn_parameter_urn", "SDN:P01::DTUT8601")



  ncatt_put(ncout, "longitude", "sdn_parameter_name", "Longitude east")
  ncatt_put(ncout, "latitude", "sdn_parameter_name", "Latitude north")
  #ncatt_put(ncout, 'ELTMEP01', "sdn_parameter_name", "Elapsed time (since 1970-01-01T00:00:00Z)")
  ncatt_put(ncout, 'time_string', "sdn_parameter_name", "String corresponding to format 'YYYY-MM-DDThh:mm:ss.sssZ' or other valid ISO8601 string")



  #ncatt_put(ncout, "lon", "sdn_uom_urn", "SDN:P06::DEGE")
  #ncatt_put(ncout, "lat", "sdn_uom_urn", "SDN:P06:DEGN")
  #ncatt_put(ncout, "ELTMEP01", "sdn_uom_urn", "SDN:P06::UTBB")
  #ncatt_put(ncout, "time_string", "sdn_uom_urn", "SDN:P06::TISO")



  ncatt_put(ncout, "longitude", "sdn_uom_name", "Degrees east")
  ncatt_put(ncout, "latitude", "sdn_uom_name", "Degrees north")
  #ncatt_put(ncout, "ELTMEP01", "sdn_uom_name", "Seconds")
  ncatt_put(ncout, "time_string", "sdn_uom_name", "ISO8601")


  #####CF standard names####
  #ncatt_put(ncout, "ELTMEP01", "standard_name", "time")
  #ncatt_put(ncout, "latitude", "standard_name", "latitude")
  #ncatt_put(ncout, "longitude", "standard_name", "longitude")

  ####data max and min####

  #metadata from spreadsheet

  if (!missing(metadata)) {
    metad <- read.csv(metadata, header = TRUE)

    mn <- as.character(metad[,1])
    mv <- as.character(metad[,2])


    md <- as.list(mv)
    names(md) <- mn

    for (m in seq_along(md)) {
      ncatt_put(ncout, 0, names(md)[m], md[[m]])
    }
  }

  ####preserve ODF history header####
  if (!is.null(odf@metadata$header)){
    if (length(odf@metadata$header) != 0){
      head <- odf@metadata$header
      hi <- list(grep(names(head), pattern = "HISTORY"))
      hist <- NULL
      for ( i in 1:length(hi[[1]])){
        hist[[i]] <- unlist(head[[hi[[1]][i]]])
      }
      histo <- unlist(hist)
      histor <- NULL
      for (i in 1:length(histo)){
        histor[[i]] <- paste(names(histo)[[i]],":", histo[[i]])
      }

      history <- unlist(histor)

      for (i in 1:length(history)){
        ncatt_put(ncout, 0, paste0("ODF_HISTORY_", i), history[[i]])
      }
      #PRESERVE EVENT_COMMENTS
      ec <- list(grep(names(head$EVENT_HEADER), pattern = 'EVENT_COMMENTS'))
      if (length(ec[[1]] != 0)){
        evc <- NULL
        for( i in 1:length(ec[[1]])){
          evc[[i]] <- unlist(head$EVENT_HEADER[[ec[[1]][i]]])
        }
        evec <- unlist(evc)
        evenc <- NULL
        for (i in 1:length(evec)){
          evenc[[i]] <- paste(names(evec)[[i]], ":", evec[[i]])
        }
        eventc <- unlist(evenc)
        for( i in 1:length(eventc)){
          ncatt_put(ncout, 0, paste0("EVENT_COMMENTS_", i), eventc[[i]])
        }
      }

    }
  }

  ####nc close####

  nc_close(ncout)




}


