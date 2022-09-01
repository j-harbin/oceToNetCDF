#' Converts odf object to netCDF
#'
#' This function converts and odf object to a netCDF files for CTD
#' and RCM types.
#'
#' @param odf an odf object from oce which contains mctd or rcm data
#' @param data a data frame of standard name, name, units, and GF3 codes likely from [getCFData()]
#' @param filename the desired name for the netCDF file produced, if left NULL
#'   the default will conform to Bedford Institute of Oceanography ("BIO") naming conventions
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#' @return netCDF file with a maximum of 12 variables
#' @importFrom ncdf4 nc_close ncdim_def ncvar_def nc_create ncvar_put ncatt_put
#' @importFrom utils tail
#' @examples
#' \dontrun{
#' library(odfToNetCDF)
#' library(oce)
#' data <- getCFData(type="ctd")
#' f <- system.file("extdata", "mctd.ODF", package="odfToNetCDF")
#' odf1 <- read.odf(f)
#' odf2 <- nameReplacement(odf1, data=data, unit="S/m")
#' odf3 <- removeDerived(odf2)
#' odf4 <- fixMetadata(odf3, data=data)
#' convertNetCDF(odf4, data=data)
#' }
#' @export

convertNetCDF <- function(odf, filename = NULL, debug=0, data=NULL){
  if (is.null(data)) {
    stop("In convertNetCDF(), must provide a data frame for data")
  }
  if (!(class(data) == "data.frame")) {
    stop("In convertNetCDF(), data must be a data.frame class, not ", class(data))
  }

  if (!requireNamespace("oce", quietly=TRUE))
    stop("must install.packages(\"oce\") for convertNetCDF() to work")
  if (!requireNamespace("ncdf4", quietly=TRUE))
    stop("must install.packages(\"ncdf4\") for convertNetCDF() to work")
  MCTD <- grepl("MCTD", odf[['filename']])
  mctd <- grepl("mctd", odf[['filename']])
  RCM <- grepl("RCM", odf[['filename']])
  rcm <- grepl("rcm", odf[['filename']])
  v <- names(odf@data)

  if (MCTD | mctd) {
      DF <- data[which(data$code %in% c("SYTM", "CNDC", "PSAL", "TEMP", "PRES")),]
  } else if (RCM | rcm) {
      DF <- data[which(data$code %in% c("HCDT", "HCSP", "PRES", "PSAL", "SYTM", "TEMP")),]
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

 variable_1 <- var1 <- units1 <- variable_2 <- var2 <- units2 <-
    variable_3 <- var3 <- units3 <- variable_4 <- var4 <- units4 <-
    variable_5 <- var5 <- units5 <- variable_6 <- var6 <- units6 <-
    variable_7 <- var7 <- units7 <- variable_8 <- var8 <- units8 <-
    variable_9 <- var9 <- units9 <- variable_10 <- var10 <- units10 <-
    variable_11 <- var11 <- units11 <- variable_12 <- var12 <- units12 <-
    NULL
  for ( vv in VAR ){

    eval(parse(text = paste0("variable_", i, "<- '" , v[[i]], "'")))
    eval(parse(text= paste0("var",i," <-'", v[[i]],"'")))
    eval(parse(text = paste0("units", i, " <-'", vv$units, "'")))
    #eval(parse(text = paste0("generic_parameter_name", i, " <-'", vv$name, "'")))
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
    if (MCTD | mctd) {
    f <- gsub(".*M","",odf[['filename']])
    ff <-  gsub("\\..*","",f)
    filename <- paste0("M", ff, sep="")
    } else if (RCM | rcm) {
      f <- gsub(".*M","",odf[['filename']])
      ff <-  gsub("\\..*","",f)
      filename <- paste0("MCM", ff, sep="")
    }
  }
  ncpath <- "./"
  ncfname <- paste(ncpath, filename, ".nc", sep = "")

  if (debug > 0) {
    message("Step 5: Check dimensions of time, station, lon, lat, and dimnchar.")
  }
  #DIMENSIONS
  timedim <- ncdf4::ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", as.double(odf[['time']]))
  stationdim <- ncdf4::ncdim_def("station", "counts", as.numeric(length(odf[['station']])))
  londim <- ncdf4::ncdim_def("longitude", "degrees_east" , as.double(odf[['longitude']]))
  latdim <- ncdf4::ncdim_def("latitude", "degrees_north", as.double(odf[['latitude']]))
  dimnchar <- ncdf4::ncdim_def('nchar', '', 1:23, create_dimvar = FALSE)

  #FILLVALUE
  FillValue <- 1e35


  #VARIABLES

  if (debug > 0) {
    message("Step 6: About to define netCDF variablesusing ncdf4::ncvar_def.")
  }
  dlname <- 'longitude'
  lon_def <- ncdf4::ncvar_def(longname= "longitude", units = 'degrees_east', dim = stationdim, name = dlname, prec = 'double')

  dlname <- 'latitude'
  lat_def <- ncdf4::ncvar_def( longname = 'latitude', units = 'degrees_north', dim =  stationdim, name = dlname, prec = 'double')

  dlname <- "time_string"
  ts_def <- ncdf4::ncvar_def("DTUT8601", units = "",dim =  list( dimnchar, timedim), missval = NULL, name =  dlname, prec = "char")

  dlname <- variable_1
  v1_def <- ncdf4::ncvar_def(var1, units1, list(timedim, stationdim), FillValue, dlname, prec = 'double')

  if (numvar >1){
    dlname <- variable_2
    v2_def <- ncdf4::ncvar_def(var2, units2, list(timedim, stationdim), FillValue, dlname, prec = 'double')

    if (numvar >2){
      dlname <- variable_3
      v3_def <- ncdf4::ncvar_def(var3, units3, list(timedim, stationdim), FillValue, dlname, prec = 'double')

      if (numvar >3){
        dlname <- variable_4
        v4_def <- ncdf4::ncvar_def(var4, units4, list(timedim, stationdim), FillValue, dlname, prec = 'double')

        if (numvar >4){
          dlname <- variable_5
          v5_def <- ncdf4::ncvar_def(var5, units5, list(timedim, stationdim), FillValue, dlname, prec = 'double')

          if (numvar >5){
            dlname <- variable_6
            v6_def <- ncdf4::ncvar_def(var6, units6, list(timedim, stationdim), FillValue, dlname, prec = 'double')

            if (numvar >6){
              dlname <- variable_7
              v7_def <- ncdf4::ncvar_def(var7, units7, list(timedim, stationdim), FillValue, dlname, prec = 'double')

              if (numvar >7){
                dlname <- variable_8
                v8_def <- ncdf4::ncvar_def(var8, units8, list(timedim, stationdim), FillValue, dlname, prec = 'double')

                if (numvar >8){
                  dlname <- variable_9
                  v9_def <- ncdf4::ncvar_def(var9, units9, list(timedim, stationdim), FillValue, dlname, prec = 'double')

                  if (numvar >9){
                    dlname <- variable_10
                    v10_def <- ncdf4::ncvar_def(var10, units10, list(timedim, stationdim), FillValue, dlname, prec = 'double')

                    if (numvar > 10){
                      dlname <- variable_11
                      v11_def <- ncdf4::ncvar_def(var11, units11, list(timedim, stationdim), FillValue, dlname, prec = 'double')

                      if (numvar > 11){
                        dlname <- variable_12
                        v12_def <- ncdf4::ncvar_def(var12, units12, list(timedim, stationdim), FillValue, dlname, prec = 'double')

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
  message("Step 8: About to insert data to an existing netCDF using ncdf4::ncdf4::ncvar_put.")
  }

  ####INSERT DATA####
  ncdf4::ncvar_put(ncout, ts_def, odf[['time']])
  #ncdf4::ncvar_put(ncout, t_def, as.POSIXct(odf[['time']], tz = 'UTC', origin = '1970-01-01 00:00:00'))
  ncdf4::ncvar_put(ncout, lon_def, odf[['longitude']])
  ncdf4::ncvar_put(ncout, lat_def, odf[['latitude']])
  ncdf4::ncvar_put(ncout, v1_def, odf[[variable_1]])
  if (numvar >1){
    ncdf4::ncvar_put(ncout, v2_def, odf[[variable_2]])
    if (numvar >2){
      ncdf4::ncvar_put(ncout, v3_def, odf[[variable_3]])
      if (numvar >3){
        ncdf4::ncvar_put(ncout, v4_def, odf[[variable_4]])
        if (numvar >4){
          ncdf4::ncvar_put(ncout, v5_def, odf[[variable_5]])
          if (numvar >5){
            ncdf4::ncvar_put(ncout, v6_def, odf[[variable_6]])
            if (numvar >6){
              ncdf4::ncvar_put(ncout, v7_def, odf[[variable_7]])
              if (numvar >7){
                ncdf4::ncvar_put(ncout, v8_def, odf[[variable_8]])
                if (numvar >8){
                  ncdf4::ncvar_put(ncout, v9_def, odf[[variable_9]])
                  if (numvar >9){
                    ncdf4::ncvar_put(ncout, v10_def, odf[[variable_10]])
                    if (numvar >10){
                      ncdf4::ncvar_put(ncout, v11_def, odf[[variable_11]])
                      if(numvar >11){
                        ncdf4::ncvar_put(ncout, v12_def, odf[[variable_12]])
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
  ncdf4::ncatt_put(ncout, 'station', 'longitude', odf[['longitude']])
  ncdf4::ncatt_put(ncout, 'station', 'latitiude', odf[['latitude']])
  ncdf4::ncatt_put(ncout, 'station', 'standard_name', 'platform_name')
  ncdf4::ncatt_put(ncout, 'station', 'cf_role', 'timeseries_id')
  ncdf4::ncatt_put(ncout, 'time' , 'calendar', 'gregorian')
  #ncdf4::ncatt_put(ncout, 'time_string', 'note', 'time values as ISO8601 string, YY-MM-DD hh:mm:ss')
  ncdf4::ncatt_put(ncout, 'time_string', 'time_zone', 'UTC')

  #FROM ODF
  ncdf4::ncatt_put(ncout, 0, 'inst_type', odf[['type']])
  ncdf4::ncatt_put(ncout, 0, 'model', odf[['model']])
  ncdf4::ncatt_put(ncout, 0, 'sampling_interval', odf[['samplingInterval']])
  ncdf4::ncatt_put(ncout, 0, 'country_code', odf[['countryInstituteCode']])
  ncdf4::ncatt_put(ncout, 0, 'cruise_number', odf[['cruiseNumber']])
  ncdf4::ncatt_put(ncout, 0, "mooring_number", odf[['station']])
  ncdf4::ncatt_put(ncout, 0, "time_coverage_duration", (utils::tail(odf[['time']], n = 1) - odf[['time']][[1]]))
  ncdf4::ncatt_put(ncout, 0, "time_coverage_duration_units", "days")
  ncdf4::ncatt_put(ncout, 0, "cdm_data_type", "station")
  ncdf4::ncatt_put(ncout, 0, "serial_number", odf[['serialNumber']])
  ncdf4::ncatt_put(ncout, 0, "data_type", 'MCTD')
  ncdf4::ncatt_put(ncout, 0, "longitude", odf[['longitude']])
  ncdf4::ncatt_put(ncout, 0, "latitude", odf[['latitude']])
  ncdf4::ncatt_put(ncout, 0, "platform", odf[['cruise']])
  ncdf4::ncatt_put(ncout, 0, "sounding", odf[['sounding']])
  ncdf4::ncatt_put(ncout, 0, "chief_scientist", odf[['scientist']])
  ncdf4::ncatt_put(ncout, 0, "water_depth", odf[['waterDepth']])
  ncdf4::ncatt_put(ncout, 0, "cruise_name", odf[['cruise']])

  ####variable ATTRIBUTES####

  ncdf4::ncatt_put(ncout, var1, 'reference_scale', 'IPTS-68')


var1max <- var1min <- var2max <- var2min <- var3max <- var3min <-
  var4max <- var4min <- var5max <- var5min <- var6max <- var6min <-
  var7max <- var7min <- var8max <- var8min <- var9max <- var9min <-
  var10max <- var10min <- var11max <- var11min <- var12max <- var12min <-
  NULL

  ncdf4::ncatt_put(ncout, var1, "sensor_type", odf[['model']])
  ncdf4::ncatt_put(ncout, var1, "sensor_depth", odf[['depthMin']])
  ncdf4::ncatt_put(ncout, var1, "serial_number", odf[['serialNumber']])
  ncdf4::ncatt_put(ncout, var1, "generic_parameter_name", data$name[which(data$code == var[1])])


  #if (!is.null(std_variable_1)){
  #  ncdf4::ncatt_put(ncout, var1, "standard_name", variable_1)
  #}
  ncdf4::ncatt_put(ncout, var1, "data_max", max(odf[[variable_1]], na.rm = TRUE))
  ncdf4::ncatt_put(ncout, var1, "data_min", min(odf[[variable_1]], na.rm = TRUE))
  ncdf4::ncatt_put(ncout, var1, "valid_max", var1max)
  ncdf4::ncatt_put(ncout, var1, "valid_min", var1min)

  if (numvar > 1){
    ncdf4::ncatt_put(ncout, var2, "sensor_type", odf[['model']])
    ncdf4::ncatt_put(ncout, var2, "sensor_depth", odf[['depthMin']])
    ncdf4::ncatt_put(ncout, var2, "serial_number", odf[['serialNumber']])
    ncdf4::ncatt_put(ncout, var2, "generic_parameter_name", data$name[which(data$code == var[2])])

    #if (!is.null(std_variable_2)){
    #  ncdf4::ncatt_put(ncout, var2, "standard_name", variable_2)
    #}
    ncdf4::ncatt_put(ncout, var2, "data_max", max(odf[[variable_2]], na.rm = TRUE))
    ncdf4::ncatt_put(ncout, var2, "data_min", min(odf[[variable_2]], na.rm = TRUE))
    ncdf4::ncatt_put(ncout, var2, "valid_max", var2max)
    ncdf4::ncatt_put(ncout, var2, "valid_min", var2min)

    if (numvar >2){
      ncdf4::ncatt_put(ncout, var3, "sensor_type", odf[['model']])
      ncdf4::ncatt_put(ncout, var3, "sensor_depth", odf[['depthMin']])
      ncdf4::ncatt_put(ncout, var3, "serial_number", odf[['serialNumber']])
      ncdf4::ncatt_put(ncout, var3, "generic_parameter_name", data$name[which(data$code == var[3])])

            #if (!is.null(std_variable_3)){
      #  ncdf4::ncatt_put(ncout, var3, "standard_name", variable_3)
      #}
      ncdf4::ncatt_put(ncout, var3, "data_max", max(odf[[variable_3]], na.rm = TRUE))
      ncdf4::ncatt_put(ncout, var3, "data_min", min(odf[[variable_3]], na.rm = TRUE))
      ncdf4::ncatt_put(ncout, var3, "valid_max", var3max)
      ncdf4::ncatt_put(ncout, var3, "valid_min", var3min)


      if (numvar >3){
        ncdf4::ncatt_put(ncout, var4, "sensor_type", odf[['model']])
        ncdf4::ncatt_put(ncout, var4, "sensor_depth", odf[['depthMin']])
        ncdf4::ncatt_put(ncout, var4, "serial_number", odf[['serialNumber']])
        ncdf4::ncatt_put(ncout, var4, "generic_parameter_name", data$name[which(data$code == var[4])])

        #ncdf4::ncatt_put(ncout, var4, "generic_name", variable_4)
        #if (!is.null(std_variable_4)){
        #  ncdf4::ncatt_put(ncout, var4, "standard_name", variable_4)
        #}
        ncdf4::ncatt_put(ncout, var4, "data_max", max(odf[[variable_4]], na.rm = TRUE))
        ncdf4::ncatt_put(ncout, var4, "data_min", min(odf[[variable_4]], na.rm = TRUE))
        ncdf4::ncatt_put(ncout, var4, "valid_max", var4max)
        ncdf4::ncatt_put(ncout, var4, "valid_min", var4min)


        if (numvar >4){
          ncdf4::ncatt_put(ncout, var5, "sensor_type", odf[['model']])
          ncdf4::ncatt_put(ncout, var5, "sensor_depth", odf[['depthMin']])
          ncdf4::ncatt_put(ncout, var5, "serial_number", odf[['serialNumber']])
          ncdf4::ncatt_put(ncout, var5, "generic_parameter_name", data$name[which(data$code == var[5])])

          #ncdf4::ncatt_put(ncout, var5, "generic_name", variable_5)
          #if (!is.null(std_variable_5)){
          #  ncdf4::ncatt_put(ncout, var5, "standard_name", variable_5)
          #}
          ncdf4::ncatt_put(ncout, var5, "data_max", max(odf[[variable_5]], na.rm = TRUE))
          ncdf4::ncatt_put(ncout, var5, "data_min", min(odf[[variable_5]], na.rm = TRUE))
          ncdf4::ncatt_put(ncout, var5, "valid_max", var5max)
          ncdf4::ncatt_put(ncout, var5, "valid_min", var5min)


          if (numvar >5){
            ncdf4::ncatt_put(ncout, var6, "sensor_type", odf[['model']])
            ncdf4::ncatt_put(ncout, var6, "sensor_depth", odf[['depthMin']])
            ncdf4::ncatt_put(ncout, var6, "serial_number", odf[['serialNumber']])
            ncdf4::ncatt_put(ncout, var6, "generic_parameter_name", data$name[which(data$code == var[6])])

            #ncdf4::ncatt_put(ncout, var6, "generic_name", variable_6)
            #if (!is.null(std_variable_6)){
            #  ncdf4::ncatt_put(ncout, var6, "standard_name", variable_6)
            #}
            ncdf4::ncatt_put(ncout, var6, "data_max", max(odf[[variable_6]], na.rm = TRUE))
            ncdf4::ncatt_put(ncout, var6, "data_min", min(odf[[variable_6]], na.rm = TRUE))
            ncdf4::ncatt_put(ncout, var6, "valid_max", var6max)
            ncdf4::ncatt_put(ncout, var6, "valid_min", var6min)


            if (numvar > 6){
              ncdf4::ncatt_put(ncout, var7, "sensor_type", odf[['model']])
              ncdf4::ncatt_put(ncout, var7, "sensor_depth", odf[['depthMin']])
              ncdf4::ncatt_put(ncout, var7, "serial_number", odf[['serialNumber']])
              ncdf4::ncatt_put(ncout, var7, "generic_parameter_name", data$name[which(data$code == var[7])])

              #ncdf4::ncatt_put(ncout, var7, "generic_name", variable_7)
              #if (!is.null(std_variable_7)){
              #  ncdf4::ncatt_put(ncout, var7, "standard_name", variable_7)
              #}
              ncdf4::ncatt_put(ncout, var7, "data_max", max(odf[[variable_7]], na.rm = TRUE))
              ncdf4::ncatt_put(ncout, var7, "data_min", min(odf[[variable_7]], na.rm = TRUE))
              ncdf4::ncatt_put(ncout, var7, "valid_max", var7max)
              ncdf4::ncatt_put(ncout, var7, "valid_min", var7min)


              if (numvar > 7){
                ncdf4::ncatt_put(ncout, var8, "sensor_type", odf[['model']])
                ncdf4::ncatt_put(ncout, var8, "sensor_depth", odf[['depthMin']])
                ncdf4::ncatt_put(ncout, var8, "serial_number", odf[['serialNumber']])
                ncdf4::ncatt_put(ncout, var8, "generic_parameter_name", data$name[which(data$code == var[8])])

                #ncdf4::ncatt_put(ncout, var8, "generic_name", variable_8)
                #if (!is.null(std_variable_8)){
                #  ncdf4::ncatt_put(ncout, var8, "standard_name", variable_8)
                #}
                ncdf4::ncatt_put(ncout, var8, "data_max", max(odf[[variable_8]], na.rm = TRUE))
                ncdf4::ncatt_put(ncout, var8, "data_min", min(odf[[variable_8]], na.rm = TRUE))
                ncdf4::ncatt_put(ncout, var8, "valid_max", var8max)
                ncdf4::ncatt_put(ncout, var8, "valid_min", var8min)


                if (numvar > 8){
                  ncdf4::ncatt_put(ncout, var9, "sensor_type", odf[['model']])
                  ncdf4::ncatt_put(ncout, var9, "sensor_depth", odf[['depthMin']])
                  ncdf4::ncatt_put(ncout, var9, "serial_number", odf[['serialNumber']])
                  ncdf4::ncatt_put(ncout, var9, "generic_parameter_name", data$name[which(data$code == var[9])])

                  #ncdf4::ncatt_put(ncout, var9, "generic_name", variable_9)
                  #if (!is.null(std_variable_9)){
                  #  ncdf4::ncatt_put(ncout, var9, "standard_name", variable_9)
                  #}
                  ncdf4::ncatt_put(ncout, var9, "data_max", max(odf[[variable_9]], na.rm = TRUE))
                  ncdf4::ncatt_put(ncout, var9, "data_min", min(odf[[variable_9]], na.rm = TRUE))
                  ncdf4::ncatt_put(ncout, var9, "valid_max", var9max)
                  ncdf4::ncatt_put(ncout, var9, "valid_min", var9min)


                  if (numvar >9){
                    ncdf4::ncatt_put(ncout, var10, "sensor_type", odf[['model']])
                    ncdf4::ncatt_put(ncout, var10, "sensor_depth", odf[['depthMin']])
                    ncdf4::ncatt_put(ncout, var10, "serial_number", odf[['serialNumber']])
                    ncdf4::ncatt_put(ncout, var10, "generic_parameter_name", data$name[which(data$code == var[10])])

                    #ncdf4::ncatt_put(ncout, var10, "generic_name", variable_10)
                    #if (!is.null(std_variable_10)){
                    #  ncdf4::ncatt_put(ncout, var10, "standard_name", variable_10)
                    #}
                    ncdf4::ncatt_put(ncout, var10, "data_max", max(odf[[variable_10]], na.rm = TRUE))
                    ncdf4::ncatt_put(ncout, var10, "data_min", min(odf[[variable_10]], na.rm = TRUE))
                    ncdf4::ncatt_put(ncout, var10, "valid_max", var10max)
                    ncdf4::ncatt_put(ncout, var10, "valid_min", var10min)


                    if (numvar >10){
                      ncdf4::ncatt_put(ncout, var11, "sensor_type", odf[['model']])
                      ncdf4::ncatt_put(ncout, var11, "sensor_depth", odf[['depthMin']])
                      ncdf4::ncatt_put(ncout, var11, "serial_number", odf[['serialNumber']])
                      ncdf4::ncatt_put(ncout, var11, "generic_parameter_name", data$name[which(data$code == var[11])])

                      #ncdf4::ncatt_put(ncout, var11, "generic_name", variable_11)
                      #if (!is.null(std_variable_11)){
                      #  ncdf4::ncatt_put(ncout, var11, "standard_name", variable_11)
                      #}
                      ncdf4::ncatt_put(ncout, var11, "data_max", max(odf[[variable_11]], na.rm = TRUE))
                      ncdf4::ncatt_put(ncout, var11, "data_min", min(odf[[variable_11]], na.rm = TRUE))
                      ncdf4::ncatt_put(ncout, var11, "valid_max", var11max)
                      ncdf4::ncatt_put(ncout, var11, "valid_min", var11min)


                      if (numvar >11){
                        ncdf4::ncatt_put(ncout, var12, "sensor_type", odf[['model']])
                        ncdf4::ncatt_put(ncout, var12, "sensor_depth", odf[['depthMin']])
                        ncdf4::ncatt_put(ncout, var12 , "serial_number", odf[['serialNumber']])
                        ncdf4::ncatt_put(ncout, var12, "generic_parameter_name", data$name[which(data$code == var[12])])

                        #ncdf4::ncatt_put(ncout, var12, "generic_name", variable_12)
                        #if (!is.null(std_variable_12)){
                        #  ncdf4::ncatt_put(ncout, var12, "standard_name", variable_12)
                        #}
                        ncdf4::ncatt_put(ncout, var12, "data_max", max(odf[[variable_12]], na.rm = TRUE))
                        ncdf4::ncatt_put(ncout, var12, "data_min", min(odf[[variable_12]], na.rm = TRUE))
                        ncdf4::ncatt_put(ncout, var12, "valid_max", var12max)
                        ncdf4::ncatt_put(ncout, var12, "valid_min", var12min)

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

  ncdf4::ncatt_put(ncout, 0, 'Conventions', 'CF-1.7')
  ncdf4::ncatt_put(ncout, 0, "creator_type", "person")

  ncdf4::ncatt_put(ncout, 0, "time_coverage_start", as.character(as.POSIXct(odf[['time']][1])))
  ncdf4::ncatt_put(ncout, 0, "time_coverage_end", as.character(as.POSIXct(utils::tail(odf[['time']], n= 1))))
  ncdf4::ncatt_put(ncout, 0, "geospatial_lat_min", odf[['latitude']])
  ncdf4::ncatt_put(ncout, 0, "geospatial_lat_max", odf[['latitude']])
  ncdf4::ncatt_put(ncout, 0, "geospatial_lat_units", "degrees_north")
  ncdf4::ncatt_put(ncout, 0, "geospatial_lon_min", odf[['longitude']])
  ncdf4::ncatt_put(ncout, 0, "geospatial_lon_max", odf[['longitude']])
  ncdf4::ncatt_put(ncout, 0, "geospatial_lon_units", "degrees_east")
  ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_max", odf[['depthMax']])
  ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_min", odf[['depthMin']])
  ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_units", "metres")
  ncdf4::ncatt_put(ncout, 0, "geospatial_vertical_positive", 'down')
  ncdf4::ncatt_put(ncout,0, "_FillValue", "1e35")
  ncdf4::ncatt_put(ncout, 0, "date_modified", date())
  ncdf4::ncatt_put(ncout, 0, "institution", odf[['institute']])

  ncdf4::ncatt_put(ncout, 0, "filename", gsub(".*M","",odf[['filename']]))
  ncdf4::ncatt_put(ncout, 0, "model", gsub(".*M","",odf[['model']]))
  ncdf4::ncatt_put(ncout, 0, "serialNumber", gsub(".*M","",odf[['serialNumber']]))
  ncdf4::ncatt_put(ncout, 0, "water_depth", gsub(".*M","",odf[['waterDepth']]))


  ncdf4::ncatt_put(ncout, 0, "country_institute_code", gsub(".*M","",odf[['countryInstituteCode']]))
  ncdf4::ncatt_put(ncout, 0, "institute", gsub(".*M","",odf[['institute']]))
  ncdf4::ncatt_put(ncout, 0, "sample_interval", gsub(".*M","",odf[['sampleInterval']]))
  ncdf4::ncatt_put(ncout, 0, "ship", gsub(".*M","",odf[['ship']]))

  ncdf4::ncatt_put(ncout, 0, "station", gsub(".*M","",odf[['station']]))
  ncdf4::ncatt_put(ncout, 0, "cruise", gsub(".*M","",odf[['cruise']]))
  ncdf4::ncatt_put(ncout, 0, "type", gsub(".*M","",odf[['type']]))
  ncdf4::ncatt_put(ncout, 0, "cruise_number", gsub(".*M","",odf[['cruiseNumber']]))
  ncdf4::ncatt_put(ncout, 0, "depth_off_bottom", gsub(".*M","",odf[['depthOffBottom']]))


  ####BODC P01 names####
  #ncdf4::ncatt_put(ncout, "ELTMEP01", "sdn_parameter_urn", "SDN:P01::ELTMEP01")
  #ncdf4::ncatt_put(ncout, "lon", "sdn_parameter_urn", "SDN:P01::ALONZZ01")
  #ncdf4::ncatt_put(ncout, "lat", "sdn_parameter_urn", "SDN:P01::ALATZZ01")
  #ncdf4::ncatt_put(ncout, "time_string", "sdn_parameter_urn", "SDN:P01::DTUT8601")



  ncdf4::ncatt_put(ncout, "longitude", "generic_parameter_name", "Longitude east")
  ncdf4::ncatt_put(ncout, "latitude", "generic_parameter_name", "Latitude north")
  ncdf4::ncatt_put(ncout, 'time_string', "generic_parameter_name", "String corresponding to format 'YYYY-MM-DDThh:mm:ss.sssZ' or other valid ISO8601 string")



  #ncdf4::ncatt_put(ncout, "lon", "sdn_uom_urn", "SDN:P06::DEGE")
  #ncdf4::ncatt_put(ncout, "lat", "sdn_uom_urn", "SDN:P06:DEGN")
  #ncdf4::ncatt_put(ncout, "ELTMEP01", "sdn_uom_urn", "SDN:P06::UTBB")
  #ncdf4::ncatt_put(ncout, "time_string", "sdn_uom_urn", "SDN:P06::TISO")



  ncdf4::ncatt_put(ncout, "longitude", "sdn_uom_name", "Degrees east")
  ncdf4::ncatt_put(ncout, "latitude", "sdn_uom_name", "Degrees north")
  #ncdf4::ncatt_put(ncout, "ELTMEP01", "sdn_uom_name", "Seconds")
  ncdf4::ncatt_put(ncout, "time_string", "sdn_uom_name", "ISO8601")


  #####CF standard names####
  #ncdf4::ncatt_put(ncout, "ELTMEP01", "standard_name", "time")
  #ncdf4::ncatt_put(ncout, "latitude", "standard_name", "latitude")
  #ncdf4::ncatt_put(ncout, "longitude", "standard_name", "longitude")

  ####data max and min####

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
        ncdf4::ncatt_put(ncout, 0, paste0("ODF_HISTORY_", i), history[[i]])
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
          ncdf4::ncatt_put(ncout, 0, paste0("EVENT_COMMENTS_", i), eventc[[i]])
        }
      }

    }
  }

  ####nc close####

  ncdf4::nc_close(ncout)




}


