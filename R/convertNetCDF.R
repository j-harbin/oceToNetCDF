#' Converts odf object to netCDF
#'
#' This function converts an odf object to a NetCDF file for CTD
#' and RCM types.
#'
#' @param odf an odf object from oce which contains mctd or rcm data
#' @param data a data frame of standard name, name, units, and GF3 codes likely from [getCFData()]
#' @param filename the desired name for the netCDF file produced (not including the extension), if left NULL
#'   the default will conform to Bedford Institute of Oceanography ("BIO") naming conventions
#'@param destination the specified location to save the NetCDF. By default this is set
#' to the local directory
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

convertNetCDF <- function(odf, filename = NULL, debug=0, data=NULL, destination="."){
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





  if (grepl("MCTD", odf[['filename']]) == FALSE && grepl("mctd", odf[['filename']]) == FALSE && grepl("RCM", odf[['filename']]) == FALSE
      && grepl("rcm", odf[['filename']]) == FALSE && is.null(odf[['mooringType']])) {
    stop("Type of file not found in filename. Set your odf[['mooringType']] to be either mctd, rcm, or adcp. See help page for details")
  }

  if (grepl("MCTD", odf[['filename']]) | !(is.null(odf[['mooringType']])) && odf[['mooringType']] == "mctd") {
    MCTD <- TRUE
  } else {
    MCTD <- FALSE
  }

  if (grepl("mctd", odf[['filename']]) | !(is.null(odf[['mooringType']])) && odf[['mooringType']] == "mctd") {
    mctd <- TRUE
  } else {
    mctd <- FALSE
  }
  if (grepl("RCM", odf[['filename']]) | !(is.null(odf[['mooringType']])) && odf[['mooringType']] == "rcm") {
    RCM <- TRUE
  } else {
    RCM <- FALSE
  }
  if (grepl("rcm", odf[['filename']]) | !(is.null(odf[['mooringType']])) && odf[['mooringType']] == "rcm") {
    rcm <- TRUE
  } else {
    rcm <- FALSE
  }



  v <- names(odf@data)

  if (MCTD | mctd) {
      DF <- data[which(data$code %in% c("SYTM", "CNDC", "PSAL", "TEMP", "PRES", "FLOR")),]
  } else if (RCM | rcm) {
      DF <- data[which(data$code %in% c("HCDT", "HCSP", "PRES", "PSAL", "SYTM", "TEMP", "EWCT", "NSCT")),]
  } else {
      message("Unrecognizable file type.")
  }

  var <- list()
  for (i in v) {
    code <- data$code[which(data$standard_name %in% gsub("_[0-9]$.*","",i))][1]
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
  if (debug > 0) {
    message("After removing time, var =", paste0(var, sep=","))
  }

  #POPULATE VARIABLES WITH APPROPRIATE CODES
  VAR <- list()
  if (debug > 0) {
    message("Step 2: About to determine units and standard_name for each code.")
    }


  for (i in seq_along(var)) {
    VAR[[i]] <- standardName(var[[i]], data=data)
  }
  i <- 1

  if (debug > 0) {
    message("class of VAR IS ",class(VAR), " and the names in the first var is =", names(VAR[[1]]),". The lenght of VAR IS ", length(VAR))
    message("The gf3 is ", VAR[[1]]$gf3)

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
  for (vv in VAR) {
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
  if (debug > 0) {
      message("numvar is equal to = ", numvar)
  }

  #FILENAME
  if (missing(filename)) {
    filename <- "MCAT"
    }


  ncpath <- destination
  ncfname <- paste(ncpath,"/", filename, ".nc", sep = "")

  if (debug > 0) {
    message("filename = ", filename, " and ncfname =", ncfname)
  }

  if (debug > 0) {
    message("Step 5: Check dimensions of time, station, lon, lat, and dimnchar.")
  }
  #DIMENSIONS
  timedim <- ncdf4::ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", as.double(odf[['time']]))
  stationdim <- ncdf4::ncdim_def("station", "counts", as.numeric(length(odf[['station']])))
  londim <- ncdf4::ncdim_def("longitude", "degrees_east" , as.double(odf[['longitude']]))
  latdim <- ncdf4::ncdim_def("latitude", "degrees_north", as.double(odf[['latitude']]))
  dimnchar <- ncdf4::ncdim_def('nchar', '', 1:23, create_dimvar = FALSE)

  #if (debug > 0) {
  #    message("timedim= ", timedim)
  #}

  #FILLVALUE
  FillValue <- 1e35


  #VARIABLES

  if (debug > 0) {
    message("Step 6: About to define netCDF variables using ncdf4::ncvar_def.")
  }
  dlname <- 'longitude'
  lon_def <- ncdf4::ncvar_def(longname= "longitude", units = 'degrees_east', dim = stationdim, name = dlname, prec = 'double')

  dlname <- 'latitude'
  lat_def <- ncdf4::ncvar_def( longname = 'latitude', units = 'degrees_north', dim =  stationdim, name = dlname, prec = 'double')

  dlname <- "time_string"
  ts_def <- ncdf4::ncvar_def("DTUT8601", units = "",dim =  list( dimnchar, timedim), missval = NULL, name =  dlname, prec = "char")

  dlname <- variable_1
  v1_def <- ncdf4::ncvar_def(ifelse(is.null(data$bodc), var1, data$bodc[which(data$standard_name == var1)]), units1, list(timedim, stationdim), FillValue, longname=unique(data$name[which(data$standard_name == variable_1)]), prec = 'double')

  if (numvar >1){
    dlname <- variable_2
    v2_def <- ncdf4::ncvar_def(ifelse(is.null(data$bodc), var2, data$bodc[which(data$standard_name == var2)]), units2, list(timedim, stationdim), FillValue, longname=unique(data$name[which(data$standard_name == variable_2)]), prec = 'double')

    if (numvar >2){
      dlname <- variable_3
      v3_def <- ncdf4::ncvar_def(ifelse(is.null(data$bodc), var3, data$bodc[which(data$standard_name == var3)]), units3, list(timedim, stationdim), FillValue, longname=unique(data$name[which(data$standard_name == variable_3)]), prec = 'double')
      if (numvar >3){
        dlname <- variable_4
        v4_def <- ncdf4::ncvar_def(ifelse(is.null(data$bodc), var4, data$bodc[which(data$standard_name == var4)]), units4, list(timedim, stationdim), FillValue, longname=unique(data$name[which(data$standard_name == variable_4)]), prec = 'double')

        if (numvar >4){
          dlname <- variable_5
          v5_def <- ncdf4::ncvar_def(ifelse(is.null(data$bodc), var5, data$bodc[which(data$standard_name == var5)]), units5, list(timedim, stationdim), FillValue, longname=unique(data$name[which(data$standard_name == variable_5)]), prec = 'double')

          if (numvar >5){
            dlname <- variable_6
            v6_def <- ncdf4::ncvar_def(ifelse(is.null(data$bodc), var6, data$bodc[which(data$standard_name == var6)]), units6, list(timedim, stationdim), FillValue, longname=unique(data$name[which(data$standard_name == variable_6)]), prec = 'double')

            if (numvar >6){
              dlname <- variable_7
              v7_def <- ncdf4::ncvar_def(ifelse(is.null(data$bodc), var7, data$bodc[which(data$standard_name == var7)]), units7, list(timedim, stationdim), FillValue, longname=unique(data$name[which(data$standard_name == variable_7)]), prec = 'double')

              if (numvar >7){
                dlname <- variable_8
                v8_def <- ncdf4::ncvar_def(ifelse(is.null(data$bodc), var8, data$bodc[which(data$standard_name == var8)]), units8, list(timedim, stationdim), FillValue, longname=unique(data$name[which(data$standard_name == variable_8)]), prec = 'double')

                if (numvar >8){
                  dlname <- variable_9
                  v9_def <- ncdf4::ncvar_def(ifelse(is.null(data$bodc), var9, data$bodc[which(data$standard_name == var9)]), units9, list(timedim, stationdim), FillValue, longname=unique(data$name[which(data$standard_name == variable_9)]), prec = 'double')

                  if (numvar >9){
                    dlname <- variable_10
                    v10_def <- ncdf4::ncvar_def(ifelse(is.null(data$bodc), var10, data$bodc[which(data$standard_name == var10)]), units10, list(timedim, stationdim), FillValue, longname=unique(data$name[which(data$standard_name == variable_10)]), prec = 'double')

                    if (numvar > 10){
                      dlname <- variable_11
                      v11_def <- ncdf4::ncvar_def(ifelse(is.null(data$bodc), var11, data$bodc[which(data$standard_name == var11)]), units11, list(timedim, stationdim), FillValue, longname=unique(data$name[which(data$standard_name == variable_11)]), prec = 'double')

                      if (numvar > 11){
                        dlname <- variable_12
                        v12_def <- ncdf4::ncvar_def(ifelse(is.null(data$bodc), var12, data$bodc[which(data$standard_name == var12)]), units12, list(timedim, stationdim), FillValue, longname=unique(data$name[which(data$standard_name == variable_12)]), prec = 'double')

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
  if (debug > 0) {
    message("defs =", defs)
  }
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
  #browser()

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
   bad <- which(names(odf[['metadata']]) %in% c("units", "header", "hexfilename", "filename", "dataNamesOriginal", "flags", "waterDepth", "date", "recoveryTime", "sampleInterval"))
  namesMeta <- names(odf[['metadata']])[-bad]
  for (i in seq_along(namesMeta)) {
      #message("This is for namesMeta = ", namesMeta[i])
      ncdf4::ncatt_put(ncout, 0, namesMeta[i], odf[[namesMeta[i]]])
  }

  ####preserve ODF history header####
  if (!is.null(odf@metadata$header)){
    if (length(odf@metadata$header) != 0){
      head <- odf@metadata$header
      hi <- list(grep(names(head), pattern = "HISTORY"))
      if (!(length(unlist(hi)) == 0)) {
        if (debug > 0) {
          message('were not doing header')
        }
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
      }
      #PRESERVE EVENT_COMMENTS
      if (!(is.null(names(head)))) {
        if (debug >0) {
          message('were not doing event comments')
        }
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
  }

  ####nc close####
  ncdf4::nc_close(ncout)

}


