#' Structure adp data in matrix instead of array
#'
#' This function identifies when v, a (signal_intensity_from_multibeam
#' _acoustic_doppler_velocity_sensor_in_sea_water), q (percent_good_ping),
#' br (bottom_range), bv (bottom_velocity), ba, and bq
#' are stored within an adp object. If v is an array, the components get
#' broken up into eastward_sea_water_velocity, northward_sea_water_velocity,
#' upward_sea_water_velocity, and indicative_error_from_multibeam_acoustic
#' _doppler_velocity_profiler_in_sea_water. If q is identified as an array
#' the data is broken up into separate data named percent_good_ping_1,
#' percent_good_ping_2, etc. If it is identified as a matrix, it is renamed
#' to be average_percent_ping. The same can be said for a, expect as
#' signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water.
#' For bottom tracking, all data are broken up into vectors.
#' If a or q are identified as raw, it turns it into number.
#' Lastly, if the oceCoordinate is identified to be beam or xyz,
#' it is converted to be enu using [oce::beamToXyz()] and [oce::xyzToEnu()]
#' while correcting for declination using [oce::magneticField()].
#' If oceCoordinate is identified as enu, velocities are corrected
#' for declination using [oce::enuToOtherAdp].
#'
#' @param adp an adp object [oce::read.odf()]
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#' @return an adp object
#' @importFrom oce oceSetMetadata
#' @importFrom oce oceDeleteData
#' @importFrom oce beamToXyz
#' @importFrom oce xyzToEnu
#' @importFrom oce enuToOtherAdp
#' @importFrom oce magneticField
#'@examples
#' \dontrun{
#' library(odfToNetCDF)
#' data <- getCFData(type="adcp")
#' f1 <- system.file("extdata", "adcp1.ODF", package="odfToNetCDF")
#' f2 <- system.file("extdata", "adcp2.ODF", package="odfToNetCDF")
#' files <- c(f1,f2)
#' adp <- compileOdfToAdp(files)
#' adp2 <- nameReplacement(adp, data=data)
#' adp3 <- structureAdp(adp2)
#' names(adp2[['data']])
#' names(adp3[['data']])
#' }
#' @export

structureAdp <- function(adp, debug=0) {

  if (!inherits(adp, "adp")){
    stop("method is only for obects of class '", "adp", "'")
  }

  namesData <- names(adp[['data']])

  if (debug > 0) {
    message("namesData =", paste0(namesData, sep=","))
  }

  if ("northward_sea_water_velocity" %in% namesData) {
    if (debug > 0) {
      message("northward_sea_water_velocity identified")
    }
    if (!(is.null(dim(adp[["northward_sea_water_velocity"]])[3]))) {
      if (debug > 0) {
        message("It was not an average")
      }
      # This is not an average
      adp <- oceSetData(adp, name="eastward_sea_water_velocity", value=adp[["northward_sea_water_velocity"]][,,1])
      adp <- oceSetData(adp, name="northward_sea_water_velocity_1", value=adp[["northward_sea_water_velocity"]][,,2])
      adp <- oceSetData(adp, name="upward_sea_water_velocity", value=adp[["northward_sea_water_velocity"]][,,3])
      adp <- oceSetData(adp, name="indicative_error_from_multibeam_acoustic_doppler_velocity_profiler_in_sea_water", value=adp[["northward_sea_water_velocity"]][,,4])
      adp <- oceDeleteData(adp, name="northward_sea_water_velocity")
      adp <- oceSetData(adp, name="northward_sea_water_velocity", value=adp[["northward_sea_water_velocity_1"]])
      adp <- oceDeleteData(adp, name="northward_sea_water_velocity_1")
    }
  }
  if ("signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water" %in% namesData) {
    if (debug > 0) {
      message("signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water identified")
    }
    if (!(is.na(dim(adp[["signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water"]])[3]))) {
      if (debug > 0) {
        message("It was not an average")
      }
      if (unique(class(adp[['signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water']][,,1][,1])) == "raw") {
        qn <- as.numeric(adp[["signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water"]])
        dim(qn) <- dim(adp[['signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water']])
        adp[['signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water']] <- qn
      }

      # This is not an average
      adp <- oceSetData(adp, name="signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water_1", value=adp[["signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water"]][,,1])
      adp <- oceSetData(adp, name="signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water_2", value=adp[["signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water"]][,,2])
      adp <- oceSetData(adp, name="signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water_3", value=adp[["signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water"]][,,3])
      adp <- oceSetData(adp, name="signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water_4", value=adp[["signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water"]][,,4])
      adp <- oceDeleteData(adp, name="signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water")
    } else {
      if (debug > 0) {
        message("it was an average")
      }
      # This is an average
      if (unique(class(adp[['signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water']][,1][1])) == "raw") {
        qn <- as.numeric(adp[["signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water"]])
        dim(qn) <- dim(adp[['signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water']])
        adp[['signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water']] <- qn
      }
      adp <- oceSetData(adp, name="average_signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water", value=adp[["signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water"]])
      adp <- oceDeleteData(adp, name="signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water")
    }

  }

  if ("percent_good_ping" %in% namesData) {
    if (debug > 0) {
      message("percent_good_ping identified")
    }
    if (!(is.na(dim(adp[["percent_good_ping"]])[3]))) {
      if (debug > 0) {
        message("It was not an average")
      }
      # This is not an average
      if (unique(class(adp[['percent_good_ping']][,,1][,1])) == "raw") {
      qn <- as.numeric(adp[["percent_good_ping"]])
      dim(qn) <- dim(adp[['percent_good_ping']])
      adp[['percent_good_ping']] <- qn
      }
      adp <- oceSetData(adp, name="percent_good_ping_1", value=adp[["percent_good_ping"]][,,1])
      adp <- oceSetData(adp, name="percent_good_ping_2", value=adp[["percent_good_ping"]][,,2])
      adp <- oceSetData(adp, name="percent_good_ping_3", value=adp[["percent_good_ping"]][,,3])
      adp <- oceSetData(adp, name="percent_good_ping_4", value=adp[["percent_good_ping"]][,,4])
      adp <- oceDeleteData(adp, name="percent_good_ping")
    } else {
      if (debug > 0) {
        message("It was an average")
      }
      # This is an average
      if (unique(class(adp[['percent_good_ping']][,1][1])) == "raw") {
        qn <- as.numeric(adp[["percent_good_ping"]])
        dim(qn) <- dim(adp[['percent_good_ping']])
        adp[['percent_good_ping']] <- qn
      }

      adp <- oceSetData(adp, name="average_percent_good_ping", value=adp[["percent_good_ping"]])
      adp <- oceDeleteData(adp, name="percent_good_ping")
    }


  }






  if ("bottom_velocity" %in% namesData) {
    if (debug > 0) {
      message("bottom_velocity identified")
    }
      adp <- oceSetData(adp, name="bottom_velocity_1", value=adp[["bottom_velocity"]][,1])
      adp <- oceSetData(adp, name="bottom_velocity_2", value=adp[["bottom_velocity"]][,2])
      adp <- oceSetData(adp, name="bottom_velocity_3", value=adp[["bottom_velocity"]][,3])
      adp <- oceSetData(adp, name="bottom_velocity_4", value=adp[["bottom_velocity"]][,4])
      adp <- oceDeleteData(adp, name="bottom_velocity")
  }

  if ("bottom_range" %in% namesData) {
    if (debug > 0) {
      message("bottom_range identified")
    }
    adp <- oceSetData(adp, name="bottom_range_1", value=adp[["bottom_range"]][,1])
    adp <- oceSetData(adp, name="bottom_range_2", value=adp[["bottom_range"]][,2])
    adp <- oceSetData(adp, name="bottom_range_3", value=adp[["bottom_range"]][,3])
    adp <- oceSetData(adp, name="bottom_range_4", value=adp[["bottom_range"]][,4])
    adp <- oceDeleteData(adp, name="bottom_range")
  }

  if ("bottom_signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water" %in% namesData) {
    if (debug > 0) {
      message("bottom_signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water identified")
    }
    adp <- oceSetData(adp, name="bottom_signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water_1", value=adp[["bottom_signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water"]][,1])
    adp <- oceSetData(adp, name="bottom_signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water_2", value=adp[["bottom_signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water"]][,2])
    adp <- oceSetData(adp, name="bottom_signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water_3", value=adp[["bottom_signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water"]][,3])
    adp <- oceSetData(adp, name="bottom_signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water_4", value=adp[["bottom_signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water"]][,4])
    adp <- oceDeleteData(adp, name="bottom_signal_intensity_from_multibeam_acoustic_doppler_velocity_sensor_in_sea_water")
  }

  if ("bottom_percent_good_ping" %in% namesData) {
    if (debug > 0) {
      message("bottom_percent_good_ping identified")
    }
    adp <- oceSetData(adp, name="bottom_percent_good_ping_1", value=adp[["bottom_percent_good_ping"]][,1])
    adp <- oceSetData(adp, name="bottom_percent_good_ping_2", value=adp[["bottom_percent_good_ping"]][,2])
    adp <- oceSetData(adp, name="bottom_percent_good_ping_3", value=adp[["bottom_percent_good_ping"]][,3])
    adp <- oceSetData(adp, name="bottom_percent_good_ping_4", value=adp[["bottom_percent_good_ping"]][,4])
    adp <- oceDeleteData(adp, name="bottom_percent_good_ping")
  }

  if (!(is.null(adp[['oceCoordinate']]))) {
      if (debug > 0 ) {
          message("adp[['oceCoordinate']] is identified as ", adp[['oceCoordinate']])
      }
    adp <- oceSetMetadata(adp, name="northdec", value="magnetic") # Assuming declination needs to be corrected
      declination <- magneticField(longitude=rep(adp[['longitude']], length(adp[['time']])), latitude=rep(adp[['latitude']], length(adp[['time']])),time = adp[['time']])$declination
      if (adp[['oceCoordinate']] == "beam") {
          adp <- beamToXyz(adp)
          adp <- xyzToEnu(adp, declination=declination)
          adp <- oceSetMetadata(adp, name="oceCoordinate", value="enu")
          adp <- oceSetMetadata(adp, name="northdec", value="true")
      } else if (adp[['oceCoordinate']] == "xyz") {
          adp <- xyzToEnu(adp, declination=declination)
          adp <- oceSetMetadata(adp, name="oceCoordinate", value="enu")
          adp <- oceSetMetadata(adp, name="northdec", value="true")
      } else if (adp[['oceCoordinate']] == "enu") {
        # This will occur with oce
              #adp <- enuToOtherAdp(adp, heading=declination)
              #adp <- oceSetMetadata(adp, name="oceCoordinate", value="enu")
              #adp <- oceSetMetadata(adp, name="northdec", value="true")
      }

  }
  adp

}
