#' Create ctd objects from Matlab CTD file with finite data
#'
#' This function removes NaN values in moored CTD Matlab files.
#' It starts by reading a matlab file [R.matlab::readMat()], then
#' cycling through the multiple profiles of multiple stations
#' to determine which data values are finite. The finite data
#' values are then used to create an oce ctd object using
#' [oce::as.ctd()].
#'
#' @param matfile a moored CTD matlab file
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#' @return a list of oce object with finite data values
#' from multiple stations
#' @importFrom R.matlab readMat
#' @importFrom oce as.ctd
#' @importFrom oce swSCTp
#' @importFrom oce numberAsPOSIXct
#' @examples
#' \dontrun{
#' library(oce)
#' library(R.matlab)
#' filesInst <- list.files(path="./20_rawDavis/instData/", full.names = TRUE)
#' x <- filesInst[[2]]
#' ctds <- matlabToOce(x)
#' }
#' @export

matlabToOce <- function(matfile=NULL, debug=0) { # is ctd matlab file
  if (debug > 0) {
    message("Step 1: About to read matlab file using readMat()")
  }
  matFilesInst <- readMat(matfile)

  names <- names(matFilesInst)
  if (debug > 0) {
    message("Step 2: Identified station names = ", paste0(names, sep=","))
  }

  M <- NULL
  for (i in seq_along(unname(matFilesInst))) {
    M[[i]] <- matFilesInst[[i]][,,1]
  }

  if (debug > 0) {
    message("Step 3: About to create storage for pressure, temperature, salinity, and oxygen")
  }

  pressure <- as.list(seq_along(names))
  names(pressure) <- names

  temperature <- as.list(seq_along(names))
  names(temperature) <- names

  salinity <- as.list(seq_along(names))
  names(salinity) <- names

  oxygen <- as.list(seq_along(names))
  names(oxygen) <- names

  conductivity <- as.list(seq_along(names))
  names(conductivity) <- names

  hp <- NULL
  ht <- NULL
  hs <- NULL
  ho <- NULL
  hc <- NULL

  if (debug > 0) {
    message("Step 4: Cycle through all profiles stations named ", paste0(names, sep=","), " to determine finite data values")
  }

  for (j in seq_along(names)) {
    for (i in seq_along(M[[j]]$lat[,1])) {
      if (!(is.null(M[[j]]$prDM))) {
        p <- M[[j]]$prDM[,i]
        ok <- is.finite(p)
        hp[[i]] <- p[ok]
      } else if (!(is.null(M[[j]]$pressure))) {
        p <- M[[j]]$pressure[,i]
        ok <- is.finite(p)
        hp[[i]] <- p[ok]
      } else {
        message("No pressure has been identified")
      }
      if (!(is.null(M[[j]]$t190C))) {
        t <- M[[j]]$t190C[,i][ok]
        ht[[i]] <- t[ok]
      } else if (!(is.null(M[[j]]$temperature))) {
        t <- M[[j]]$temperature[,i][ok]
        ht[[i]] <- t[ok]
      } else {
        message("No temperature has been identified")
      }
      if (!(is.null(M[[j]]$sal00))) {
        s <- M[[j]]$sal00[,i][ok]
        hs[[i]] <- s[ok]
      } else if (!(is.null(M[[j]]$salinity))) {
        s <- M[[j]]$salinity[,i][ok]
        hs[[i]] <- s[ok]
      } else {
        message("No salinity has been identified")
      }
      if (!(is.null(M[[j]]$o2ML.L))) {
        o <- M[[j]]$o2ML.L[,i][ok]
        ho[[i]] <- o[ok]
      }

      if (!(is.null(M[[j]]$conductivity))) {
        if (debug > 0) {
          message("Conductivity has been identified")
        }
        c <- M[[j]]$conductivity[,i][ok]
        hc[[i]] <- c[ok]
      } else {
        message("No conductivity has been identified")
      }

    }
    pressure[[j]] <- hp
    temperature[[j]] <- ht
    salinity[[j]] <- hs
    oxygen[[j]] <- ho
    conductivity[[j]] <- hc
  }

  if (debug > 0) {
    message("Step 5: Identified finite data")
  }

  if (debug > 0) {
    message("Step 6: About to create an oce ctd object")
  }

  # Step 3: Create ctd objects from matlab files
  c <- NULL
  ctds <- as.list(seq_along(names))
  names(ctds) <- names

  # FIXME: There is still x, yday, t090, sal11


  for (j in seq_along(M)) {
    for (i in seq_along(M[[j]]$lat[,1])) {
      #stop("JAIM length(conductivity) =", length(conductivity), " length(temperature) =", length(temperature))
      if (length(conductivity) == length(temperature)) {
        if (debug > 0) {
          message("conductivity has been located for the ctd object")
        }
        pressureLength <- length(pressure[[j]][[i]])
        if (pressureLength == 1) {
          if (debug > 0) {
            message('pressureLength has been identified as one')
          }
          pres <- rep(pressure[[j]][[i]], length(conductivity[[j]][[i]]))
        } else {
          pres <- pressure[[j]][[i]]
        }
        salinityCalc <- swSCTp(conductivity = conductivity[[j]][[i]], temperature=temperature[[j]][[i]], pressure=pres, eos="gsw", conductivityUnit = "S/m")
        object <- as.ctd(salinity=salinityCalc, temperature = temperature[[j]][[i]], pressure = pressure[[j]][[i]], longitude=M[[j]]$lon[,1][i], latitude=M[[j]]$lat[,1][i])
      } else if (inherits(class(salinity[[j]][[i]][1]), "complex")) {
        object <- as.ctd(salinity=Re(salinity[[j]][[i]]), temperature = temperature[[j]][[i]], pressure = pressure[[j]][[i]], longitude=M[[j]]$lon[,1][i], latitude=M[[j]]$lat[,1][i])
      } else {
        object <- as.ctd(salinity=salinity[[j]][[i]], temperature = temperature[[j]][[i]], pressure = pressure[[j]][[i]], longitude=M[[j]]$lon[,1][i], latitude=M[[j]]$lat[,1][i])
      }
      object <- oceSetMetadata(object, name="waterDepth", value=M[[j]]$bottom[,1][i])
      if (!(is.null(M[[j]]$o2ML.L))) {
        object <- oceSetData(object, name="oxygen", value=oxygen[[j]][[i]])
        object@metadata$units$oxygen$unit <- "mL/L"
        object@metadata$units$oxygen$scale <- ""
      }
      object <- oceSetMetadata(object, name="station", value=names[[j]])
      if (!(is.null(M[[j]]$yday))) {
        object <- oceSetData(object, name="time", value=rep(M[[j]]$yday[,1][i], length(temperature[[j]][[i]])))
      } else if (!(is.null(M[[j]]$datenum))) {
        object <- oceSetData(object, name="time", value=numberAsPOSIXct(as.vector(matFilesInst[[j]][,,1]$datenum), type="matlab"))
        if (debug > 0) {
          message("datenum has been identified. The length is ", length(as.vector(matFilesInst[[j]][,,1]$datenum)), " and the
                length of temperature is ", length(temperature[[j]][[i]]))
        }
      }
      object@metadata$units$time$unit <- "s"
      object@metadata$units$time$scale <- ""
      namesDON <- names(object[['data']])[-which(names(object[['data']]) == "scan")]
      object@metadata$dataNamesOriginal <- as.list(seq_along(namesDON))
      names(object@metadata$dataNamesOriginal) <- namesDON

      if (!is.null(M[[j]]$prDM)) {
        object@metadata$dataNamesOriginal$pressure <- "prDM"
      } else if (!(is.null(M[[j]]$pressure))) {
        object@metadata$dataNamesOriginal$pressure <- "pressure"
      }
      if (!(is.null(M[[j]]$t090C))) {
        object@metadata$dataNamesOriginal$temperature <- "t090C"
      } else if (!(is.null(M[[j]]$temperature))) {
        object@metadata$dataNamesOriginal$temperature <- "temperature"
      }
      if (!(is.null(M[[j]]$o2ML.L))) {
        object@metadata$dataNamesOriginal$oxygen <- "o2ML.L"
      }
      if (!(is.null(M[[j]]$sal11))) {
        object@metadata$dataNamesOriginal$salinity <- "sal11"
      } else if (!(is.null(M[[j]]$salinity))) {
        object@metadata$dataNamesOriginal$salinity <- "salinity"
      }
      if (!(is.null(M[[j]]$yday))) {
        object@metadata$dataNamesOriginal$time <- "yday"

      } else if (!(is.null(M[[j]]$datenum))) {
        object@metadata$dataNamesOriginal$time <- "datenum"

      }
      object@metadata$fileType <- "matlab"
      object@metadata$mooringType <- "mctd"
      c[[i]] <- object
    }

    ctds[[j]] <- c
  }




  return(ctds)
}


