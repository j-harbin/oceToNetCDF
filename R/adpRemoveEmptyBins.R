#' Remove adp bins that contain all NA values
#'
#' This function removes distance bins that have been flagged as bad,
#' likely from [oce::adpFlagPastBoundary] and then set to NA, likely from
#' [oce::handleFlags]. It does this by identifying the minimum bin that contains
#' all NA values. If this minimum bin is found to be within the upper 30 m,
#' and likely a result of manual flagging due to another instrument on the moored
#' line, it will skip that and move to the next one. The fields with the
#' same dimensions as "v", as well as the "distance" variable, and "flags"
#' are then replaced to only contain the distance bins with at least one finite value.
#'
#' @param adp an [adp-class] object containing bottom ranges.
#'
#' @param fields a variable contained within `adp` indicating which field to flag.
#' If NULL (the default) then [adpRemoveEmptyBins()] applies itself to all flag
#' fields that have the same dimensions as `v` in the `data` slot.
#'
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#'
#' @importFrom oce oceDeleteData
#'
#' @importFrom oce oceSetData
#'
#' @return `adpRemoveEmptyBins` returns an [adp-class] object only
#' distances with finite values
#'
#' @author Jaimie Harbin
#'
#' @examples
#' \dontrun{
#' library(oce)
#' library(odfToNetCDF)
#' load(system.file("extdata", "adcpNA", package="odfToNetCDF"))
#' adp2 <- adpFlagPastBoundary(adcpNA)
#' adp3 <- adpConvertRawToNumeric(adp2)
#' adp4 <- handleFlags(adp3, flags=4)
#' adp5 <- adpRemoveEmptyBins(adp4, debug=1)
#' par(mfrow=c(2,1))
#' plot(adp4, which="a1")
#' plot(adp5, which="a1")
#' }
#'
#' @export

adpRemoveEmptyBins <- function(adp, debug=1, fields=NULL) {

    if (!inherits(adp, "adp")) {
        stop("adp must be an adp object")
    }

    dimNeeded <- dim(adp[["v"]])
    if (is.null(fields)) {
        dataNames <- names(adp@data)
        keep <- sapply(dataNames,
            function(variableTrial) {
                dimtest <- dim(adp[[variableTrial]])
                length(dimtest) == 3 && all(dimtest == dimNeeded)
            })
        fields <- dataNames[keep]
        if (debug > 0) {
            message("inferred fields =", paste0(fields, collapse=","))
        }
    }

    numberOfDistances <- dim(adp[['v']])[2]
    numberOfBeams <- dim(adp[['v']])[3]
    if (adp[["oceCoordinate"]] %in% c("enu", "xyz", "other")) {
        if (debug > 0) {
            message("The oceCoordinate was identified to be ", adp[["oceCoordinate"]])
        }
        nonValues <- NULL
        for (i in 1:numberOfDistances) {
            bad <- (any(is.finite(adp[['v']][,,1][,i]))) # don't need to do beam by beam because these are averages
            if (bad == FALSE) {
                nonValues[[i]] <- i
            }
        }
    } else if (adp[["oceCoordinate"]] == "beam") {
        stop("As of 2023-02-24, cannot set trim adp objects with oceCoordinate == 'beam'")

        #   nonValues <- vector("list", numberOfBeams)
        #   for (i in 1:numberOfDistances) {
        #       for (j in 1:numberOfBeams) {
        #           bad <- (any(is.finite(adp[['v']][,,j][,i]))) # don't need to do beam by beam because these are averages
        #           if (bad == FALSE) {
        #               nonValues[[j]][i] <- i
        #           }
        #       }
        #   }
        #   nonValues <- lapply(nonValues, function(x) x[which(is.finite(x))])

        #   minDistance <- lapply(nonValues, function(x) min(x, na.rm=TRUE)) # This is looking for NAs near surface
        #   #This is where it starts to not work
        #   keep <- vector("list", numberOfBeams)
    }

    # Then for fields of non values I do something like this:
    # Where 20 would be max non value
    nonValues <- unlist(nonValues)
    # Can't just take the min incase I removed bins already. Skip the first 30 m
    if (any(is.finite(nonValues))) {
        minDistance <- min(nonValues) # This is looking for NAs near surface

        if (adp[["distance"]][minDistance] < 30) { # Skipping first 30 m (this could be improved)
            message("A NA bin has been located in the upper 30 m of the water column")
            keep <- NULL
            for (i in seq_along(nonValues)) {
                if (adp[["distance"]][nonValues[i]] > 30) {
                    keep[[i]] <- adp[['distance']][nonValues[[i]]]
                }
            }
            minDistance <- which(adp[['distance']] == min(unlist(keep)))
        }
        if (debug > 0) {
            message("minDistance = ", minDistance)
        }

        if (debug > 0) {
            #par(mfrow=c(2,1))
            #plot(adp, which="a1")
            #abline(h=adp[['distance']][minDistance], col="magenta")
            #plot(adp, which="a1", ylim=c(0,adp[['distance']][minDistance]))
        }
        newData <- array(1, dim=c(dim(adp[["v"]])[1], minDistance, numberOfBeams))
        list <- vector("list",length(fields))

        for (i in seq_along(list)) {
            list[[i]] <- newData
        }

        names(list) <- fields
        flags <- vector("list", length(fields))
        for (i in seq_along(flags)) {
            flags[[i]] <- newData
        }
        names(flags) <- fields
        for (i in 1:numberOfBeams) {
            for (j in seq_along(fields)) {
                list[[j]][,,i] <- adp[[fields[[j]]]][,,i][,1:minDistance]
                flags[[j]][,,i] <- adp[["flags"]][[fields[[j]]]][,,i][,1:minDistance]
            }
        }
        for (i in seq_along(fields)) {
            adp <- oceDeleteData(adp, name=fields[i])
            adp <- oceSetData(adp, name=fields[i], value=list[[i]])
            adp <- oceDeleteMetadata(adp, name="flags")
            adp <- oceSetMetadata(adp, name="flags", value=flags)
        }
        adp <- oceSetData(adp, name="distance", value=adp[["distance"]][1:minDistance])
    } else {
        if (debug > 0) {
            message("Nothing to trim for this file")
        }
    }
    return(adp)
}
