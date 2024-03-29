#' Check if required metadata exists
#'
#' This function checks that an ODF object has all the metadata
#' required to build a complete netCDF file
#'
#' @param odf an odf object obtained from [oce::read.odf()]
#' @param data a data frame of standard name, name, units, and GF3 codes likely from getStandardData
#' @param print TRUE or FALSE, TRUE will cause errors to be displayed at command
#'   line, FALSE will sink errors into text document
#' @param debug integer value indicating level of debugging.
#' If this is less than 1, no debugging is done. Otherwise,
#' some functions will print debugging information
#'
#' @return a print statement of any issues with metadata to command line, if nothing
#'   prints then all metadata is intact
#' @examples
#' library(oceToNetCDF)
#' library(oce)
#' data <- getStandardData(type="ctd")
#' f <- system.file("extdata", "mctd.ODF", package="oceToNetCDF")
#' odf1 <- read.odf(f)
#' odfMetadataCheck(odf1, data=data)
#' @export
odfMetadataCheck <- function(odf, data=NULL, print = TRUE, debug=0) {
  if (is.null(data)) {
    stop("must provide a data frame for data")
  }
  if (!inherits(data, "data.frame")) {
    stop("data must be a data.frame class, not ", class(data))
  }
    if (print == FALSE) {
        name <- gsub(odf[['filename']], pattern = ".ODF", replacement = "")
    }
    if (is.null(odf[['longitude']]) | is.na(odf[["longitude"]])) {
        print('Missing Longitude Value!')
    }
    if (is.null(odf[['latitude']]) | is.na(odf[['latitude']])) {
        print('Missing Latitude Value!')
    }
    if (is.null(odf[['type']]) | is.na(odf[['type']])) {
        print('Missing type value!')
    }
    if (is.null(odf[['model']]) | is.na(odf[["model"]])) {
        print('Missing model value!')
    }
    if (is.null(odf[['samplingInterval']]) | is.na(odf[['samplingInterval']])) {
        print('Missing SamplingInterval value!')
    }
    if (is.null(odf[['countryInstituteCode']]) | odf[['countryInstituteCode']] == 0 | is.na(odf[['countryInstituteCode']])) {
        print('Missing CountryInstituteCode value!')
    }
    if (is.null(odf[['cruiseNumber']]) | is.na(odf[['cruiseNumber']])) {
        print('Missing cruiseNumber value!')
    }
    if (is.null(odf[['station']]) | is.na(odf[['station']])) {
        print('Missing station value!')
    }
    if (is.null(odf[['serialNumber']]) | is.na(odf[['serialNumber']])) {
        print('Missing serialNumber value!')
    }
    if (is.null(odf[['cruise']]) | is.na(odf[['cruise']])) {
        print('Missing cruise value')
    }
    if (is.null(odf[['sounding']]) | is.na(odf[['sounding']])) {
        print('Missing sounding value!')
    }

    if (is.null(odf[['scientist']]) | is.na(odf[['scientist']])) {
        print('Missing scientist value!')
    }

    if (is.null(odf[['waterDepth']]) | is.na(odf[['waterDepth']])) {
        print('Missing waterDepth value!')
    }
    if (is.null(odf[['depthMin']]) | is.na(odf[['depthMin']])) {
        print('Missing depthMin value')
    }
    if (is.null(odf[['depthMax']]) | is.na(odf[['depthMax']])) {
        print('Missing depthMax value')
    }
    if (is.null(odf[['institute']])) {
        print('Missing institute value')
    }
    #load('gf3-p01.RData')
    Names <- odf[['dataNamesOriginal']]
    names(Names) <- NULL

    if (debug > 0) {
        message("Names = ", Names)
    }

    names <- list()
    for (i in seq_along(Names)) {
        if (grepl("_", Names[i])) {
            names[i] <-gsub("\\_[0-9]$.*","",Names[i])
        } else {
            names[i] <- Names[i]
        }
    }

    if (debug > 0) {
        message("names = ", names)
    }

    l <- list()
    for (i in 1:length(names)) {
        l[[i]] <- grep(data$standard_name, pattern = names[[i]])
    }
    for (i in seq_along(l)) {
        if (length(l[[i]]) == 0) {
            print(paste("Original Data name does not conform to GF3 standards:", names[[i]], ", Please adjust!"))
        }
    }
    if (print == FALSE) {
        sink(NULL)
    }

}
