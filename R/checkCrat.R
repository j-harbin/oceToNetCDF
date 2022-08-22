#' Check range of conductivity
#'
#' @param odf an odf object (oce::read.odf())
#'
#' @param unit the specified unit of conductivity to convert to
#' (Either 'S/m' or 'mS/cm')
#' @importFrom oce swCSTp
#' @return a print statement indicating if the conductivity
#' is in the expected range
#' @examples
#' \dontrun{
#' library(odfToNetCDF)
#' data <- getData(type="ctd")
#' odf1 <- read.odf("MCTD_KN179-05_1533_3309_1800.ODF")
#' odf2 <- nameReplacement(odf1, data=data)
#' odf3 <- removeDerived(odf2)
#' odf4 <- polishODF(odf3, data=data, unit='S/m')
#' odf5 <- checkCrat(odf3)
#' }
#' @export

checkCrat <- function(odf, unit="S/m") {
    if (!requireNamespace("oce", quietly=TRUE))
        stop("must install.packages(\"oce\") for checkCrat() to work")
    N <- 100
    TT <- seq(-2, 30, length.out = N)
    SS <- seq(20, 40, length.out = N)
    T <- expand.grid(TT, SS)[,1]
    S <- expand.grid(TT, SS)[,2]
    p <- rep(0, length(T))
    CSpm <- range(array(4.2914*(oce::swCSTp(S, T, p)), dim=c(N, N)))
    CmSpcm <- range(array(42.914*(oce::swCSTp(S, T, p)), dim=c(N, N)))

    names <- names(odf[['data']])
    keep <- which(grepl("sea_water_electrical_conductivity", names) == TRUE)
    number <- grepl("\\_[0-9]$", names[keep])

    if (number) {
        range <- range(unlist(unname(odf@data[names[keep]])))
    } else {
        range <- range(odf@data$sea_water_electrical_conductivity)

    }

    if ((range[1] < CSpm[1] | range[2] > CSpm[2]) && unit == "S/m") {
        message("WARNING: The expected range of sea_water_electrical_conductivity in S/m is ", paste0(CSpm, sep=", "), ". File ",gsub(".*M","",odf[['filename']]), " has a range of ", paste0(range, sep=", "),". Perhaps use polishODF() to convert crat to conductivity")
    } else if ((range[1] < CmSpcm[1] | range[2] > CmSpcm[2]) && unit =="mS/cm") {
        message("WARNING: The expected range of sea_water_electrical_conductivity in mS/cm is ", paste0(CmSpcm, sep=", "), ". File ",gsub(".*M","",odf[['filename']]), " has a range of ", paste0(range, sep=", "),". Perhaps use polishODF() to convert crat to conductivity")

    } else {
        message("sea_water_electrical_conductivity range is good for file ", gsub(".*M","",odf[['filename']]))
    }
}
