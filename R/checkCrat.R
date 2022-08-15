#' Check range of conductivity
#'
#' @param odf an odf object (oce::read.odf())
#'
#' @param unit the specified unit of conductivity to convert to
#' (Either 'S/m' or 'mS/cm')
#'
#' @return a print statement indicating if the conductivity
#' is in the expected range

checkCrat <- function(odf, unit="S/m") {
N <- 100
TT <- seq(-2, 30, length.out = N)
SS <- seq(20, 40, length.out = N)
T <- expand.grid(TT, SS)[,1]
S <- expand.grid(TT, SS)[,2]
p <- rep(0, length(T))
CSpm <- range(array(convertConductivityRatio(swCSTp(S, T, p)), dim=c(N, N)))

names <- names(odf[['data']])
keep <- which(grepl("sea_water_electrical_conductivity", names) == TRUE)
number <- grepl("\\_[0-9]$", names[keep])

if (number) {
  range <- range(unlist(unname(odf@data[names[keep]])))
} else {
  range <- range(odf@data$sea_water_electrical_conductivity)

}

if (range[1] < CSpm[1] | range[2] > CSpm[2]) {
message("WARNING: The expected range of sea_water_electrical_conductivity in S/m is ", paste0(CSpm, sep=", "), ". File ",gsub(".*M","",odf[['filename']]), " has a range of ", paste0(range, sep=", "))
} else {
  message("sea_water_electrical_conductivity range is good for file ", gsub(".*M","",odf[['filename']]))
}
}
