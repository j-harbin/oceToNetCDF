library(oce)
# Step 1
data <- getCFData(type="ctd")
# Step 2
files <- list.files("../davisStrait/data/mooring/2004/MCat/odf/", full.names = TRUE)
odfs <- lapply(files, read.odf)
#for (j in seq_along(odfs)) {
#  odfMetadataCheck(odfs[[j]], data=data)
#}

# Step 3
odfs <- lapply(odfs, function(x) nameReplacement(x, data=data, institute="UW/BIO", unit="S/m"))

# Step 4
odfs <- lapply(odfs, removeDerived)

# Step 5

odfs <- lapply(odfs, function(x) fixMetadata(x, data=data))

# Step 6

lapply(odfs, function(x) convertNetCDF(x, data=data))
#lapply(odfs, function(x) convertNetCDF(x, data=data, destination="/Users/jaimiekeeping/Documents/GitHub/odfToNetCDF/R"))




