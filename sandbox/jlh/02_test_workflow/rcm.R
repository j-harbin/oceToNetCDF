# Step 1
data <- getData(type="rcm")
# Step 2
files <- list.files("../davisStrait/data/mooring/2004-05/RCM", full.names = TRUE)
odfs <- lapply(files, read.odf)
#for (j in seq_along(odfs)) {
#  odfMetadataCheck(odfs[[j]], data=data)
#}

# Step 3
odfs <- lapply(odfs, function(x) nameReplacement(x, data=data, institute = "UW/BIO"))

# Step 4
odfs <- lapply(odfs, removeDerived)

# Step 5

odfs <- lapply(odfs, function(x) polishODF(x, unit="S/m", data=data))

# Step 6

lapply(odfs, function(x) convertNetCDF(x, data=data))


# odfMetadataCheck
# checkCrat



