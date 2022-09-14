library(oce)
# Step 1
data <- getCFData(type="ctd")
# Step 2
files <- list.files("sandbox/jlh/04_issue26/", full.names = TRUE)
k <- grepl(pattern = ".ODF", files)
files <- files[k]
odfs <- read.odf(files)

# Step 3
odfs <- nameReplacement(odfs, data=data,institute="UW/BIO", unit="S/m", debug=1)

# Step 4
odfs <-removeDerived(odfs)

# Step 5

odfs <- fixMetadata(odfs, data=data)

# Step 6

convertNetCDF(odfs, data=data)
#lapply(odfs, function(x) convertNetCDF(x, data=data, destination="/Users/jaimiekeeping/Documents/GitHub/odfToNetCDF/R"))




