# Step one
data <- getCFData(type="adcp")

# Step two
path <- "../davisStrait/data/mooring/2004-05/ADCP/BI3/"
files <- list.files(path=path, pattern="*.ODF", ignore.case=TRUE, full.names = TRUE)
adps <- lapply(files, read.oce)
#adps2 <- lapply(adps, function(x) nameReplacement(x, data=data, debug=1))
source("sandbox/jlh/03_new_adcp_flow/compileAdps.R"); adps3 <- compileAdps(adps, debug=1)
