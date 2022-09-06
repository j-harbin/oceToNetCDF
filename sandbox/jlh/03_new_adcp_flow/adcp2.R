library(oce)
# Step one
data <- getCFData(type="adcp")

# Step two
path <- "../davisStrait/data/mooring/2004-05/ADCP/BI3/"
files <- list.files(path=path, pattern="*.ODF", ignore.case=TRUE, full.names = TRUE)
adps <- lapply(files, read.oce)
source("sandbox/jlh/03_new_adcp_flow/compile2.R"); adps3 <- compile2(files, debug=1)
#source("sandbox/jlh/03_new_adcp_flow/test.R"); testADP(adps3, data=data, name="TEST", debug=1)

