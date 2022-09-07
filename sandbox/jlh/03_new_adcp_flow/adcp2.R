library(oce)
# Step one
data <- getCFData(type="adcp")

# Step two
path <- "../davisStrait/data/mooring/2004-05/ADCP/BI3/"
files <- list.files(path=path, pattern="*.ODF", ignore.case=TRUE, full.names = TRUE)
adps <- lapply(files, read.oce)
source("sandbox/jlh/03_new_adcp_flow/compile2.R"); adps3 <- compile2(files, debug=1)
source("sandbox/jlh/03_new_adcp_flow/single2.R"); single2(adps3, data=data, name="TEST", debug=1)

#singleAdpNetCDF(adps3, data=data, name="j", debug=1)

#http://nco.sourceforge.net
# https://pro.arcgis.com/en/pro-app/latest/help/data/multidimensional/fundamentals-of-netcdf-data-storage.htm
