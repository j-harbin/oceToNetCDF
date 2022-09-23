library(oce)
# Step one
data <- getCFData(type="adcp")

# Step two
path <- "../davisStrait/data/mooring/2004/ADCP/odf/C1/"

#path <- "../davisStrait/data/mooring/2004-05/ADCP/BI3/"
files <- list.files(path=path, pattern="*.ODF", ignore.case=TRUE)
adp <- compileOdfToAdp(paste0(path,files), debug=1)

# Step three
singleAdpNetCDF(adp, name="2004-05.BI3", debug=1, data=data)
#singleAdpNetCDF(adp, name="2004-05.BI3", debug=1, data=data, destination="/Users/jaimiekeeping/Documents/GitHub/odfToNetCDF/R")
