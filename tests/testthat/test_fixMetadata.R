## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(odfToNetCDF)

context("fixMetadata")

library(odfToNetCDF)
library(oce)
data <- getCFData(type="ctd")
f <- system.file("extdata", "mctd.ODF", package="odfToNetCDF")
odf1 <- read.odf(f)
odf2 <- nameReplacement(odf1, data=data, unit="S/m")
odf3 <- removeDerived(odf2)

test_that("fixMetadata",
          {
            odf4 <- fixMetadata(odf3, data=data)
            expect_equal(names(odf4[['metadata']]$flags[1]), "sea_water_temperature_1")
            expect_equal(odf4[['metadata']]$units[[1]]$unit, "degree_C")


          }
)

test_that("errors",
          {
            expect_error(fixMetadata(odf3), "must provide a dataframe data, likely from getCFData()")
            expect_error(fixMetadata(odf1, data=1:10), "the data must be of class data.frame, not integer")


          }
)
