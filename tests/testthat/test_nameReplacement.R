## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(oceToNetCDF)

context("nameReplacement()")

library(oceToNetCDF)
library(oce)
data <- getStandardData(type="ctd")
f <- system.file("extdata", "mctd.ODF", package="oceToNetCDF")
odf1 <- read.odf(f)

test_that("nameReplacement",
          {
            odf2 <- nameReplacement(odf1, data=data, unit="S/m")
            expect_equal(names(odf2[['data']])[1], "sea_water_temperature")

          }
)

test_that("errors",
          {
            expect_error(nameReplacement(odf1), "In nameReplacement, must provide a data frame for data")
            expect_error(nameReplacement(odf1, data=1:10), "In nameReplacement, data must be a data.frame class, not integer")


          }
)
