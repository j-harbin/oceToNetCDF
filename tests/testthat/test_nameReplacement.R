## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(odfToNetCDF)

context("nameReplacement()")

library(odfToNetCDF)
library(oce)
data <- getData(type="ctd")
f <- system.file("extdata", "mctd.ODF", package="odfToNetCDF")
odf1 <- read.odf(f)

test_that("nameReplacement",
          {
            odf2 <- nameReplacement(odf1, data=data, unit="S/m")
            expect_equal(names(odf2[['data']])[1], "sea_water_temperature_1")

          }
)

test_that("errors",
          {
            expect_error(nameReplacement(odf1), "In nameReplacement, must provide a data frame for data")
            expect_error(nameReplacement(odf1, data=1:10), "In nameReplacement, data must be a data.frame class, not integer")


          }
)
