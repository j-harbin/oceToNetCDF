## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(oceToNetCDF)
library(testthat)

context("nameReplacement()")

library(oceToNetCDF)
library(oce)
data <- getStandardData(type="ctd")
f <- system.file("extdata", "mctd.ODF", package="oceToNetCDF")
odf1 <- read.odf(f)
odf2 <- nameReplacement(odf1, data=data, unit="S/m")


test_that("removeDerived",
          {
            odf3 <- removeDerived(odf2)
            expect_equal(odf3@data$sea_water_sigma_theta_1, NULL)
            expect_equal(odf3@metadata$unit$sea_water_sigma_theta_1, NULL)

          }
)
