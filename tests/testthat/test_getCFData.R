## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(odfToNetCDF)

context("getCFData")

test_that("data frame components, ctd",
          {
            data <- getCFData(type="ctd")
            expect_equal(names(data), c("code", "name", "units", "standard_name", "type"))
            expect_equal(unique(data$type), "ctd")
          }
)

test_that("data frame components, rcm",
          {
            data <- getCFData(type="rcm")
            expect_equal(names(data), c("code", "name", "units", "standard_name", "type"))
            expect_equal(unique(data$type), "rcm")
          }
)

test_that("data frame components, adcp",
          {
            data <- getCFData(type="adcp")
            expect_equal(names(data), c("code", "name", "units", "standard_name", "type"))
            expect_equal(unique(data$type), "adcp")
          }
)

