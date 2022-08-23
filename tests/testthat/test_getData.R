## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(odfToNetCDF)

context("getData")

test_that("data frame components, ctd",
          {
            data <- getData(type="ctd")
            expect_equal(names(data), c("code", "name", "units", "standard_name", "type"))
            expect_equal(unique(data$type), "ctd")
          }
)

test_that("data frame components, rcm",
          {
            data <- getData(type="rcm")
            expect_equal(names(data), c("code", "name", "units", "standard_name", "type"))
            expect_equal(unique(data$type), "rcm")
          }
)

test_that("data frame components, adcp",
          {
            data <- getData(type="adcp")
            expect_equal(names(data), c("code", "name", "units", "standard_name", "type"))
            expect_equal(unique(data$type), "adcp")
          }
)

test_that("data frame errors",
          {
            expect_error(getData(type="dog"), "getData can only work for data type ctd, rcm, or adcp")
          }
)
