## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(odfToNetCDF)

context("getStandardData")

test_that("data frame components, ctd",
          {
            data <- getStandardData(type="ctd")
            expect_equal(names(data), c("code", "name", "units", "standard_name", "type"))
            expect_equal(unique(data$type), "ctd")
            data2 <- getStandardData(type="ctd", standard="bodc")
            expect_equal(names(data2), c("code", "name", "units", "standard_name", "type", "bodc"))


          }
)

test_that("data frame components, rcm",
          {
            data <- getStandardData(type="rcm")
            expect_equal(names(data), c("code", "name", "units", "standard_name", "type"))
            expect_equal(unique(data$type), "rcm")
            expect_equal(data$standard_name[which(data$code == "HCSP")], "horizontal_current_speed")
          }
)

test_that("data frame components, adcp",
          {
            data <- getStandardData(type="adcp")
            expect_equal(names(data), c("code", "name", "units", "standard_name", "type"))
            expect_equal(unique(data$type), "adcp")
          }
)

