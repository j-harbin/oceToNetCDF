## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(oceToNetCDF)

context("fixMetadata")

library(oceToNetCDF)
library(oce)
data <- getStandardData(type="adcp")
f1 <- system.file("extdata", "adcp1.ODF", package="oceToNetCDF")
f2 <- system.file("extdata", "adcp2.ODF", package="oceToNetCDF")
files <- c(f1,f2)
adp <- compileOdfToAdp(files)



test_that("compileOdfToAdp",
          {
            adp <- compileOdfToAdp(files)
            expect_equal(dim(adp[["v"]]), c(4, 2, 4))
          }
)

