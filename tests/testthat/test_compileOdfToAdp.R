## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(odfToNetCDF)

context("fixMetadata")

library(odfToNetCDF)
library(oce)
data <- getCFData(type="adcp")
f1 <- system.file("extdata", "adcp1.ODF", package="odfToNetCDF")
f2 <- system.file("extdata", "adcp2.ODF", package="odfToNetCDF")
files <- c(f1,f2)
adp <- compileOdfToAdp(files)



test_that("compileOdfToAdp",
          {
            adp <- compileOdfToAdp(files)
            expect_equal(dim(adp[["v"]]), c(4, 2, 4))
          }
)

