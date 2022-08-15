#' Divide data frame based on GF3 standard
#'
#' Use this function to map gf3 codes to P01 codes for exporting to netCDF
#'
#' @param gf3 a gf3 standard code paramater
#'
#' @return a matching CF value with units and standard name (if applicable)
#' @export
#'
#' @examples
dataFrameDivide <- function(gf3){
  line <- grep(DF$code, pattern = gf3, ignore.case = T)

  if (length(line) == 0){
    yn <- list()
    for (i in 1:length(DF$code)){
      yn[[i]] <- grep( pattern = DF$code[[i]], x = gf3, value = TRUE)
      if (length(yn[[i]] != 0)) {
        line <- i
      }
    }
  }
  if (length(line) == 0){
    warning(paste(gf3, 'not recognized in list of GF3 codes!'))
    stop()
  }

  gf3 <- list(gf3 = gf3)
  gf3$units <- as.character(DF$units[[line]])
   gf3$std <- as.character(DF$standard_name[[line]])
     if (gf3$std == ""){
       gf3$std <- NULL
     }

  return(gf3)
}

