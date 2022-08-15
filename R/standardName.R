#' Obtain standard name and units from GF3 code
#'
#' This function reveals the standard name and units in climate and
#' forecast (CF) standards from a General Formatting (GF3) standard
#'code parameter.
#'
#' @param gf3 a character indicating a GF3 (General Formatting) standard code parameter
#'
#' @return A list containing the standard name and unit of the GF3 code in CF standard
#' @export
#'
#' @examples
standardName <- function(gf3) {
  line <- grep(DF$code, pattern = gf3, ignore.case = TRUE)

  if (length(line) == 0) {
    yn <- list()
    for (i in 1:length(DF$code)) {
      yn[[i]] <- grep( pattern = DF$code[[i]], x = gf3, value = TRUE)
      if(length(yn[[i]] != 0)) {
        line <- i
      }
    }

  }
  if (length(line) == 0) {
    stop(gf3, " is not recognized in the codes.")
  }

  gf3 <- list(gf3 = gf3)
  gf3$standard_name <- as.character(DF$standard_name[[line]])
  gf3$units <- as.character(DF$units[[line]])


  return(gf3)
}
