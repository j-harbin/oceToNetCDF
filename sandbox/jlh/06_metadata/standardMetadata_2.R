#' Insert Metadata names to be compliant with IOOS
#'
#' This function inserts the required global attributes that are
#' "highly recommended" for the IOOS (Integrated Ocean Observing
#' System) standards (see Reference 1). This function works by
#' extracting what it can from the data and metadata within the
#' `oce` object, but if it required field is determined to be NULL
#' the user will be prompted to enter in the required information.
#' If `fixed=TRUE`, the information that is input by the user will
#' be saved and applied to a list of `oce` objects.
#'
#' If `fixed = TRUE` and a data frame named "metadataAnswers" doesn't exist
#' it will save the user's answers into a data frame named "metadataAnswers"
#' which contains the name of the field that required a users answers as well
#' as the answers the user input. This means, if `fixed=TRUE` and "metadataAnswers"
#' does exist, this function will load up the previously answered questions
#' to avoid the user needing to answer them over and over again.
#'
#' @param x an [oce-class] object containing bottom ranges.
#'
#' @param fixed a Boolean indicating if the answers input by the user
#' should be saved as a data frame and applied to a list of `oce` objects
#' See Details.
#'
#' @importFrom oce oceSetMetadata
#'
#' @return `an [oce-class] object containing highly reccommended IOOS metadata
#'
#' @references
#' 1. https://wiki.esipfed.org/Attribute_Convention_for_Data_Discovery_1-3#Global_Attributes
#'
#' @author Jaimie Harbin
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(ctd)
#' ctdMeta <- standardMetadata(ctd)
#' }
#'
#' @export

standardMetadata <- function(x, fixed=FALSE) # oce
{

  if (!inherits(x, "oce")) {
    stop("x must be an oce object")
  }

  if (fixed) {
    if (file.exists("metadataAnswers")) {
      load("metadataAnswers")
      # Just load up the answers and don't ask the questions
      for (i in seq_along(d$namesFixed)) {
        x <- oceSetMetadata(x, name=d$namesFixed[[i]], value=d$fixedAnswers[[i]])
      }
    }
  }

  needNames <- c("Conventions", "date_created", "institution", "source", "creator_type", "creator_name",
                 "creator_country", "creator_email", "creator_institution", "creator_address", "creator_city",
                 "creator_sector", "creator_url", "featureType", "creator_url", "featureType", "id",
                 "naming_authority", "infoUrl", "license", "summary", "title", "project", "keywords", "platform",
                 "platform_name", "platform_id", "platform_vocabulary", "deployment_platform_name", "deployment_platform_vocabulary",
                 "instrument", "instrument_vocabulary", "time_coverage_resolution", "time_coverage_duration", "time_coverage_start",
                 "time_coverage_end", "geospatial_lat_min", "geospation_lat_max", "geospatial_lat_units", "geospatial_lon_min",
                 "geospatial_lon_max", "geospatial_lat_units", "geospatial_lon_min", "geospatial_lon_max",
                 "geospatial_lon_units", "geospatial_vertical_max", "geospatial_vertical_min", "geospatial_vertical_units",
                 "geospatial_vertical_positive", "FillValue","date_modified", "standard_name_vocabulary", "history")

  fixedAnswers <- NULL
  namesFixed <- NULL

  for (i in seq_along(needNames)) {
    if (needNames[[i]] == 'Conventions') {
      if (is.null(x[["Conventions"]])) {
        fun <- function(){
          ans <- readline("Enter your value for Conventions (eg. CF-1.6, ACDD-1.3, IOOS-1.2) ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name="Conventions", value=answer)
      }
    } else if (needNames[[i]] == "date_created") {
      x <- oceSetMetadata(x, name="date_created", value=Sys.Date())
    } else if (needNames[[i]] == "source") {
      x <- oceSetMetadata(x, name = "source", value=x[['filename']])
    } else if (needNames[[i]] == "creator_type") {
      x <- oceSetMetadata(x, name = "creator_type", value="person")
    } else if (needNames[[i]] == "creator_name") {
      if (is.null(x[['creator_name']])) {
        fun <- function(){
          ans <- readline("Enter your creator_name (eg. John Smith) ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name="creator_name", value=answer)
      }
    } else if (needNames[[i]] == "creator_country") {
      if (is.null(x[['creator_country']])) {
        fun <- function(){
          ans <- readline("Enter your creator_country (eg. Canada) ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name="creator_country", value=answer)
      }
    } else if (needNames[[i]] == "creator_email") {
      if (is.null(x[['creator_email']])) {
        fun <- function(){
          ans <- readline("Enter your creator_email (eg. john.smith@example.com) ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name="creator_email", value=answer)
      }
    } else if (needNames[[i]] == "creator_institution") {
      if (is.null(x[['creator_institution']])) {
        fun <- function(){
          ans <- readline("Enter your creator_institution (eg. Bedford Institute of Oceanography) ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name="creator_institution", value=answer)
      }
    } else if (needNames[[i]] == "creator_address") {
      if (is.null(x[['creator_address']])) {
        fun <- function(){
          ans <- readline("Enter your creator_address (eg. 123 Example Drive) ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name="creator_address", value=answer)
      }
    } else if (needNames[[i]] == "creator_city") {
      if (is.null(x[['creator_city']])) {
        fun <- function(){
          ans <- readline("Enter your creator_city (eg. Halifax) ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name="creator_city", value=answer)
      }
    } else if (needNames[[i]] == "creator_sector") {
      if (is.null(x[['creator_sector']])) {
        fun <- function(){
          ans <- readline("Enter your creator_sector (eg. Science) ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name="creator_sector", value=answer)
      }

    } else if (needNames[[i]] == "creator_url") {
      if (is.null(x[['creator_url']])) {
        fun <- function(){
          ans <- readline("Enter your creator_url (eg. www.bio.gc.ca/index-en.php) ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name="creator_url", value=answer)
      }
    } else if (needNames[[i]] == "featureType") {
      x <- oceSetMetadata(x, name = "featureType", value="timeSeries")

    } else if (needNames[[i]] == "id") {
      if (!(is.null(x[['filename']]))) {
        x <- oceSetMetadata(x, name = "id", value=x[['filename']])
      } else {
        if (is.null(x[['id']])) {
          fun <- function(){
            ans <- readline("Enter your id (eg. MCAT1234) ")
            ans <- unlist(strsplit(ans, ","))
            out1 <- ans
            return(out1)
          }
          answer <- fun()
          fixedAnswers[[i]] <- answer
          namesFixed[[i]] <- needNames[[i]]
          x <- oceSetMetadata(x, name = "id", value=answer)
        }
      }
    } else if (needNames[[i]] == "naming_authority") {
      if (is.null(x[['naming_authority']])) {
        fun <- function(){
          ans <- readline("Enter your naming_authority (eg. 'ca.gc.bio') ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name = "naming_authority", value=answer)
      }
    } else if (needNames[[i]] == "infoUrl") {
      if (is.null(x[['infoUrl']])) {
        fun <- function(){
          ans <- readline("Enter your infoUrl (eg. 'https://www.dfo-mpo.gc.ca/science/data-donnees/azmp-pmza/index-eng.html') ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name = "infoUrl", value=answer)
      }

    } else if (needNames[[i]] == "license") {
      if (is.null(x[['license']])) {
        fun <- function(){
          ans <- readline("Enter your license (eg. 'https://open.canada.ca/en/open-government-licence-canada') ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name = "license", value=answer)
      }

    } else if (needNames[[i]] == "summary") { #FIXME
      # Make cheecks here # JAIM FIXTHIS


    } else if (needNames[[i]] == "title") { #FIXME
      # Make checks this is JAIM


    } else if (needNames[[i]] == "project") {
      if (is.null(x[['project']])) {
        fun <- function(){
          ans <- readline("Enter your project (eg. 'AZMP') ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name = "project", value=answer)
      }

    } else if (needNames[[i]] == "keywords") {
      if (is.null(x[['keywords']])) {
        fun <- function(){
          ans <- readline("Enter your keywords (eg. Argo) ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name="keywords", value=answer)
      }
    } else if (needNames[[i]] == "platform") {
      if (is.null(x[['platform']])) {
        fun <- function(){
          ans <- readline("Enter your platform (eg. mooring) ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name="platform", value=answer)
      }
    } else if (needNames[[i]] == "platform_name") {
      if (is.null(x[['station']])) {
        x <- oceSetMetadata(x, name="platform_name", value=x[['station']]) #mooring number
      } else {
        if (is.null(x[['platform_name']])) {
          fun <- function(){
            ans <- readline("Enter your platform_name (eg. mooring) ")
            ans <- unlist(strsplit(ans, ","))
            out1 <- ans
            return(out1)
          }
          answer <- fun()
          fixedAnswers[[i]] <- answer
          namesFixed[[i]] <- needNames[[i]]
          x <- oceSetMetadata(x, name="platform_name", value=answer)
        }

      }
    } else if (needNames[[i]] == "platform_id") {
      if (!(is.null(x[['station']]))) {
        x <- oceSetMetadata(x, name="platform_id", value=x[['station']]) #mooring number
      } else {
        if (is.null(x[['platform_id']])) {
          fun <- function(){
            ans <- readline("Enter your platform_id (eg. BATS)")
            ans <- unlist(strsplit(ans, ","))
            out1 <- ans
            return(out1)
          }
          answer <- fun()
          fixedAnswers[[i]] <- answer
          namesFixed[[i]] <- needNames[[i]]
          x <- oceSetMetadata(x, name="platform_id", value=answer)
        }
      }

    } else if (needNames[[i]] == "platform_vocabulary") {
      x <- oceSetMetadata(x, name="platform_vocabulary", "https://vocab.nerc.ac.uk/collection/L06/current/")
    } else if (needNames[[i]] == "deployment_platform_name") {
      if (is.null(x[['deployment_platform_name']])) {
        fun <- function(){
          ans <- readline("Enter your deployment_platform_name (eg. Hudson) ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name="deployment_platform_name", value=answer)
      }

    } else if (needNames[[i]] == "instrument") {
      # Check for things (user could add it in) FIXME
    } else if (needNames[[i]] == "instrument_vocabulary") {
      if (is.null(x[['instrument_vocabulary']])) {
        fun <- function(){
          ans <- readline("Enter your instrument_vocabulary (eg. 'http://vocab.nerc.ac.uk/collection/L22/current/') ")
          ans <- unlist(strsplit(ans, ","))
          out1 <- ans
          return(out1)
        }
        answer <- fun()
        fixedAnswers[[i]] <- answer
        namesFixed[[i]] <- needNames[[i]]
        x <- oceSetMetadata(x, name="instrument_vocabulary", value=answer)
      }

    } else if (needNames[[i]] == "time_coverage_resolution") {
      # FIXME
    } else if (needNames[[i]] == "time_coverage_duration") {
      #FIXME
    } else if (needNames[[i]] == "time_coverage_start") {
      x <- oceSetMetadata(x, name="time_coverage_start", value=x[['time']][1])
    } else if (needNames[[i]] == "time_coverage_end") {
      x <- oceSetMetadata(x, name="time_coverage_end", value=x[['time']][length(x[['time']])])

    } else if (needNames[[i]] == "geospatial_lat_min") {
      x <- oceSetMetadata(x, name="geospatial_lat_min", value=min(x[['latitude']]))
    } else if (needNames[[i]] == "geospatial_lat_max") {
      x <- oceSetMetadata(x, name="geospatial_lat_max", value=max(x[['latitude']]))
    } else if (needNames[[i]] == "geospatial_lat_units") {
      x <- oceSetMetadata(x, name="geospatial_lat_units", value="degrees_north")

    } else if (needNames[[i]] == "geospatial_lon_min") {
      x <- oceSetMetadata(x, name="geospatial_lon_min", value=min(x[['longitude']]))
    } else if (needNames[[i]] == "geospatial_lon_max") {
      x <- oceSetMetadata(x, name="geospatial_lon_max", value=max(x[['longitude']]))
    } else if (needNames[[i]] == "geospatial_lon_units") {
      x <- oceSetMetadata(x, name="geospatial_lon_units", value="degrees_east")
    } else if (needNames[[i]] == "geospatial_vertical_max") {
      x <- oceSetMetadata(x, name="geospatial_vertical_max", value=max(x[['pressure']]))
    } else if (needNames[[i]] == "geospatial_vertical_min") {
      x <- oceSetMetadata(x, name="geospatial_vertical_min", value=min(x[['pressure']]))
    } else if (needNames[[i]] == "geospatial_vertical_max") {
      x <- oceSetMetadata(x, name="geospatial_vertical_max", value=max(x[['pressure']]))

    } else if (needNames[[i]] == "geospatial_vertical_units") {
      x <- oceSetMetadata(x, name="geospatial_vertical_units", value=x[['units']][['pressure']][['unit']])
    } else if (needNames[[i]] == "geospatial_veterical_positive") {
      x <- oceSetMetadata(x, name="geospatial_vertical_positive", value="down")
    } else if (needNames[[i]] == "FillValue") {
      x <- oceSetMetadata(x, name="FillValue", value=1e35)
    } else if (needNames[[i]] == "date_modified") {
      x <- oceSetMetadata(x, name="date_modified", value=date())
    } else if (needNames[[i]] == "standard_name_vocabulary") {
      x <- oceSetMetadata(x, name="standard_name_vocabulary", value= "https://vocab.nerc.ac.uk/search_nvs/P01/")
    } else if (needNames[[i]] == "history") {
      # FIXME
    }

  }
  if (fixed) {
    if (!(file.exists("metadataAnswers"))) {
      # This means we already have the saved data
      d <- data.frame("fixedAnswers"=unlist(fixedAnswers), "namesFixed"=unlist(namesFixed))
      save(d, file="metadataAnswers")
    }
  }

  return(x)

}
