standardMetadata <- function(x) # oce
{

    if (!inherits(x, "oce")) {
        stop("x must be an oce object")
    }

    namesMeta <- names(x[['metadata']])

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



    for (i in seq_along(needNames)) {
        if (needNames[[i]] == 'Conventions') {
            # USER
        } else if (needNames[[i]] == "date_created") {
            oceSetMetadata(x, name="date_created", value=Sys.Date())
        } else if (needNames[[i]] == "source") {
            oceSetMetadata(x, name = "source", value=x[['filename']])
        } else if (needNames[[i]] == "creator_type") {
            oceSetMetadata(x, name = "creator_type", value="person")
        } else if (needNames[[i]] == "creator_name") {
            # USER
        } else if (needNames[[i]] == "creator_country") {
            # USER
        } else if (needNames[[i]] == "creator_email") {
          #USER
        } else if (needNames[[i]] == "creator_institution") {
          # USER
        } else if (needNames[[i]] == "creator_address") {
          # USER
        } else if (needNames[[i]] == "creator_city") {
          #USER
        } else if (needNames[[i]] == "creator_sector") {
          # USER

        } else if (needNames[[i]] == "creator_url") {
          #USER
        } else if (needNames[[i]] == "featureType") {
          oceSetMetadata(x, name = "featureType", value="timeSeries")

        } else if (needNames[[i]] == "id") {
          oceSetMetadata(x, name = "id", value=x[['filename']])
        } else if (needNames[[i]] == "naming_authority") { #FIXME
          oceSetMetadata(x, name = "naming_authority", value="ca.gc.bio")

        } else if (needNames[[i]] == "infoUrl") { #FIXME
          oceSetMetadata(x, name = "infoUrl", value="https://www.dfo-mpo.gc.ca/science/data-donnees/azmp-pmza/index-eng.html")


        } else if (needNames[[i]] == "license") { #FIXME
          oceSetMetadata(x, name = "license", value="https://open.canada.ca/en/open-government-licence-canada")


        } else if (needNames[[i]] == "summary") { #FIXME
          # Make cheecks here # JAIM FIXTHIS


        } else if (needNames[[i]] == "title") { #FIXME
          # Make checks this is JAIM


        } else if (needNames[[i]] == "project") { #FIXME
          # USER

        } else if (needNames[[i]] == "keywords") {

          # USER
        } else if (needNames[[i]] == "platform") {
          # USER (example mooring)
        } else if (needNames[[i]] == "platform_name") {
          oceSetMetadata(x, name="platform_name", value=x[['station']]) #mooring number
        } else if (needNames[[i]] == "platform_id") {
          oceSetMetadata(x, name="platform_name", value=x[['station']]) #mooring number

        } else if (needNames[[i]] == "platform_vocabulary") {
          oceSetMetadata(x, name="platform_vocabulary", "https://vocab.nerc.ac.uk/collection/L06/current/") #FIXME
        } else if (needNames[[i]] == "deployment_platform_name") {
          # User (ship name)
          # Could check if there is an odf
        } else if (needNames[[i]] == "instrument") {
          # Check for things (user could add it in)
        } else if (needNames[[i]] == "instrument_vocabulary") { #
          oceSetMetadata(x, name="instrument_vocabulary", value="http://vocab.nerc.ac.uk/collection/L22/current/") #FIXME

        } else if (needNames[[i]] == "time_coverage_resolution") {
          # FIXME
        } else if (needNames[[i]] == "time_coverage_duration") {
          #FIXME
        } else if (needNames[[i]] == "time_coverage_start") {
          oceSetMetadata(x, name="time_coverage_start", value=x[['time']][1])
        } else if (needNames[[i]] == "time_coverage_end") {
          oceSetMetadata(x, name="time_coverage_start", value=x[['time']][length(x[['time']])])

        } else if (needNames[[i]] == "geospatial_lat_min") {
          oceSetMetadata(x, name="geospatial_lat_min", value=min(x[['latitude']]))
        } else if (needNames[[i]] == "geospatial_lat_max") {
          oceSetMetadata(x, name="geospatial_lat_max", value=max(x[['latitude']]))
        } else if (needNames[[i]] == "geospatial_lat_units") {
          oceSetMetadata(x, name="geospatial_lat_units", value="degrees_north")

        } else if (needNames[[i]] == "geospatial_lon_min") {
          oceSetMetadata(x, name="geospatial_lon_min", value=min(x[['longitude']]))
        } else if (needNames[[i]] == "geospatial_lon_max") {
          oceSetMetadata(x, name="geospatial_lon_max", value=max(x[['longitude']]))
        } else if (needNames[[i]] == "geospatial_lon_units") {
          oceSetMetadata(x, name="geospatial_lon_units", value="degrees_east")
        } else if (needNames[[i]] == "geospatial_vertical_max") {
          oceSetMetadata(x, name="geospatial_vertical_max", value=max(x[['pressure']]))
        } else if (needNames[[i]] == "geospatial_vertical_min") {
          oceSetMetadata(x, name="geospatial_vertical_min", value=min(x[['pressure']]))
        } else if (needNames[[i]] == "geospatial_vertical_max") {
          oceSetMetadata(x, name="geospatial_vertical_max", value=max(x[['pressure']]))

        } else if (needNames[[i]] == "geospatial_vertical_units") {
          oceSetMetadata(x, name="geospatial_vertical_units", value=x[['units']][['pressure']][['unit']])


        } else if (needNames[[i]] == "geospatial_veterical_positive") {
          oceSetMetadata(x, name="geospatial_vertical_positive", value="down")
        } else if (needNames[[i]] == "FillValue") {
          oceSetMetadata(x, name="FillValue", value=1e35)
        } else if (needNames[[i]] == "date_modified") {
          oceSetMetadata(x, name="date_modified", value=date())
        } else if (needNames[[i]] == "standard_name_vocabulary") {
          oceSetMetadata(x, name="standard_name_vocabulary", value= "https://vocab.nerc.ac.uk/search_nvs/P01/")
        } else if (needNames[[i]] == "history") {
          # FIXME
        }

    }

}

# Make a test that when the adp is filling it in, if it doesn't exist ask the user

# Make another check if the answers are the same in a for loop, just fill it in for all of them

#institution,


}
