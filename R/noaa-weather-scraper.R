#' Get data from NOAA weather API for Ames, IA
#' 
#' @param lat_long_df data frame with columns id, latitude, and longitude
#' @param var variables to download (TMAX, TMIN by default, see rnoaa 
#'            documentation for other options)
#' @param date_min ymd format, minimum date
#' @param date_max ymd format, maximum date
#' @param quiet print messages?
#' @import assertthat
#' @import rnoaa
#' @importFrom lubridate today
#' @importFrom magrittr '%>%'
#' @export
noaa_api_data <- function(location = NULL, var = c("TMAX", "TMIN"),
                          date_min = "2008-01-01", date_max = Sys.Date(),
                          quiet = T) {
  if (is.null(location)) {
    if (!quiet) message("Defaulting to Ames, IA as location")
    location <- data.frame(id = "ames", 
                           latitude = 42.034722, 
                           longitude = -93.62)
  }
  
  assert_that(is.data.frame(location),
              has_name(location, "id"),
              has_name(location, "latitude"),
              has_name(location, "longitude"),
              is.numeric(location$latitude),
              is.numeric(location$longitude),
              abs(location$latitude) <= 90,
              abs(location$longitude) <= 360)
  
  assert_that(!is.null(var))
  assert_that(!is.na(as.Date(date_min)))
  assert_that(!is.na(as.Date(date_max)))
  
  nearby_stations <- rnoaa::meteo_nearby_stations(lat_lon_df = location, 
                                                  limit = 3, var = var)
  
  ids <- sapply(nearby_stations, function(x) x$id)
  
  rnoaa::meteo_pull_monitors(
    ids, 
    var = var, 
    date_min = date_min, 
    date_max = date_max) %>%
    mutate(
      Accessed = lubridate::today()
    )
}
