context("noaa-weather-scraper")


test_that("noaa-weather-scraper works as expected", {
  expect_message(
    noaa_api_data(location = NULL, var = "TMAX", 
                  date_min = Sys.Date(), quiet = F), 
    "Defaulting to Ames, IA as location")
  expect_error(noaa_api_data(location = "Ames"), 
               'location is not a data frame')
  expect_error(noaa_api_data(location = data.frame("a" = "A")), 
               "location does not have name id")
  expect_error(noaa_api_data(data.frame(id = "Ames", a = "A", b = "B")), 
               "location does not have name latitude")
  expect_error(noaa_api_data(data.frame(id = "Ames", latitude = 42.034722)), 
               "location does not have name longitude")
  expect_error(noaa_api_data(data.frame(id = "Ames", latitude = 42.034722)), 
               "location does not have name longitude")
  expect_error(noaa_api_data(var = NULL), "!is.null.var. is not TRUE")
  expect_error(noaa_api_data(date_min = NA))
  expect_error(noaa_api_data(date_max = NA))
  
  tmp <- noaa_api_data(date_min = "2018-01-01", date_max = "2018-01-01")
  expect_equal(tmp$tmax, -199)
  expect_equal(tmp$tmin, -299)
  expect_equal(tmp$Accessed, Sys.Date())
})
