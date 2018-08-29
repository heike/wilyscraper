#' Function to get weather data from weather underground
#'
#' @param remDr a selenium webdriver client
#' @param station station call sign
#' @param year year (4 digit number)
#' @param month month (1 or 2 digit number)
#' @param day day (1 or 2 digit number)
#' @return table of weather data, with date, time, variable, value, and unit
#' @import rvest
#' @importFrom stringr str_extract str_trim str_replace_all
#' @importFrom dplyr '%>%'
#' @importFrom tidyr gather
wUnderground_day <- function(remDr, station = "KAMW", year = 2015, month = 1, day = 1) {
  numbers <- "[-]*[0-9\\.]*[e0-9]*"
  units <- "[°a-zA-Z\\s\\\\]+"

  url <-
    sprintf(
      "https://www.wunderground.com/history/airport/%s/%04d/%02d/%02d/DailyHistory.html",
      station, year, month, day
    )

  remDr$open()
  on.exit(remDr$close())
  # Establish a wait for an element
  remDr$setImplicitWaitTimeout(1000)
  remDr$navigate(url)
  i <- 0
  tabs <- NULL
  while (i < 3 & length(tabs) == 0) {
    Sys.sleep(2)
    html <- remDr$getPageSource()[[1]] %>% read_html()
    tabs <- html %>% html_nodes("#history-observation-table")
  }
  if (length(tabs) > 0) {
    day.stats <- html_table(tabs[[length(tabs)]], fill = TRUE)

    day.stats.long <- day.stats %>% tidyr::gather(key = Var, value = Value, -Time)
    if (is.null(day.stats.long$Value)) return(NULL) # no data available

    day.stats.long$Unit <- str_extract(day.stats.long$Value, pattern = units) %>%
      str_replace_all("\\s{1,}", " ") %>%
      str_trim()
    day.stats.long$Value <- as.numeric(str_extract(day.stats.long$Value, pattern = numbers))
    day.stats.long$Station <- station
    day.stats.long$Date <- sprintf("%s-%s-%s", year, month, day)

    return(day.stats.long)
  } else {
    return(NULL)
  }
}

#' Function to get all weather data from wunderground for a period of time
#'
#' @param start start date
#' @param end end date
#' @param station weather station
#' @param file file to save the CSV data file
#' @import RSelenium
#' @importFrom lubridate today ymd year month mday
#' @importFrom dplyr '%>%'
#' @export
wUnderground_data <- function(start = lubridate::ymd("2011-01-01"),
                              end = lubridate::today(),
                              station = "KAMW") {
  days <- seq(start, end, "day")

  # crx <- tempfile(fileext = ".zip")
  # crxfolder <- tempdir()
  # download.file("https://github.com/gorhill/uBlock/releases/download/1.16.21b1/uBlock0.chromium.zip",
  #               destfile = crx)
  # unzip(crx, exdir = crxfolder)
  # list.files(file.path(crxfolder), full.names = T)
  # Start selenium driver
  rD <- RSelenium::rsDriver(browser = "chrome", version = "3.13.0")
  remDr <- rD[["client"]]

  res <- purrr::map_dfr(
    days,
    ~try(
      wUnderground_day(
        remDr = remDr, station = station,
        lubridate::year(.), lubridate::month(.), lubridate::mday(.)
      )
    )
  )

  # write.table(res,
  #   file = file, append = file.exists(file),
  #   col.names = !file.exists(file),
  #   row.names = F, sep = ","
  # )
  
  return(res)
}
