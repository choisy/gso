# Once this script is sourced, the outputed CSV file should be converted to
# excel and edited manually.
get_tables_names <- function(url, remDr) {
  # "url" is a vector of URLs
  # "remDr" is a remote driver
  require(RSelenium) # for "$navigate()", "$findElements()",
                     # "$getElementAttribute()", "$getElementText()",
  # sets up the url:
  remDr$navigate(url)

  # selects the right pane:
  webElems <- remDr$findElements(using = "xpath", "//iframe")
  remDr$navigate(unlist(webElems[[1]]$getElementAttribute("src")))

  # retrieving the names of the tables:
  webElems <- remDr$findElements(using = "xpath", "//li//a")
  the_texts <- unlist(lapply(webElems, function(x)x$getElementText()))
  gsub("/", "-", the_texts)
}

################################################################################

library(RSelenium) # for "rsDriver", "$open()", "$close()", "$stop()"
library(dplyr) # for "bind_rows" and the " %>% " pipe operator

# starting a Selenium server and the chrome browser:
if(TRUE) {
  message("Connecting to remote server...")
  rD <- rsDriver(browser = "chrome", verbose = FALSE) # headless mode using phantomJS via the
  driver <- rD[["client"]]                            # Selenium server doesn't support file download
  driver$setImplicitWaitTimeout(milliseconds = 60000)
}

# getting the directories names and URLs:
driver$navigate("https://www.gso.gov.vn/Default_en.aspx?tabid=766")
webElems <- driver$findElements(using = "xpath", "//div[@class = 'text_blue3']/a")
dir_names <- unlist(lapply(webElems, function(x) x$getElementText()))
urls <- unlist(lapply(webElems, function(x) x$getElementAttribute("href")))
pattern <- "^\\d\\d\\. "
sel <- grep(pattern, dir_names)
urls <- urls[sel]
dir_names <- dir_names[sel]
dir_names <- gsub(pattern, "", dir_names)
names(dir_names) <- urls

# setting the working directory:
path0 <- getwd()
if(sub(".*/", "", path0) == "gso") setwd("data-raw")

# getting the tables names for each of the categories:
data_frames_summary <- urls %>%
  lapply(function(x) as.data.frame(cbind(dir_names[x],
                                         get_tables_names(x, driver)),
                                   stringsAsFactors = FALSE)) %>%
  bind_rows %>%
  setNames(c("category", "dataframe")) %>%
  write.csv2("data_frame_summary.xls")

# back to the initial directory:
setwd(path0)

# stops the selenium server:
rD[["server"]]$stop()

# After eidting with excel, transform the excel file in data frame (RData?)
library(readxl) # for "read_excel"
library(magrittr) # for " %>% "
data_frame_summary <-
  read_excel("data-raw/data_frame_summary.xls") %>%
  as.data.frame()
devtools::use_data(data_frame_summary, overwrite = TRUE)

