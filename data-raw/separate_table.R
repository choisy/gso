# packages neeeded
library(magrittr)
library(dplyr)
library(tidyr)

# Prerequisite -----------------------------------------------------------------
data("data_frame_summary")
provinces <- readRDS("data-raw/province.RDS") # dico provinces

# Function ---------------------------------------------------------------------
# tidy province
tidy_province <- function(df){
  df[, 1] <- df[, 1] %>%
    unlist  %>%
    as.vector  %>%
    gsub("city", "", .) %>%
    gsub("  ", " ", .) %>%
    provinces[.]
  names(df)[1] <- "province"
  df %>% filter(is.na(province) == FALSE)
}

# Separate table with multiple sections
separate_df <- function(df){
  # first table with the province dataset
  province <- df[which(df[, 1] %>%
      gsub("city", "", .) %>%
      gsub("  ", " ", .)  %>%
      provinces[.]  %>%
      is.element(provinces %>% unique)), ] %>%
    tidy_province()
  # second table
  other <- df[which(df[, 1] %in% province[, 1] == FALSE),]
  sel <- grep("Class|Pupils", other[, 1] %>% unlist %>% as.vector)
  other <- other[sel, ]
  # total
  total <- list(province = province, other = other)
}

# Data creation ----------------------------------------------------------------
# first case
sel_market <- "data-raw/Trade, Price and Tourism/Number of markets as of annual December 31st by class and by province.csv"
nb_market <- read.csv(sel_market,header = TRUE, skip = 2, sep = ";",
                      na.strings = "..") %>%
  separate_df()
name_one_market <- sel_market %>% gsub("by class and ", "", .)
name_two_market <- sel_market %>% gsub(" and by province", "", .)

write.csv(nb_market[[1]], file = name_one_market)
write.csv(nb_market[[2]], file = name_two_market)
file.remove(sel_market)

# second case
sel_pupils <- "data-raw/Education/Number of pupils of general education as of 30 September by province.csv"
pupils <- read.csv(sel_pupils,
                 header = TRUE, skip = 2, sep = ";", na.strings = "..") %>%
  separate_df()
name_one_pupils <- sel_pupils
name_two_pupils <- sel_pupils %>% gsub(" by province", " in university", .)

write.csv(pupils[[1]], file = name_one_pupils)
write.csv(pupils[[2]], file = name_two_pupils)




