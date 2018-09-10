# packages ---------------------------------------------------------------------
library(magrittr)  # for ' %>% ', ' %<>% '
library(dplyr)  # for 'filter'
library(dictionary)  # for 'vn_province'

# Prerequisite -----------------------------------------------------------------
provinces <- dictionary::vn_province

# Function ---------------------------------------------------------------------
# Translate the province names that are first column of a data frame and rename
# the column "province". Keep only the row with a province name.
tidy_province <- function(df) {
    df[, 1] <- df[, 1] %>% unlist %>% as.vector %>%
      gsub("city", "", .) %>%
      gsub("  ", " ", .) %>%
      stringi::stri_escape_unicode(.) %>%
        provinces[.]
    names(df)[1] <- "province"
    df %>% filter(is.na(province) == FALSE)
}

# Separate table with multiple spatial definition: provinces and others (sep)
separate_df <- function(df, sep) {
    # filter the data frame containing the province definition
    province <- df[which(df[, 1] %>%
                           gsub("city", "", .) %>%
                           gsub("  ", " ", .) %>%
                           stringi::stri_escape_unicode(.) %>%
                           provinces[.] %>%
                           is.element(provinces %>% unique)), ] %>%
      tidy_province()
    # second table containing the others spatial definitions
    other <- df[which(df[, 1] %in% province[, 1] == FALSE), ]
    sel <- grep(sep, other[, 1] %>% unlist %>% as.vector, ignore.case = TRUE)
    other <- other[sel, ]
    names(other)[1] <- sep
    # total: return a list containing the two data frames
    total <- list(province = province, other = other)
}

# Data creation ----------------------------------------------------------------

# first case -------------------------------------------------------------------
sel_market <-
  "Number of markets as of annual December 31st by class and by province"

nb_market <- read.csv(paste0("Trade, Price and Tourism/", sel_market, ".csv"),
                      header = TRUE, skip = 2, sep = ";", na.strings = "..") %>%
  separate_df(sep = "class")
name_one_market <- sel_market %>% gsub("by class and ", "", .)
name_two_market <- sel_market %>% gsub(" and by province", "", .)

# register new files and update data_frame_summary
write.csv2(nb_market[[1]],
           file = paste0("Trade, Price and Tourism/", name_one_market, ".csv"))
write.csv2(nb_market[[2]],
           file = paste0("Trade, Price and Tourism/", name_two_market, ".csv"))
file.remove(paste0("Trade, Price and Tourism/", sel_market, ".csv"))

# second case ------------------------------------------------------------------
sel_pupils <-
  "Number of pupils of general education as of 30 September by province"

pupils <- read.csv(paste0("Education/", sel_pupils, ".csv"),
                   header = TRUE, skip = 2, sep = ";", na.strings = "..") %>%
    separate_df(sep = "pupils")

name_one_pupils <- paste0("Education/", sel_pupils, ".csv")
name_two_pupils <- paste0("Education/", sel_pupils, ".csv") %>%
  gsub(" by province", " in university", .)

write.csv2(pupils[[1]], file = name_one_pupils)
write.csv2(pupils[[2]], file = name_two_pupils)

# erase everything --------------------------------------------------------------
rm(list = ls())
