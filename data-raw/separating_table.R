# packages ---------------------------------------------------------------------
library(magrittr)  # for ' %>% ', ' %<>% '
library(dplyr)  # for 'filter', 'select', 'add_row'
library(dictionary)  # for 'vn_province'
setwd("~/Desktop/gso")

# Prerequisite -----------------------------------------------------------------
provinces <- dictionary::vn_province

regions <- c("red_river_delta", "northern_midlands_and_mountain_areas",
              "north_central_and_central_coastal_areas",
              "north_central_area_and_central_coastal_area",
              "central_highlands", "south_east", "mekong_river_delta",
              "whole_country")

residences <- c("urban", "rural")

# Function ---------------------------------------------------------------------

# Create a vector of multiple file names from one file name, separated by the
# different spatial definition.
create_filename <- function(filename) {
  filename %>% strsplit(" by ") %>%
    unlist %>% paste0(. [1], " by ", ., ".csv") %>%
    gsub("\\,\\.| and\\.", "\\.", .) %>% gsub("\\.csv\\.csv", ".csv", .) %>%
    .[-1]
}

# Add a row at the beggining of the data (first row), containing the column name
add_colname_torow <- function(df) {
  cres <- matrix(colnames(df), nrow = 1) %>% data.frame() %>%
    mutate_all(as.character)
  colnames(cres) <- colnames(df)
  rbind(cres, df)
}

# Data creation ----------------------------------------------------------------

all_files <- dir("data-raw/") %>%
  grep(".R|.xls|.txt", ., invert = T, value = T) %>%
  lapply(., function(x) {
    file <- dir(paste0("data-raw/", x)) %>%
      paste0("data-raw/", x, "/", .)
    }) %>%
  unlist

cases <- grep(" and by", all_files, value = TRUE)

# Region cases -----------------------------------------------------------------

sel_region_residence <- grep("region", cases, value = TRUE) %>%
  grep("residence", ., value = TRUE)

df_lst <- lapply(seq_along(sel_region_residence), function(x){

  df <- read.csv(sel_region_residence[x], header = TRUE, skip = 2, sep = ";",
                 na.strings = "..")

  file_name <- create_filename(sel_region_residence[x])

  df[, 1] %<>% tolower %>%
    gsub("[[:punct:][:blank:]]+", "_", .) %>%
    gsub("resicence_|residence_", "", .)

  if (is_in("Region", colnames(df))) {
    df[, "Region"] %<>% tolower %>%
      gsub("[[:punct:][:blank:]]+", "_", .) %>%
      gsub("resicence_|residence_", "", .)
    sel <- names(df) %>% grep("Region", ., invert = TRUE)
    df %<>% select(Region, sel)
  }


  if (length(file_name) == 2) {
    tmp <- df %>% split(is.element(df[, 1], regions))
    df_region <- tmp$`TRUE`
    df_residence <- tmp$`FALSE`

    if (is.null(tmp$`FALSE`)) {
      df_list <- list(df_region %>% rbind(colnames(.), .)) %>%
        setNames(c(grep("region", file_name, value = TRUE)))
    } else {
      df_list <- list(df_region %>% rbind(colnames(.), .),
                      df_residence %>% rbind(colnames(.), .)) %>%
        setNames(c(
          grep("region", file_name, value = TRUE),
          grep("residence", file_name, value = TRUE)))
    }
  } else {
    tmp <- df %>% split(is.element(df[, 1], regions))
    df_region <- tmp$`TRUE`
    tmp <- tmp$`FALSE` %>% split(is.element(tmp$`FALSE`[, 1], residences))
    df_residence <- tmp$`TRUE`
    df_other <- tmp$`FALSE`

    if (is.null(tmp$`FALSE`)) {
      df_list <- list(df_region %>% rbind(colnames(.), .),
                      df_residence %>% rbind(colnames(.), .)) %>%
        setNames(c(
          grep("region", file_name, value = TRUE),
          grep("residence", file_name, value = TRUE)))
    } else {
      df_list <- list(df_region %>% rbind(colnames(.), .),
                      df_residence %>% rbind(colnames(.), .),
                      df_other %>% rbind(colnames(.), .)) %>%
        setNames(c(
          grep("region", file_name, value = TRUE),
          grep("residence", file_name, value = TRUE),
          grep(df_other[, 1] %>% gsub("[[:digit:]]", "", .) %>%
                 strsplit("_") %>% unlist %>% unique %>% .[. != ""] %>%
                 paste(collapse = "|"),
               file_name, value = TRUE)))
    }
  }
  df_list
})

df_lst %<>% flatten
for (i in seq_along(df_lst)) {
  write.csv2(df_lst[[i]], names(df_lst[i]), row.names = FALSE)
}
file.remove(sel_region_residence)


# Residence cases --------------------------------------------------------------

sel_residence <-  cases %>% grep("region", ., value = TRUE, invert = TRUE ) %>%
  grep("residence", ., value = TRUE)

df_lst <- lapply(seq_along(sel_residence), function(x) {

  df <- read.csv(sel_residence[x], header = TRUE, skip = 2, sep = ";",
               na.strings = "..")
  file_name <- create_filename(sel_residence[x])

  df[, 1] %<>% tolower %>% gsub("[[:punct:][:blank:]]+", "_", .)

  if (is_in("Urban", colnames(df))) {
    df_residence <- df %>%
      select(-matches("Male|Female|Total", ignore.case = TRUE))
    df_sex <- df %>%
      select(-matches("Urban|Rural|Total", ignore.case = TRUE))
  } else {
    tmp <- df %>% split(grepl("urban|rural", df[, 1]))
    df_residence <- tmp$`TRUE`
    df_sex <- tmp$`FALSE`
  }
  df_list <- list(df_sex %>% add_colname_torow(),
                  df_residence %>% add_colname_torow()) %>%
    setNames(c(
      grep("sex", file_name, value = TRUE),
      grep("residence", file_name, value = TRUE)))
})

df_lst %<>% flatten
for (i in seq_along(df_lst)) {
  write.csv2(df_lst[[i]], names(df_lst[i]), row.names = FALSE)
}

file.remove(sel_residence)

# Provinces cases --------------------------------------------------------------

sel_province <- grep("region", cases, value = TRUE, invert = TRUE)  %>%
  grep("residence", ., value = TRUE, invert = TRUE) %>%
  grep("province", ., value = TRUE) %>%
  grep("class", ., value = TRUE)

df_lst <- lapply(seq_along(sel_province), function(x) {

  df <- read.csv(sel_province[x], header = TRUE, skip = 2, sep = ";",
                 na.strings = "..")
  file_name <- create_filename(sel_province[x])

  tmp <- df %>% split(grepl("Class", df[, 1]))
  df_other <- tmp$`TRUE` %>% rename(class = contains("rovince"))
  df_province <- tmp$`FALSE`
  df_list <- list(df_province %>% add_colname_torow(),
                  df_other %>% add_colname_torow()) %>%
    setNames(c(
      grep("province", file_name, value = TRUE),
      grep("class", file_name, value = TRUE)))
})


df_lst %<>% flatten
for (i in seq_along(df_lst)) {
  write.csv2(df_lst[[i]], names(df_lst[i]), row.names = FALSE)
}

file.remove(sel_province)

# erase everything -------------------------------------------------------------
rm(list = ls())
