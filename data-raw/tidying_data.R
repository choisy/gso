# packages neeeded
library(magrittr)  # for '%>%' & '%<>%'
library(dplyr)  # for 'filter', 'mutate', 'select'
library(tidyr)  # for 'gather'
library(dictionary)  # for 'provinces'
setwd("~/Desktop/gsotest")

# Prerequisites ---------------------------------------------------------------

provinces <- dictionary::vn_province
stations <- read.table("data-raw/stations_dictionary.txt",
                       sep = ";", stringsAsFactors = FALSE)
stations <- setNames(stringi::stri_escape_unicode(stations[, 2]),
                     stringi::stri_escape_unicode(stations[, 1]))

load("data/content.rda")

# Functions --------------------------------------------------------------------

# Function specific to the Dack Lak province. With the translation, the name of
# the province is always written 'Dak Lak'. But, from 1980 until 2004, the name
# of this province should be written 'Dack Lak'.
dack_lak_function <- function(df) {
    df %<>% mutate(province = as.character(province))
    df[which(df$year < 2004 & df$province == "Dak Lak"), ] <-
      dplyr::filter(df, province == "Dak Lak", year <  2004) %>%
      mutate(province = as.character("Dack Lak"))
    df
}

# Standardize the columns names, and the column year (if exists): get rid of
# the punctuation, standardized the name of the spatial definition column
# (col_name), correct typo, and correct error of duplicated names of column in
# one specific data frame.
# Also translate one row in the first column of a specific data frame.
make_col <- function(df, col_name = "province") {

  # Clean (remove space, punctuation replace by one "_"), correct typo, get rid
  # of the "pre" in front of a year, and standardized the column containing the
  # spatial definition.
  colnames(df) %<>%
    tolower %>%
    gsub("cities..provincies|province..city|cities..provinces", col_name, .) %>%
    gsub("countries.and.territories", col_name, .) %>%
    gsub("[[:punct:]]+", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("prel_2|pre_2|prel_ 2|pre_ 2|sơ_bộ_2", "2", .) %>%
    gsub("^x_|^x", "", .) %>%
    gsub("_$", "", .) %>%
    gsub("iterms", "items", .) %>%
    gsub("toal", "total", .) %>%
    gsub("months", "month", .) %>%
    gsub("residence_and_by_region|residence_and_region", "region_residence", .)

    # In one data frame, two columns are named `2015`, replace the second to
    # '2016'.
    colnames(df)[which(duplicated(colnames(df)) == TRUE)] <- "2016"

    # If a column year is present, get rid of the space and
    # characters and replace punctuation by one "_".
    if (is.na(match("year", names(df))) == FALSE) {
      df[, "year"] %<>%
        tolower() %>%
        gsub("[[:blank:]]+", "", .) %>%
        gsub("[[:punct:]]+", "_", .) %>%
        gsub("_+", "_", .) %>%
        gsub("prel_2|pre_2|prel_ 2|pre_ 2|sơ_bộ_2", "2", .) %>%
        gsub("year", NA, .) %>%
        gsub("total", NA, .)
      df %<>% filter(is.na(year) == FALSE)
    }

    # In one data frame (concerning state budget), one row name in the first
    # column is not translated to English.
    if (is.na(match("state_budget_expenditure", names(df))) == FALSE) {
      df[, 1] %<>%
        gsub("Chi sự nghiệp kinh tế, bảo vệ môi trường",
             "Expense for economic, environmental protection", .)
    }
    df
}

# Get rid of the raw 'of which:' NA which as no information and substitute the
# row with the repetition of the word 'Total' to one 'total' in a column "key"
# of a data frame (df).
clean_rowkey <- function(df) {
  if (is_in("key", names(df))) {
    sel <- grep("Of which", df$key)
    df[sel, ] %<>% mutate(key = NA)
    df %<>% filter(is.na(key) == FALSE)
    df %<>% mutate(key = gsub("Total_total|TOTAL_total", "total", key) %>%
                     gsub("[[:blank:]]+", "_", .) %>%
                     gsub("[[:punct:]]+", "_", .) %>%
                     gsub("_+", "_", .) %>%
                     gsub("_$", "", .))
  }
  df
}


# Gathers the information in columns names of a data frame (df) to have the data
# express in a long format. As the data are processed by spatial definition, the
# parameters 'sp_res' permit to select the processing adpated to the data.
gather_data <- function(df, sp_res) {

    # If the year is the only name of the columns, gather the year in one column
    if (isTRUE(names(df) %>% grep("^[[:digit:]]{4}$", .) %>% length >= 1)) {
        sel <- grep("[[:digit:]]{4}", names(df), value = T)
        df %<>% gather(year, value, one_of(sel), convert = TRUE)

    # If the column names are two years separate by '_', gather the year in one
    # column, containing the year expressed like : "XXXX_XXXX"
    } else if (isTRUE(names(df) %>% grep("[[:digit:]]{4}_[[:digit:]]{4}", .) %>%
                      length >= 1)) {
        sel <- grep("[[:digit:]]{4}", names(df), value = T)
        df %<>% gather(year, value, one_of(sel), convert = TRUE)

    # If the column names start with the year, gather the year in one column and
    # put the rest of the column names in one column named "key".
    } else if (isTRUE(names(df) %>% grep("^[[:digit:]]{4}", .) %>%
                      length >= 1)) {
        sel <- grep("[[:digit:]]{4}", names(df), value = T)
        df %<>%
          gather(year, value, one_of(sel), convert = TRUE) %>%
          separate(year, c("year", "key"), sep = "(?<=[[:digit:]]{4})",
                   extra = "merge") %>%
          mutate(key = gsub("^_", "", key))

    # If the column names end with the year, gather the year in one column and
    # put the rest of the column names in one column named "key".
    } else if (isTRUE(names(df) %>% grep("[[:digit:]]{4}$", .) %>%
                      length >= 1)) {
        sel <- grep("[[:digit:]]{4}", names(df), value = T)
        df %<>% gather(year, value, one_of(sel), convert = TRUE) %>%
          separate(year, c("key", "year"), sep = "(?=[[:digit:]]{4})",
            extra = "merge") %>% mutate(key = gsub("_$", "", key))

    # If the data are expressed by region and the column region is missing but
    # the region information are in the columns names, gather the region
    # information in one column called "region"
    } else if (sp_res == "region" & is_in("region", names(df)) == FALSE) {
        vect_reg <- c("red_river_delta", "northern_midlands_and_mountain_areas",
                      "north_central_and_central_coastal_areas",
                      "central_highlands", "south_east", "mekong_river_delta")
        df %<>% gather(key, value, one_of(vect_reg)) %>%
          mutate(key = gsub("_", " ", key) %>% tools::toTitleCase(.)) %>%
          rename(region = key)

    # If the data are expressed by province but don't have the year information
    # in the column names, gather the column names in one column named "key".
    } else if (sp_res == "province") {
        df %<>% gather(key, value, -province)

    # If the data are expressed by region but don't have the year information
    # in the column names, gather the column names in one column named "key".
    } else if (sp_res == "region") {
        sel <- c("region", "year")
        suppressWarnings(df %<>% gather(key, value, -one_of(sel)))

    # If the data are expressed by residence but doesn't have the year
    # information in the column names, gather the numeric columns names in one
    # column named "key".
    } else if (sp_res == "residence" | sp_res == "station") {
        sel <- select_if(df, is.numeric) %>% names %>%
          grep("year", ., invert = TRUE, value = TRUE)
        df %<>% gather(key, value, one_of(sel))

#    # If the data are expressed by station but doesn't have the year information
#   # in the column names, gather the numeric columns names in one
#        # column names "key".
#    } else if (sp_res == "station") {
#        sel <- select_if(df, is.numeric) %>% names %>% grep("year", ., invert = TRUE, value = TRUE)
#        df %<>% gather(key, value, one_of(sel))

    # If the data are expressed by country and have different column explaining
    # the value (unite, subcategory, detailed inforamtion, ...) but doesn't have
    # the year information in the column names, gather the columns containing
    # the value in one column named "key"
    } else if (sp_res == "country") {
        sel <- c("year", "type_of_land", "items", "countries_and_territories",
                 "kinds_of_economic_activity", "main_counterparts",
                 "indicators", "commondity_group", "country")
        suppressWarnings(df %<>% gather(key, value, -one_of(sel)))
    }

    # If a column contains the month name, it is rename 'month', and the month
    # information are standardized to be the same across all the data frame
    month_name <- c(month.abb %>% tolower(), month.name %>% tolower())
    if (is_in("key", names(df))) {
        if ((is.na(match(df$key, month_name)) == FALSE) %>%
            sum == length(df$key))
          df %<>% rename(month = key)
    }
    if (is_in("month", names(df))) {
        month_trans <- setNames(rep(month.name, 3), c(month_name, month.name))
        df %<>% mutate(month = month_trans[month])
    }

    # If the data are expressd by region but contain the spatial information in
    # a column named "items", renamed the column "region_residence"
    if (sp_res == "region" & is_in("items", names(df))) {
        df %<>% rename(region_residence = items)
    }

    # The names of the columns should be: one or two columns for time
    # resolution (year, month), one column for spatial resolution (two for
    # rivers: stations and rivers), one for the value (value), one for the key
    # of the value (key) (unite or specific subcategory of the value).
    # If others names columns are present, (different key column mostly),
    # merge together in an unique key column containing all the information
    sel <- df %>% names %>%
      grep(paste0("^", sp_res, "$|year|value|key|month"), ., value = T,
           invert = T)
    if (length(sel) >= 1) {
      # Unite all the "key" columns together in one column "key"
        if (is_in("key", names(df))) {
            df %<>% unite(key, c(sel, "key"), sep = "_")
        } else {
            df %<>% unite(key, sel, sep = "_")
        }
    }

    # output the data frame with the column names ordered as time, space, key,
    # value.
    suppressWarnings(df %>%
                       select(one_of(
                         c("year", "month", sp_res, "key", "value"))))
}

# Translate the Vietname names in one column (col_name) of a data frame (df) in
# an English standardized version. 'hash' should be a named vector.
translate <- function(df, col_name, hash) {
  df[, col_name] %<>%
    gsub("B\xecnh Dinh", "Binh Dinh", .) %>%
    stringi::stri_escape_unicode(.) %>%
    hash[.] %>%
    as.character()
  df <- df[which(is.na(df[, sp_res]) == FALSE), ]
}

# Read one csv file and standardized and remove the empty columns and rows.
# Return a data frame
read_file <- function(file) {

    # For 4 csv files, the format is different
    if (file == "Education/Number of pupils of general education as of 30 September in university.csv" |
        file == "Trade, Price and Tourism/Number of markets as of annual December 31st by class.csv" |
        file == "Education/Number of pupils of general education as of 30 September by province.csv" |
        file == "Trade, Price and Tourism/Number of markets as of annual December 31st by province.csv") {
      df <- read.csv(paste0("data-raw/", file), sep = ";", header = TRUE)
      df <- df[, -1] %>%
        janitor::remove_empty(c("rows", "cols"))
    } else {
      df <- read.csv(paste0("data-raw/", file), sep = ";", header = TRUE,
                     skip = 1, na.strings = c("..", "...")) %>%
        janitor::remove_empty(c("rows", "cols"))
    }
    df
}

# Load data --------------------------------------------------------------------

sp_res <- "province"
df_tot <- content %>%
  filter(sp_resolution == sp_res)
lst_province <- lapply(seq_along(df_tot$data_frame), function(x) {
    df <- df_tot[x, ]
    file <- paste0(df$category, "/", df$data_frame, ".csv")
    df <- read_file(file) %>%
      make_col %>%
      translate(sp_res, provinces) %>%
      gather_data(sp_res) %>%
      clean_rowkey
}) %>% setNames(df_tot$data_name)

# lst_province %>% purrr::map(head) # to check the result

sp_res <- "country"
df_tot <- content %>% filter(sp_resolution == sp_res)
lst_country <- lapply(seq_along(df_tot$data_frame), function(x) {
    df <- df_tot[x, ]
    file <- paste0(df$category, "/", df$data_frame, ".csv")
    df <- read_file(file) %>%
      make_col(col_name = sp_res) %>%
      gather_data(sp_res) %>%
      clean_rowkey
}) %>% setNames(df_tot$data_name)

# lst_country[1:75] %>% purrr::map(head) lst_country[75:156] %>% purrr::map(head)

sp_res <- "region"
df_tot <- content %>% filter(sp_resolution == sp_res)
lst_region <- lapply(seq_along(df_tot$data_frame), function(x) {
    df <- df_tot[x, ]
    file <- paste0(df$category, "/", df$data_frame, ".csv")
    df <- read_file(file) %>%
      make_col %>%
      gather_data(sp_res) %>%
      clean_rowkey
}) %>% setNames(df_tot$data_name)

# lst_region %>% purrr::map(head)

sp_res <- "residence"
df_tot <- content %>% filter(sp_resolution == sp_res)
lst_residence <- lapply(seq_along(df_tot$data_frame), function(x) {
    df <- df_tot[x, ]
    file <- paste0(df$category, "/", df$data_frame, ".csv")
    df <- read_file(file) %>%
      make_col %>%
      gather_data(sp_res) %>%
      clean_rowkey
}) %>% setNames(df_tot$data_name)

# lst_residence %>% purrr::map(head)

sp_res <- "station"
df_tot <- content %>% filter(sp_resolution == sp_res)
lst_station <- lapply(seq_along(df_tot$data_frame), function(x) {
    df <- df_tot[x, ]
    file <- paste0(df$category, "/", df$data_frame, ".csv")
    df <- read_file(file) %>%
      make_col(col_name = "station") %>%
      translate(sp_res, stations) %>%
      gather_data(sp_res)  %>%
      clean_rowkey
}) %>% setNames(df_tot$data_name)

# lst_station %>% purrr::map(head)

sp_res <- "river"
df_tot <- content %>% filter(sp_resolution == sp_res)
lst_river <- lapply(seq_along(df_tot$data_frame), function(x) {
    df <- df_tot[x, ]
    file <- paste0(df$category, "/", df$data_frame, ".csv")
    df <- read_file(file) %>%
      make_col(col_name = "station") %>%
      gather_data(sp_res = "station") %>%
      separate(station, c("river", "station"), sep = " - ") %>%
      mutate(station = gsub(" station", "", station)) %>% translate("station",
        stations) %>%
      clean_rowkey
}) %>% setNames(df_tot$data_name)

# lst_river %>% purrr::map(head)

sp_res <- "seaport"
df_tot <- content %>% filter(sp_resolution == sp_res)
lst_seaport <- lapply(seq_along(df_tot$data_frame), function(x) {
    df <- df_tot[x, ]
    file <- paste0(df$category, "/", df$data_frame, ".csv")
    df <- read_file(file) %>% make_col %>% gather_data(sp_res)
}) %>% setNames(df_tot$data_name)

# lst_seaport %>% purrr::map(head)

# Regroup all the data set in one list -----------------------------------------

total_df <- purrr::splice(lst_country, lst_province, lst_region, lst_residence, lst_river, lst_seaport, lst_station)

# Correct the dack lack/ dak lak province names.
total_df <- lapply(seq_along(total_df), function(x) {
    df <- total_df[[x]]
    if (any(names(df) %in% "year" & any(names(df) %in% "province"))) {
        df %<>% dack_lak_function
    }
    df
}) %>% setNames(names(total_df))


# Updating content data frame --------------------------------------------------

# Integrating all the data in the content data frame
content %<>% full_join(tibble::tibble(data = total_df, data_name = names(total_df)), by = "data_name")


# Updating two columns containing the time range and time resolution of all the data frames
content$time_resolution <- NA
content$time_range <- NA

for (i in seq_along(content$data_name)) {

    df <- content$data[[i]]
    names_col <- df %>% names()

    if (is_in("month", names_col) == TRUE) {
        content[i, ] %<>% mutate(time_resolution = replace_na(time_resolution, "month"))
    }

    if (is_in("year", names_col) == TRUE) {
        range_y <- df$year %>% unique %>% range %>% sub("^(\\d{4}).*$", "\\1", .)
        content[i, ] %<>% mutate(time_resolution = replace_na(time_resolution, "year"), time_range = replace_na(time_range,
            paste0(min(range_y), "-", max(range_y))))
    } else {
        time <- content[i, ]$data_frame
        if (grep("31-12-2016", time) %>% length == 1) {
            content[i, ] %<>% mutate(time_resolution = replace_na(time_resolution, "single time point"), time_range = replace_na(time_range,
                "31-12-2016"))

        } else if (grep("31 Februarry 2015", time) %>% length == 1) {
            content[i, ] %<>% mutate(time_resolution = replace_na(time_resolution, "single time point"), time_range = replace_na(time_range,
                "28-02-2015"))
        } else if (grep("31 December 2015", time) %>% length == 1) {
            content[i, ] %<>% mutate(time_resolution = replace_na(time_resolution, "single time point"), time_range = replace_na(time_range,
                "31-12-2015"))
        } else if (grep("[[:digit:]]{4}", time) %>% length == 1) {
            content[i, ] %<>% mutate(time_resolution = replace_na(time_resolution, "year"), time_range = replace_na(time_range,
                time %>% stringr::str_extract("[[:digit:]]{4}")))
        } else {
            timerange <- df$key %>% unique %>% stringr::str_extract("[[:digit:]]{4}") %>% .[!is.na(.)]
            content[i, ] %<>% mutate(time_resolution = replace_na(time_resolution, "year"), time_range = replace_na(time_range,
                timerange))
        }
    }
}

content %<>% select(category, subcategory, data_frame, data_name, time_resolution, time_range, sp_resolution,
    data)


# Save content in RData --------------------------------------------------------

devtools::use_data(content, overwrite = TRUE)

# erase everything
rm(list = ls())
