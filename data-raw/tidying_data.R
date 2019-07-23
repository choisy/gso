# packages neeeded
library(magrittr)  # for '%>%' & '%<>%'
library(dplyr)  # for 'filter', 'mutate', 'select'
library(tidyr)  # for 'gather'
library(dictionary)  # for 'vn_admin1'
library(purrr)   # for map
library(stringr) # for "str_extract"
setwd("~/Desktop/gso")

# Prerequisites ---------------------------------------------------------------

provinces <- dictionary::vn_admin1
stations <- read.table("data-raw/stations_dictionary.txt",
                       sep = ";", stringsAsFactors = FALSE)
stations <- setNames(stringi::stri_escape_unicode(stations[, 2]),
                     stringi::stri_escape_unicode(stations[, 1]))

# Functions --------------------------------------------------------------------

# Function specific to the Dack Lak province. With the translation, the name of
# the province is always written 'Dak Lak'. But, from 1980 until 2004, the name
# of this province should be written 'Dack Lak'.
# Used in "tidy_clean_df"
dack_lak_function <- function(df) {
  df %<>% mutate(province = as.character(province))
  df[which(df$year < 2004 & df$province == "Dak Lak"), ] <-
    dplyr::filter(df, province == "Dak Lak", year <  2004) %>%
    mutate(province = as.character("Dack Lak"))
  df
}

# Translate the Vietnamese names in one column (col_name) of a data frame (df)
# in an English standardized version. 'hash' should be a named vector.
# Used in "make_col"
translate <- function(df, col_name, hash) {
  df[, col_name] %<>%
    gsub("B\xecnh Dinh", "Binh Dinh", .) %>%
    gsub(" {2, }", " ", .) %>%
    stringi::stri_escape_unicode(.) %>%
    hash[.] %>%
    as.character()
  df <- df[which(is.na(df[, col_name]) == FALSE), ]
}

# Returns boolean to evalue if vect contains years after removing punctuation.
# Used in "make_col"
is_year <-  function(vect) {
  vect %>%
    gsub("[^[:digit:]]", "", .) %>%
    is.element(c(1980:2030)) %>%
    all
}

# Returns boolean to evalue if vect contains residence value (rural/urban) after
# removing punctuation.
# Used in "make_col"
is_residence <-  function(vect) {
  vect %>%
    grepl("Urban|Rural", ., ignore.case = TRUE) %>%
    any
}

# Renames all the vector by `sel`
# Used in "make_col"
rename_vect <- function(vect, sel = "year") {
  vect <- replace(vect, seq_along(vect), sel)
}

# Standardize the columns names, and some columns of spatial and time resolution
# (if exists): get rid of the punctuation, standardized the name of the spatial
# definition column (col_name), correct typo, and correct error of duplicated
# names of column in one specific data frame.
# Also translate one row in the first column of a specific data frame.
# Used in "tidy_clean_df"
make_col <- function(df, col_name = "province", hash = "provinces") {

  # Clean (remove space, punctuation replace by one "_"), correct typo, get rid
  # of the "pre" in front of a year, and standardized the column containing the
  # spatial definition.
  colnames(df) %<>%
    tolower %>%
    gsub("cities..provincies|province..city|cities..provinces", col_name, .) %>%
    gsub("main.counterparts|countries.and.territories|na\\.", col_name, .) %>%
    gsub("[[:punct:]]+", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("prel_2|pre_2|prel_ 2|pre_ 2|sơ_bộ_2", "2", .) %>%
    gsub("^x_|^x", "", .) %>%
    gsub("_$", "", .) %>%
    gsub("iterms", "items", .) %>%
    gsub("toal", "total", .) %>%
    gsub("months", "month", .) %>%
    gsub("residence_and_by_region|residence_and_region", col_name, .)

  # In one data frame, two columns are named `2015`, replace the second to
  # '2016'.
  colnames(df)[which(duplicated(colnames(df)) == TRUE)] <- "2016"

  # Rename columns containing year value by "year"
  df %<>% rename_if(is_year, rename_vect)

  # If a column year is present, get rid of the space and
  # characters and replace punctuation by one "_".
  if (is_in("year", names(df))) {
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

  # If a column country is present, get rid of non informative raw.
  if (is_in("country", names(df))) {
    df[, "country"] %<>%
      gsub("WHOLE WORD|Of which:", NA, ., ignore.case = TRUE) %>%
      gsub("total|Of which", NA, ., ignore.case = TRUE)
    sel <- df[, "country"] %>% unlist  %>%
      grep("Northern|Eastern|Souther|Western|America|Europ|Asia|^Africa|Oceani",
           ., value = TRUE, invert = TRUE) %>% na.omit
    df %<>% filter(is.na(country) == FALSE) %>%
      filter(country %in% sel)
  }

  # Rename columns as residence if contains "rural" or "urban" in the column's
  # value
  if (col_name == "residence") {
    df %<>% rename_if(is_residence, funs(rename_vect(., sel = "residence")))
  }

  # If a column residence is present, get rid of non informative raw.
  if (is_in("residence", names(df))) {
    sel <- df[, "residence"] %>% unlist  %>%
      grep("Urban|Rural", ., value = TRUE, ignore.case = TRUE) %>% na.omit
    df %<>% filter(is.na(residence) == FALSE) %>%
      filter(residence %in% sel) %>%
      mutate(residence = gsub("Resicence - ", "", residence) %>%
               gsub("Residence - ", "", .))
  }


  # In one data frame (concerning state budget), one row name in the first
  # column is not translated to English.
  if (is_in("state_budget_expenditure", names(df))) {
    df[, 1] %<>%
      gsub("Chi sự nghiệp kinh tế, bảo vệ môi trường",
           "Expense for economic, environmental protection", .)
  }

  # Translate spatial definition if necessary
  if (col_name %in% c("province", "station")) {
    df %<>% translate(col_name, hash)
  }

  df
}

# Clean the column after gathering the information in columns as: spatial
# resolution, time resolution, key and value. Take care of the column: `key`,
# `region` ou `residence`. Rename duplicate of total in total, keep only the raw
# in the good spatial resolution.
# Used in "gather_data"
clean_rowkey <- function(df) {
  if (is_in("key", names(df))) {
    sel <- grep("Of which", df$key)
    df[sel, ] %<>% mutate(key = NA)
    df %<>% filter(is.na(key) == FALSE)
    df %<>%
      mutate(key = tolower(key) %>%
               gsub("Total_total|TOTAL_total", "total", .) %>%
               gsub("%", "percent", .) %>%
               gsub("others'", "others_b", .) %>%
               gsub("[[:blank:]]+", "_", .) %>%
               gsub("[[:punct:]]+", "_", .) %>%
               gsub("_+", "_", .) %>%
               gsub("_$", "", .) %>%
               gsub("^_", "", .) %>%
               rem_dup_word %>%
               unlist
             )
  }

  # If a column region is present, get rid of non informative raw and
  # standardized name.
  if (is_in("region", names(df))) {
    df[, "region"] %<>%
      tolower %>%
      gsub("_", " ", .) %>%
      stringr::str_to_title(.)
    sel <- df[, "region"] %>% unlist  %>%
      grep("Whole |Urban|Rural", ., value = TRUE, invert = TRUE,
           ignore.case = TRUE) %>%
      na.omit
    df %<>% filter(is.na(region) == FALSE) %>%
      filter(region %in% sel)
  }

  # If a column region is present, get rid of non informative raw and
  # standardized name.
  if (is_in("residence", names(df))) {
    df[, "residence"] %<>%
      tolower %>%
      gsub("_", " ", .) %>%
      stringr::str_to_title(.)
    sel <- df[, "residence"] %>% unlist  %>%
      grep("Urban|Rural", ., value = TRUE, ignore.case = TRUE) %>%
      na.omit
    df %<>% filter(is.na(residence) == FALSE) %>%
      filter(residence %in% sel)
  }

  df
}

# Gathers the information in columns names of a data frame (df) to have the data
# express in a long format. As the data are processed by spatial definition, the
# parameters 'sp_res' permit to select the processing adpated to the data. If
# the data frame does not contain a column "key", create one with the parameter
# `vect`, keep only the character before `by`, `among`.
# Used in "tidy_clean_df".
gather_data <- function(df, sp_res, vect) {

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

    # If the data are expressed by province but don't have the year information
    # in the column names, gather the column names in one column named "key".
  } else if (sp_res == "province") {
    df %<>% gather(key, value, -province)

    # If the data are expressed by region but don't have the year information
    # in the column names, gather the column names in one column named "key".
  } else if (sp_res == "region") {
    # If the data are expressed by region but contain the spatial information in
    # a column named "items", renamed the column "region_residence"
    if (is_in("items", names(df))) {
      df %<>% rename(region = items)
    }
    if (is_in("region", names(df))){
      sel <- c("region", "year")
      suppressWarnings(df %<>% gather(key, value, -one_of(sel)))
    } else {
      # If the data are expressed by region and the column region is missing but
      # the region information are in the columns names, gather the region
      # information in one column called "region"
      vect_reg <- c("red_river_delta", "northern_midlands_and_mountain_areas",
                    "north_central_and_central_coastal_areas",
                    "central_highlands", "south_east", "mekong_river_delta")
      df %<>% gather(key, value, one_of(vect_reg)) %>%
        mutate(key = gsub("_", " ", key) %>% tools::toTitleCase(.)) %>%
        rename(region = key)
    }


    # If the data are expressed by residence but doesn't have the year
    # information in the column names, gather the numeric columns names in one
    # column named "key".
  } else if (sp_res == "residence" | sp_res == "station") {
    sel1 <- grep("urban|rural|total", names(df), value = TRUE,
                 ignore.case = TRUE)
    df %<>% gather(residence, value, one_of(sel1))
    if (sel1 %>% length == 0) {
      sel <- select_if(df, is.numeric) %>% names %>%
        grep("year", ., invert = TRUE, value = TRUE)
      df %<>% gather(key, value, one_of(sel))
    }

    # If the data are expressed by country and have different column explaining
    # the value (unite, subcategory, detailed inforamtion, ...) but doesn't have
    # the year information in the column names, gather the columns containing
    # the value in one column named "key"
  } else if (sp_res == "country") {
    sel <- c("year", "type_of_land", "items", "countries_and_territories",
             "kinds_of_economic_activity", "main_counterparts",
             "indicators", "commondity_group", "country",
             "child_mortality_rate", "percentage_of_trained_employed_workers")
    suppressWarnings(df %<>% gather(key, value, -one_of(sel)))
  }

  # Rename items column in region for thedata frame expressed by region
  if (sp_res == "region" & is_in("items", names(df))) {
    df %<>% rename(region = items)
  }

  # Rename key column in residence for the data frame expressed by residence
  if (sp_res == "residence") {
    df %<>% rename_if(is_residence, funs(rename_vect(., sel = "residence")))
  }

  # If a column contains the month name, it is rename 'month', and the month
  # information are standardized to be the same across all the data frame
  month_name <- c(month.abb %>% tolower(), month.name %>% tolower())
  if (is_in("key", names(df))) {
    if ( (is.na(match(df$key, month_name)) == FALSE) %>%
        sum == length(df$key) )
      df %<>% rename(month = key)
  }
  if (is_in("month", names(df))) {
    month_trans <- setNames(rep(month.name, 3), c(month_name, month.name))
    df %<>% filter(month %in% names(month_trans)) %>%
      mutate(month = as.character(month) %>% month_trans[.])
  }

  if (is_in("residence", colnames(df))) {
    if (df$residence %>% unique %>% tolower %>%
        stringr::str_detect(., "[^[rural|urban]]") %>% all()) {
      df %<>% mutate(key = gsub("_urban|_rural", "", residence),
                     residence = stringr::str_extract(residence, "urban|rural"))
    }
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
  # If the data frame does not contain a column "key", create one with
  # the parameter `vect`
  if (length(sel) == 0 & is_in("key", names(df)) == FALSE){
    df %<>%  mutate(key = vect %>%
                      gsub(" by(.*)", "", .) %>%
                      gsub(" as(.*)", "", .) %>%
                      gsub(" among(.*)", "", .) %>%
                      gsub(" at(.*)", "", .) %>%
                      gsub(" ", "_", .))
  }  # Unite all the "key" columns together in one column "key"
  if (is_in("key", names(df))) {
    df %<>% unite(key, c(sel, "key"), sep = "_")
  } else {
    df %<>% unite(key, sel, sep = "_")
    }

  df %<>% mutate(key = gsub("_+", "_", key)) %>%
    clean_rowkey()
  # output the data frame with the column names ordered as time, space, key,
  # value.
  suppressWarnings(df %<>%
                    select(one_of(c("year", "month", sp_res, "key", "value"))))

}

# Reads one csv file and standardized and remove the empty columns and rows.
# Returns a data frame.
# Used in "tidy_clean_df"
read_file <- function(file) {
    df <- read.csv(paste0("data-raw/", file), sep = ";", header = TRUE,
                   skip = 1, na.strings = c("..", "...")) %>%
      janitor::remove_empty(c("rows", "cols"))
  df
}

# Remove duplicate words
# Used in "make_subsect"
rem_dup_word <- function(vect) {
  vect <- tolower(vect) %>%
    map(strsplit, "_") %>%
    map(unlist) %>% map(unique) %>%
    map(paste, collapse = "_")
}

# Take care of the subsection in a data frame
# Used in "tidy_clean_df"
make_subsect <- function(df) {

  # Repeat the name of the section on the row below to avoid duplication
  sel <- names(df)[1]
  df %<>%
    mutate(col = str_extract(df[, 1], ".*\\:$") %>%
             str_replace_all("Of which:", "NA")) %>%
    fill(col) %>% unite(sel, col, sel) %>%
    mutate(sel = gsub("NA_|:", "", sel) %>%
             gsub("[[:blank:]+]{1, }", " ", .) %>%
             # rename duplicated as "X_b" instead of "X'"
             gsub("'|,", "_b", .)) %>%
    distinct

  # Head of section written as the name of the section two times, renames one as
  # "total"
  selc <- df$sel %>% map(strsplit, "_") %>% flatten() %>% map(duplicated) %>%
    map(any) %>% unlist
  df$sel[selc] %<>% strsplit("_") %>% map(replace, 2, "total") %>%
    map(paste, collapse = "_") %>% unlist

  df %>%
    rename(!!sel := sel)

}

# from data of the content data frame and by spatial definition, clean and tidy
# the data to express them in a long format. returns a list of data frame
tidy_clean_df <- function(df, sp_res, hash = provinces){

  df_tot <- df %>% filter(sp_resolution == !!sp_res)

  if (sp_res == "river") {
    col_name <- "river"
    sp_res <- "station"
  } else {
    col_name <- sp_res
  }

  lst_df <- lapply(
    seq_along(df_tot$data_frame), function(x) {
    df_sel <- df_tot[x, ]
    file <- paste0(df_sel$category, "/", df_sel$data_frame, ".csv")
    df <- read_file(file) %>%
      make_col(col_name = col_name, hash = hash) %>%
      make_subsect %>%
      gather_data(sp_res = sp_res, vect = df_sel$data_frame) %>%
    # Spread key on value, to have data frame in a wide format
      spread(key, value)

    # The river and stations names are in the same column, they need to be
    # separated
    if (col_name == "river") {
      df %<>% separate(station, c("river", "station"), sep = " - ") %>%
      mutate(station = gsub(" station", "", station)) %>%
      translate("station", hash = hash)
    }

    # Correct the dack lack/ dak lak province names according to the year
    if (any(names(df) %in% "year" & any(names(df) %in% "province"))) {
      df %<>% dack_lak_function
    }

    # Correct the year expression (integer / character )
    if (is_in("year", names(df))) {
      if (!grepl("_", df$year %>% paste(collapse = "|"))) {
        df %<>% mutate(year = as.integer(year))
      }
    }

    df

  }) %>%
    setNames(df_tot$data_name)
}

# Load data --------------------------------------------------------------------

source("data-raw/creating_content.R")
# to have access to function "columns pattern"

# Creating all the data frame
lst_total <- lapply(content$sp_resolution %>% unique %>% sort, function(x) {
  if (x %in% c("station", "river")) {
    suppressWarnings(lst_df <- tidy_clean_df(content, x, stations))
  } else {
    suppressWarnings(lst_df <- tidy_clean_df(content, x))
  }
}) %>%
  unlist(recursive = FALSE)

# Integrating all the data frame in the content data frame
content %<>%
  full_join(tibble::tibble(data = lst_total,
                           data_name = names(lst_total)), by = "data_name")

# Updating two columns containing the time range and time resolution of all the
# data frames
content$time_resolution <- NA
content$time_range <- NA

content$name_col <-  purrr::map(content$data, names) %>%
  purrr::map(paste, collapse = ", ")

# integrate time range and resolution by recognizing pattern in data_frame or
# by the name of the column of data.
content %<>%
  name_pattern("name_col", "month", "time_resolution", "month") %>%
  name_pattern("name_col", "year", "time_resolution", "year") %>%
  name_pattern("data_frame", "31-12-2016", "time_resolution",
               "single time point") %>%
  name_pattern("data_frame", "31-12-2016", "time_range", "31-12-2016") %>%
  name_pattern("data_frame", "31 Februarry 2015", "time_resolution",
               "single time point") %>%
  name_pattern("data_frame", "31 Februarry 2015", "time_range",
               "28-02-2015") %>%
  name_pattern("data_frame", "31 December 2015", "time_resolution",
               "single time point") %>%
  name_pattern("data_frame", "31 December 2015", "time_range",
               "31-12-2015") %>%
  replace_na(list(time_resolution = "year"))

# integrate time range and resolution by recognizing pattern in data_frame or
# by the value inside the column of data.
for (i in seq_along(content$data_name)) {

  df <- content$data[[i]]
  time <- content[i, ]$data_frame
  timerange <- df %>% names %>% unique %>%
    stringr::str_extract("[[:digit:]]{4}") %>% .[!is.na(.)]

  if (is_in("year", colnames(df))) {
    trange <- df$year %>% as.character() %>% strsplit("_") %>% unlist %>%
      range() %>% paste(collapse = "-")
    content[i, ] %<>%
      mutate(time_range = replace_na(time_range, trange))
  }

  if (grep("[[:digit:]]{4}", time) %>% length == 1) {
    content[i, ] %<>%
      mutate(time_range = replace_na(time_range, time %>%
                                       stringr::str_extract("[[:digit:]]{4}")))
  }

  if (grep("[[:digit:]]{4}", timerange) %>% length == 1) {
    content[i, ] %<>%
      mutate(time_range = replace_na(time_range, timerange))
  }
}

content %<>% select(category, subcategory, data_frame, data_name,
                    time_resolution, time_range, sp_resolution, data)
content$data <- with(content, setNames(data, data_name))

# Save content in RData --------------------------------------------------------

usethis::use_data(content, overwrite = TRUE)

# erase everything
rm(list = ls())
