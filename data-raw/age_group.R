library(tabulizer) # for "extract_test", "extract_tables"
library(tidyr)     # for "separate", "nest", "gather", "spread", "unnest"
library(purrr)     # for "map",
library(dplyr)     # for "mutate_all", "distinct", "select", "mutate", "filter",
# "tibble", "group_by"
library(magrittr)  # for "%>%", "%<>%"
# The packages "stringr", "janitor", "dictionary" are also necessary

# Functions --------------------------------------------------------------------

# For some columns, the value of two columns are unite and separated by " ", the
# problem is that the value inside one column is also expressed with " ", for
# example "100 000" for 100000.
# This function is taking care of this problem and is separating columns in two.
# For example:
# |"100 000 120 000"| becomes |"100 000"|"120 000"|
# Recognize the columns containing the information of two columns when the
# number of character in the first row of the column is superior to the `limit`
# argument.
sep_col <- function(df, limit = 10) {
  sel <- df[1, ] %>% nchar(.) %>% {. > limit} %>% stringr::str_which("TRUE")
  df <- df %>% mutate_all(stringr::str_trim) %>%
    mutate_all(function(x) sub(" ", ".", x) %>% sub(" ", "_", .))
  df <- df[, (which(nchar(df[2, ]) != 0))]

  if (length(sel) > 0) {
    suppressWarnings(df %<>%
      separate(paste0("V", sel[1]), paste0(sel[1], ".", 1:2), sep = "_"))
    if (length(sel) == 2) {
      suppressWarnings(df %<>%
          separate(paste0("V", sel[2]), paste0(sel[2], ".", 1:2), sep = "_"))
    }
    df %<>%
      distinct %>%
      mutate_all(function(x) sub("\\.", " ", x)) %>%
      janitor::remove_empty(c("cols", "rows"))
  }

  names(df) <- paste0("V", seq_along(df))
  return(df)
}

# Data -------------------------------------------------------------------------

## PROVINCE NAME _ to extract the province name, we had to transform the pdf
# file with the page in portrait and with only the page containing the
# information to save time. The file created
# `age_group_province`, comes from the original file "Tieng Anh_Du Bao Dan So
# Viet Nam.compressed.pdf".
# The output is a vector of province name in the same order as the table extract
# to create "age_group".
text <- extract_text("data-raw/Age Group/age_group_province.pdf",
  area = list(c(81.60, 66.52, 166.79, 546.24))) %>%
  gsub("[[:digit:]]*", "", .) %>% gsub("\n*", "", .) %>% strsplit(",") %>%
  purrr::map(1) %>%
  stringr::str_extract("(?<=\\.).{1,}") %>%
  gsub("[[:punct:]]*", "", .) %>%
  gsub("^ ", "", .) %>%
  grep("projected", ., value = TRUE, invert = TRUE, ignore.case = TRUE) %>%
  na.omit %>%
  tolower %>%
  rep(., each = 2) %>%
  dictionary::vn_admin1[.]

## AGE-GROUP DATA FRAME
all_tab <- extract_tables(
  "data-raw/Age Group/Tieng Anh_Du Bao Dan So Viet Nam.compressed.pdf",
  method = "decide", pages = c(108:233))
age_group <- all_tab %>%
  setNames(text) %>%
  map(as.data.frame) %>%
  map(mutate_if, is.factor, as.character) %>%
  map(filter, grepl("Total|otal", V1) == FALSE) %>%
  map(sep_col) %>%
  map(rename, age = "V1") %>%
  tibble(province = names(.)) %>%
  group_by(province)  %>%
  nest %>%
  mutate(data = map(data, unlist, FALSE) %>%
           map(bind_rows)) %>%
  unnest %>%
  mutate(key = c(rep("Total", 17), rep("Male", 18), rep("Female", 18)) %>%
           rep(63)) %>%
  setNames(c("province", "age", c(2014:2034), "key")) %>%
  tidyr::gather(year, value, -province, -age, -key) %>%
  mutate(value = value %>% gsub("\\.|[[:blank:]]", "", .) %>% as.numeric(.),
         year  = as.integer(year)) %>%
  filter(is.na(value) == FALSE, key != "Total") %>%
  spread(key, value)

# Integrating age_group in `content` -------------------------------------------

load("data/content.rda")

c_age_group <- tibble::tibble(
  category = NA,
  subcategory = "Population",
  data_frame = "Vietnam population projection 2014 - 2049",
  data_name = "age_group",
  time_resolution = "year",
  time_range = range(age_group$year) %>% paste(collapse = "-"),
  sp_resolution = "province",
  data = list(as.data.frame(age_group)))

content %<>% rbind(c_age_group)

# Save content in RData --------------------------------------------------------

usethis::use_data(content, overwrite = TRUE)

# Remove everything ------------------------------------------------------------

rm(list = ls())
