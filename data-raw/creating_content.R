# Packages ---------------------------------------------------------------------
library(magrittr)  # for '%>% 'and '%<>% '
library(dplyr)  # for 'select', 'filter', 'bind_rows'and 'mutate'
library(tidyr)  # for 'replace_na'
library(purrr)  # for 'map'
library(lazyeval)
setwd("~/Desktop/gsotest")

# Functions --------------------------------------------------------------------

# Select row by a vector of numerics (sel), and replace NA by x in a column
# (replace_col) for this selected rows in a data frame (df). Return a data frame
# of the same dimension as inputed
# (Used in name_pattern and columns_pattern)
sel_mutate <- function(df, sel, replace_col, x) {
  df_sel <- df[sel, ] %>% mutate(UQ(rlang::sym(replace_col)) :=
                                   replace_na(UQ(rlang::sym(replace_col)), x))
  df_nnsle <- df[-sel, ]
  df <- rbind(df_sel, df_nnsle) %>% arrange(category, data_frame)
}


# In a data frame (df), select rows with a specific pattern (pattern) in one
# column (search_col), and replace NA value in a column (replace_col) by a value
# (x). If the parameters category is filled, match the pattern only in the rows
# with the category corresponding. In this case the data frame inputed should
# have a column names category. Pattern is a vector of one object containing
# one or multiple patterns to match separated by '|'.
# Return a data frame of the same dimension as inputed
name_pattern <- function(df, search_col, pattern, replace_col, x,
                         category = NULL) {

  # If 'category' is imputed, match the pattern only in the subseted data frame
  # with the category corresponging
  if(is.null(category) == FALSE) {
   tmp <- split(df, df[, "category"] == category)
   df_split <- tmp$`TRUE`
   sel <- grep(pattern, df_split[, search_col] %>% unlist, ignore.case = T)
   df_split %<>% sel_mutate(sel = sel, replace_col = replace_col, x = x)
   df <- rbind(df_split, tmp$`FALSE`) %>% arrange(category, data_frame)
  } else {
    sel <- grep(pattern, df[, search_col] %>% unlist, ignore.case = T)
    df %<>% sel_mutate(sel = sel, replace_col = replace_col, x = x)
  }
}

# Read a csv file from a column named "file" of a data frame (df), the column
# "file" should contain the path to the csv file. Then, read the names of the
# columns, if a specific pattern is present (pattern), replace NA value in a
# column (replace_col) by a value (x). Pattern is a pattern with each object
# is one specific pattern.
columns_pattern <- function(df, pattern, replace_col, x) {

  file_lst <- df %>% select(file) %>% unlist
  sel <- lapply(seq_along(file_lst), function(x) {
    any(read.csv(file_lst[x], skip = 1, sep = ";") %>% names %>%
          tolower %>% gsub("[[:punct:]]+", "_", .) %in% pattern)
  }) %>%
    grep("TRUE", .)

  df %<>% sel_mutate(sel = sel, replace_col = replace_col, x = x)
}


# Creation content data frame --------------------------------------------------

all_folder <- dir("data-raw/") %>%
  grep(".R|.xls|.txt", ., invert = T, value = T)

content <- lapply(all_folder, function(x) {
    # category contains the theme of the data frame from website gso data_frame
    # contains the name of the file download from website gso
    df <- data.frame(category = x, data_frame = dir(paste0("data-raw/", x)) %>%
                       gsub(".csv", "", .)) %>%
      mutate_all(as.character)
}) %>%
  bind_rows() %>%
  mutate(file = paste0("data-raw/", category, "/", data_frame, ".csv"))


# Create column sp_resolution --------------------------------------------------

# sp_resolution indicate the spatial resolution of each data frame
content$sp_resolution <- NA

content %<>%
  name_pattern("data_frame", "province|provinces", "sp_resolution",
               "province") %>%
  name_pattern("data_frame", " station| stations", "sp_resolution",
               "station")  %>%
  name_pattern("data_frame",
               " region| regions|Index of income inequality distribution",
               "sp_resolution", "region") %>%
  name_pattern("data_frame", " residence| residences", "sp_resolution",
               "residence")  %>%
  name_pattern("data_frame", " river| rivers", "sp_resolution", "river")  %>%
  name_pattern("data_frame", " sea-port", "sp_resolution", "seaport") %>%
  columns_pattern(c("cities_provincies", "cities_provinces"), "sp_resolution",
                  "province") %>%
  columns_pattern("province_city", "sp_resolution", "station") %>%
  columns_pattern("region", "sp_resolution", "region") %>%
  mutate(sp_resolution = replace_na(sp_resolution, "country"))
# If the spatial resolution is not indicated in the name of the file or in the
# columns, it's mean that the data are expressed for the whole country


# Create column subcategory and data_name --------------------------------------
# subcategory and data_name indicate a subcategory and an easy name to help
# select and call data frame(s). Subcategory created by indification of
# keywords in the name of the file. Each category is explored one by one to
# avoid misidenfication.

content$subcategory <- NA

content %<>%
  # For the category: 'Education', 'Industry' and 'National Account', the
  # subcategory is the same as the category.
  name_pattern("category", "Education", "subcategory", "Education") %>%
  name_pattern("category", "Industry", "subcategory", "Industry") %>%
  name_pattern("category", "National Accounts", "subcategory",
               "National Accounts") %>%
  # For the category: Administrative Unit, Land and Climate
  name_pattern("data_frame", "sea level", "subcategory", "Sea level",
               "Administrative Unit, Land and Climate") %>%
  name_pattern("data_frame",
               "precipitation|temperature|humidity|rainfall|sunshine",
               "subcategory", "Climate",
               "Administrative Unit, Land and Climate") %>%
  name_pattern("data_frame", "land use|used land", "subcategory", "Land use",
               "Administrative Unit, Land and Climate") %>%
  name_pattern("data_frame", "administrative units", "subcategory",
               "Administrative units",
               "Administrative Unit, Land and Climate") %>%
  name_pattern("data_frame", "rivers", "subcategory", "River",
               "Administrative Unit, Land and Climate") %>%
  # For the category: Agriculture, Forestry and Fishery
  name_pattern("data_frame", paste0("crops|cereals|maize|sweet potatoes|farms|",
               "buffaloes|cattle|livestock|pigs|poultry|planted area|paddy|",
               "cassava"), "subcategory", "Agriculture",
               "Agriculture, Forestry and Fishery") %>%
  name_pattern("data_frame", "forest|forests|wood", "subcategory", "Forestry",
               "Agriculture, Forestry and Fishery") %>%
  name_pattern("data_frame", "aquaculture|fishing|fish", "subcategory",
               "Fishery", "Agriculture, Forestry and Fishery") %>%
  # For the category: Health, Culture and Living Standard
  name_pattern("data_frame", paste0("disaster damage|health|medical|patient|",
               "AIDS|immunized|malnutrition"), "subcategory", "Health",
               "Health, Culture and Living Standard") %>%
  name_pattern("data_frame",
               "editorial offices|libraries|sport medals|publication",
               "subcategory", "Culture",
               "Health, Culture and Living Standard") %>%
  name_pattern("data_frame", paste0("waste|dwelling|consumption|income|poverty",
               "|expenditure|households|social|justice|household"),
               "subcategory", "Living standard",
               "Health, Culture and Living Standard") %>%
  # For the category: International Statistics
  name_pattern("data_frame", "GDP|gross domestic product", "subcategory", "GDP",
               "International Statistics") %>%
  name_pattern("data_frame", "population", "subcategory", "Population",
               "International Statistics") %>%
  # For the category: Investment and Construction
  name_pattern("data_frame", "invest", "subcategory", "Investment",
               "Investment and Construction") %>%
  name_pattern("data_frame", "construct|house", "subcategory", "Construction",
               "Investment and Construction") %>%
  # For the category: Population and Employment
  name_pattern("data_frame", "employed|labour|unemployment", "subcategory",
               "Employment", "Population and Employment") %>%
  name_pattern("data_frame",
               "population|average age|rate|expectancy|divorce|sex ratio",
               "subcategory", "Demography", "Population and Employment") %>%
  # For the category: Trade, Price and Tourism
  name_pattern("data_frame", "export|import|trade|commercial centers|markets",
               "subcategory", "Trade", "Trade, Price and Tourism") %>%
  name_pattern("data_frame", "visitors|tourism|travelling", "subcategory",
               "Tourism", "Trade, Price and Tourism") %>%
  name_pattern("data_frame", "price|cost of living", "subcategory", "Price",
               "Trade, Price and Tourism") %>%
  # For the category: Transport, Postal Services and Telecommunications
  name_pattern("data_frame", "telephone|communication", "subcategory",
               "Communication",
               "Transport, Postal Services and Telecommunications") %>%
  name_pattern("data_frame", "transport|passengers|freight|cargos",
               "subcategory", "Transport",
               "Transport, Postal Services and Telecommunications") %>%
  # Create the data_name value
  mutate(data_name = subcategory) %>%
  group_by(subcategory) %>%
  mutate(data_name = paste0(data_name, "_", seq_along(subcategory)) %>%
           tolower %>% gsub(" ", "_", .)) %>%
  ungroup %>%
  # Remove the column "file", unecessary for the rest of the process
  select(-file)

# Save content in RData --------------------------------------------------------

devtools::use_data(content, overwrite = TRUE)

# erase everything
rm(list = ls())
