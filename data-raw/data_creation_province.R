# packages neeeded
library(magrittr) # for "%>%" & "%<>%"
library(dplyr) # for "filter",
library(tidyr) # for "gather"
library(dictionary) # for "provinces"

# Prerequisite -----------------------------------------------------------------
# dictionnary of provinces
provinces <- dictionary::provinces
# summary table
load("data/data_frame_summary.rda")

# Functions --------------------------------------------------------------------

# tidy the column names, to have the same format for every columns with the year
# written in number and a descriptif with "_" instead of space.
tidy_table <- function(df){
  names(df) %<>%
    gsub("[:.:]{1,10}", "_", .) %>%
    gsub("__", "_", .) %>%
    gsub("Prel.2015|Pre.2015", "2015", .) %>%
    gsub("X", "", .) %>%
    sub("(.*)(\\d\\d\\d\\d)$*", "\\2_\\1", .) %>%
    sub("_$", "", .)
  df
}

# Translate the province column in the province name in english.
tidy_province <- function(df){
  df[, 1] <- df[, 1] %>%
    unlist  %>%
    as.vector  %>%
    gsub(" city", "", .) %>%
    gsub("  ", " ", .) %>%
    provinces[.]
  names(df)[1] <- "province"
  df %>% filter(is.na(province) == FALSE)
}

# Gathers the year information in one column
gather_data <-  function(df,vect){
  if ( df %>% colnames  %>% grep("19|20", .)  %>% length() > 0){
    df %<>%
      gather(key, value, contains("19"), contains("20")) %>%
      separate(key, letters[1:2], convert = TRUE, extra = "merge") %>%
      rename(year = a, unite = b)
    if (unique(df$unite) %>% length() == 1){
      b_name <- vect  %>%
        sub("(.*)/(.*)", "\\2", .) %>%
        gsub(" by(.*)", "", .) %>%
        gsub(" as(.*)", "", .) %>%
        gsub(" among(.*)", "", .) %>%
        gsub(" at(.*)", "", .) %>%
        gsub(" ", "_", .)
      df %<>%  mutate(unite = b_name)
    }
  } else {
    df %<>% gather(unite, value, -contains("province"))
  }
  df %>% spread(unite, value)
}

# Function specific to the Dack Lak province. On the original file, the name of
# the province is always written "Dak Lak". But, from 1980 until 2004, the name
# of this province should be written "Dack Lak".
dack_lak_function <- function(df) {
  df %<>% mutate(province = as.character(province))
  df[which(df$year < 2004 & df$province == "Dak Lak"), ] <-
    dplyr::filter(df, province == "Dak Lak", year < 2004) %>%
    mutate(province = as.character("Dack Lak"))
  df
}

# Load data --------------------------------------------------------------------

# list of the data frame with priority one
priority_one <- filter(data_frame_summary, priority == "1")
# list of data frame of priority one and by province
one_province <-  priority_one %>%
  filter(`spatial resolution` == "province")
list_province <- lapply(seq_len(dim(one_province)[1]), function(x){
  paste0(one_province[x,"category"],"/", one_province[x,"data frame"],".csv")
}) %>%
  unlist(recursive = F)

# tidy province table
lst_province <- lapply(list_province,function(x){
  sel <- grep("Number of markets|Number of pupils", list_province, invert = T,
              value = T)
  if (x %in% sel) {
    df <- read.csv(paste0("data-raw/", x),
                   header = TRUE, skip = 2, sep = ";", na.strings = "..") %>%
      tidy_table %>%
      tidy_province
  } else {
    df <- read.csv(paste0("data-raw/", x)) %>%
      tidy_table
    df <- df[,-1]
  }
  return(df)
}) %>%
  setNames(list_province %>% sub(".csv", "", .)  %>%
    sub("(.*)/(.*)", "\\2", .))

province_lst <- lapply(seq_along(lst_province), function(x){
  df <- gather_data(lst_province[[x]], names(lst_province[x]))
}) %>%
   setNames(names(lst_province))


# List of data frame of priority one in another scale than province
one_other <-  priority_one %>%
  filter(`spatial resolution` != "province")

# tidy other tables
list_2 <- lapply(seq_len(dim(one_other)[1]), function(x){
  paste0("data-raw/", one_other[x,"category"],"/", one_other[x,"data frame"],
         ".csv") %>%
    read.csv(., header = TRUE, skip = 2, sep = ";", na.strings = "..") %>%
    tidy_table()
}) %>% setNames(one_other$`data frame` %>%
                  sub(".csv", "", .)  %>%
                  sub("(.*)/(.*)", "\\2", .))

# Reunite the two lists of data frame (by province and by other scale) in one
# uniaue list
total <- do.call(c, list(list_2, province_lst))


# Create a vector of name to have short name linked to the subcategory
data_frame_summary %<>%
  mutate(data_name = subcategory) %>%
  group_by(subcategory) %>%
  mutate(data_name = paste0(data_name, "_", seq_along(subcategory)) %>%
           tolower %>%
           gsub(" ", "_", .)) %>%
  ungroup

transl <- data_frame_summary[which(data_frame_summary$`data frame` %in%
                                     names(total) == TRUE),]
dico <- setNames(transl$data_name, transl$`data frame`)


# Correct the dack lack/ dak lak province names.
total_df <- lapply(seq_along(total), function(x){
  df <- total[[x]]
  names(df) %<>% tolower
  if(any(names(df) %in% "year" &
         any(names(df) %in% "province"))){
    df %<>%  dack_lak_function
  }
  df
}) %>% setNames(dico[names(total)])

list2env(total_df,environment())

pop_size <- demography_3

devtools::use_data(pop_size, data_frame_summary, overwrite=T)

devtools::use_data(land_use_1,
                   land_use_2,
                   land_use_3,
                   land_use_4,
                   demography_1,
                   demography_2,
                   demography_3,
                   demography_4,
                   demography_5,
                   demography_7,
                   demography_8,
                   demography_9,
                   demography_10,
                   demography_11,
                   demography_12,
                   demography_14,
                   demography_15,
                   demography_16,
                   education_2,
                   divorce_1,
                   employment_3,
                   employment_11,
                   employment_16,
                   agriculture_1,
                   agriculture_2,
                   agriculture_9,
                   agriculture_10,
                   agriculture_11,
                   agriculture_13,
                   agriculture_14,
                   agriculture_15,
                   agriculture_16,
                   agriculture_17,
                   agriculture_18,
                   agriculture_19,
                   agriculture_20,
                   agriculture_21,
                   agriculture_22,
                   agriculture_23,
                   agriculture_24,
                   agriculture_25,
                   agriculture_26,
                   agriculture_27,
                   agriculture_28,
                   agriculture_29,
                   agriculture_30,
                   agriculture_31,
                   agriculture_32,
                   agriculture_33,
                   agriculture_34,
                   agriculture_35,
                   agriculture_36,
                   agriculture_37,
                   agriculture_42,
                   agriculture_43,
                   agriculture_44,
                   agriculture_45,
                   forestry_1,
                   forestry_3,
                   forestry_5,
                   forestry_6,
                   forestry_7,
                   fishery_2,
                   fishery_3,
                   fishery_4,
                   fishery_6,
                   fishery_8,
                   fishery_9,
                   fishery_11,
                   fishery_12,
                   fishery_13,
                   industry_2,
                   trade_3,
                   trade_4,
                   trade_6,
                   trade_7,
                   price_10,
                   transport_6,
                   transport_7,
                   transport_8,
                   transport_9,
                   transport_16,
                   transport_17,
                   transport_18,
                   transport_19,
                   transport_20,
                   transport_21,
                   education_4,
                   education_6,
                   education_7,
                   education_10,
                   education_11,
                   education_12,
                   education_13,
                   education_15,
                   education_16,
                   education_20,
                   education_24,
                   health_5,
                   health_8,
                   health_11,
                   health_12,
                   health_13,
                   culture_2,
                   culture_3,
                   living_standard_3,
                   living_standard_5,
                   living_standard_7,
                   living_standard_16,
                   living_standard_23,
                   living_standard_26,
                   living_standard_28,
                   living_standard_33,
                   overwrite = TRUE)

##### Test ---------------------------------------------------------------------
library(testthat) #for "expect_equal"

lapply(seq_along(province_lst),function(x){
  testthat::expect_equal(
    mean(province_lst[[x]]$province %in%
           c("Dack Lak",provinces %>% unique)), 1)
})

# erase everything #############################################################

rm(list = ls())
