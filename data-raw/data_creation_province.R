# packages neeeded
library(magrittr) # for "%>%" & "%<>%"
library(dplyr) # for "filter",
library(tidyr) # for "gather"

# Prerequisite -----------------------------------------------------------------
# dictionnary of provinces
provinces <- readRDS("data-raw/province.RDS")
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
    gsub("city", "", .) %>%
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

#lapply(names(total), function(x){
#  as.symbol(x) %>%
#    devtools::use_data(overwrite = TRUE, internal = TRUE, overwrite = TRUE)
#})

list2env(total,environment())
devtools::use_data(`Land use (As of 1 January 2014)`,
                   `Average population by sex and by residence`,
                   `Sex ratio by residence`,
                   `Crude birth rate, crude death rate and natural increase rate of population by residence`,
                   `Total fertility rate by residence`,
                   `Child mortality rate by sex and by residence`,
                   `Land use by province (As of 1 January 2014)`,
                   `Structure of used land by province (As of 1 January 2014)`,
                   `Index of land change in 2014 over 2013 by province (As of annual 1st January)`,
                   `Area, population and population density by province`,
                   `Average population by province`,
                   `Sex ratio of population by province`,
                   `Crude birth rate, crude death rate and natural increase rate by province`,
                   `Total fertility rate by province`,
                   `Infant mortality rate by province`,
                   `Under five mortality rate by province`,
                   `Population growth rate by province`,
                   `In-migration rate, out-migration rate and net-migration rate by province`,
                   `Percentage of literate population at 15 years of age and above by province`,
                   `Number of divorce cases cleared up in 2015 by province and by level`,
                   `Labour force at 15 years of age and above by province`,
                   `Percentage of employed workers at 15 years of age and above among population by province`,
                   `Percentage of trained employed workers at 15 years of age and above by province`,
                   `Number of farms by province`,
                   `Number of farms by kinds of manufacturing sector and by province`,
                   `Planted area of cereals by province`,
                   `Production of cereals by province`,
                   `Production of cereals per capita by province`,
                   `Planted area of paddy by province`,
                   `Yield of paddy by province`,
                   `Production of paddy by province`,
                   `Planted area of spring paddy by province`,
                   `Yield of spring paddy by province`,
                   `Production of spring paddy by province`,
                   `Planted area of autumn paddy by province`,
                   `Yield of autumn paddy by province`,
                   `Production of autumn paddy by province`,
                   `Planted area of winter paddy by province`,
                   `Yield of winter paddy by province`,
                   `Production of winter paddy by province`,
                   `Planted area of maize by province`,
                   `Yield of maize by province`,
                   `Production of maize by province`,
                   `Planted area of sweet potatoes by province`,
                   `Production of sweet potatoes by province`,
                   `Planted area of cassava by province`,
                   `Production of cassava by province`,
                   `Planted area of sugar-cane by province`,
                   `Production of sugar-cane by province`,
                   `Planted area of peanut by province`,
                   `Production of peanut by province`,
                   `Planted area of soya-bean by province`,
                   `Production of soya-bean by province`,
                   `Number of buffaloes as of annual 1st October`,
                   `Number of cattles as of annual 1st by province`,
                   `Number of pigs as of annual 1st October by province`,
                   `Number of poultry as of annual 1st October by province`,
                   `Area of forest as of 31 December by province`,
                   `Area of concentrated planted forest by province`,
                   `Gross output of wood by province`,
                   `Area of fired forest by province`,
                   `Area of destroyed forest by province`,
                   `Area of water surface for the aquaculture by province`,
                   `Number of upper 90 CV offshore fishing vessels by province`,
                   `Total capacity of upper 90 CV offshore fishing vessels by provinc`,
                   `Production of fishery by province`,
                   `Production of fishery caught by province`,
                   `Production of caught sea fish by province`,
                   `Production of aquaculture by province`,
                   `Production of aquaculture fish by province`,
                   `Production of aquaculture shrimp by province`,
                   `Index of Industrial production by province`,
                   `Retail sales of goods and services at current prices by province`,
                   `Number of markets as of annual December 31st by province`,
                   `Number of supermarkets as of annual December 31st by province`,
                   `Number of commercial centers as of annual December 31st by province`,
                   `Spatial cost of living index among provinces (Ha Noi = 100)`,
                   `Volume of passengers carried by province`,
                   `Number of passengers traffic by province`,
                   `Number of passengers carried by the road by province`,
                   `Number of passengers traffic by the road by province`,
                   `Volume of freight by province`,
                   `Volume of freight traffic by province`,
                   `Volume of freight by the road by province`,
                   `Volume of freight traffic by the road by province`,
                   `Volume of freight by the waterway by province`,
                   `Volume of freight traffic by the waterway by province`,
                   `Number of schools, classes, teachers and pupils of kindergarten education as of 30 September by province`,
                   `Number of school of general education as of 30 September by province`,
                   `Number of classes of general education as of 30 September by province`,
                   `Number of direct teaching teachers of general education as of 30 September by province`,
                   `Number of direct teaching woman teachers of general education as of 30 September by province`,
                   `Number of ethnic minority direct teaching teachers of general education as of 30 September by some provinces`,
                   `Number of pupils of general education as of 30 September by province`,
                   `Number of schoolgirls of general education as of 30 September by province`,
                   `Number of ethnic minority pupils of general education as of 30 September by some provinces`,
                   `Number of teachers, students in universities and colleges by province`,
                   `Number of teachers and students in professional secondary by province`,
                   `Number of health establishments under provincial departments of health by province`,
                   `Number of patient beds under provincial departments of health by province`,
                   `Number of medical staff under provincial departments of health by province`,
                   `Number of pharmaceutical staffs under provincial departments of health by province`,
                   `Number of people infected with HIV-AIDS and number of AIDS deaths by province`,
                   `Number of libraries under local management by province`,
                   `Number of editorial offices by province`,
                   `Monthly average income per capita at current prices by income source and by province`,
                   `Monthly average income per capita at current prices by income quintile and by province`,
                   `Difference between the highest income quintile and the lowest income quintile on monthly average income per capita at current prices by province`,
                   `Poverty rate by province`,
                   `Percentage of household using electricity by province`,
                   `Percentage of households in 2014 having house by type of house and by province`,
                   `Average dwelling area per capita in 2014 by type of house and by province`,
                   `Average collected solid waste treated per day in 2015 by province`,
                   overwrite = TRUE, internal = TRUE)

##### Test ---------------------------------------------------------------------
lapply(seq_along(province_lst),function(x){
  testthat::expect_equal(
    mean(province_lst[[x]]$province %in%
           c("Dack Lak",provinces %>% unique)), 1)
})

# erase everything #############################################################

rm(list = ls())
