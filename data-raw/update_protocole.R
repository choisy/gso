## TO MAKE AN UPDATE:

# 1. download all the data frames from gso:
source("data-raw/downloading_data.R")

# 2. rename all the csv file downloaded:
source("data-raw/renaming_csv.R")

# 3. Separate the table containing different information (spatial definition) in
# different table:
source("data-raw/separating_table.R")

# 4. Read all the file and create the object "content" which contains the data
# frames anf the information related (names, file associates, spatial and
# temporal information):
source("data-raw/tidying_data.R")

# 5. Add data frames containing the population size by age group and by year
# into the object "content":
source("data-raw/age_group.R")

# 6. Update description field in the file DESCRIPTION:
descrip <- readLines("DESCRIPTION")
descrip[grep("Description:", descrip) + 1] <-
  paste0("  (last update, ", format(Sys.Date(), "%d %B %Y"), ").")
writeLines(unlist(tot_rd), con = paste0(path, "/R/data.R"), sep = "\n")

# 7. Empty environment:
rm(list = ls())

# 8. Create a .zip file of the downloaded data:
source("data-raw/compressed_files.R")
