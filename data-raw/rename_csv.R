# Packages: ####################################################################
library(magrittr) # for " %>% "

# Renaming file with " .csv" to ".csv"##########################################
all_folder <- dir("data-raw/") %>% grep(".R|.xls", ., invert = T, value = T)

# Rename necessary file
list_rename <- lapply(all_folder, function(x){
  dir(paste0("data-raw/", x, "/")) %>%
    grep(" .csv", ., value = T) %>%
    paste0("data-raw/", x, "/", .)
}) %>% unlist(recursive = F) %>%
  grep(" .csv", ., value = T)

new_name <- lapply(list_rename, function(x){
  gsub(" .csv", ".csv", x)
}) %>% unlist(recursive = F)

lapply(seq_along(list_rename), function(x){
  file.rename(list_rename[x], new_name[x])
  })
