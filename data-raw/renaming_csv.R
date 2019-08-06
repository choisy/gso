# Packages ---------------------------------------------------------------------
library(magrittr)  # for ' %>% '

# Prerequisite -----------------------------------------------------------------
all_folder <- dir() %>% grep(".R|.xls|.txt|.zip", ., invert = T, value = T)

# Data -------------------------------------------------------------------------
# remove the '(*)' from files names:
dirs <- grep("\\..*$", dir(), value = TRUE, invert = TRUE)
for (directory in dirs) {
    setwd(directory)
    for (pattern in c("\\(\\*\\)", "\\*")) {
        old_names <- dir()
        old_names <- grep(pattern, old_names, value = TRUE)
        new_names <- gsub(pattern, "", old_names)
        for (i in seq_along(old_names)) file.rename(old_names[i], new_names[i])
    }
    setwd("..")
}

# shortening too long files names (all cases are in the folder Health, Cult...):
setwd("Health, Culture and Living Standard")
f <- function(x) {
    nbchar <- nchar(x)
    x[which(nbchar > sort(nbchar, decreasing = TRUE)[3])]
}

oldnames <- f(dir())
newnames <- sub(",.*$", "", oldnames) %>% paste0(".csv")
for (i in seq_along(oldnames)) file.rename(oldnames[i], newnames[i])
# going back to the working direction
setwd("..")

# Renaming file with ' .csv' to '.csv':
list_rename <- lapply(all_folder, function(x) {
    dir(paste0(x, "/")) %>% grep(" .csv", ., value = T) %>% paste0(x, "/", .)
}) %>%
  unlist(recursive = F) %>% grep(" .csv", ., value = T)

new_name <- lapply(list_rename, function(x) {
    gsub(" .csv", ".csv", x)
}) %>%
  unlist(recursive = F)

lapply(seq_along(list_rename), function(x) {
    file.rename(list_rename[x], new_name[x])
})

# Renaming file beginning with number 'XX.'
list_rename <- lapply(all_folder, function(x) {
    dir(x) %>% paste0(x, "/", .)
}) %>%
  unlist(recursive = F)

new_name <- lapply(list_rename, function(x) {
    gsub("\\/[[:digit:]]{1,2}\\.", "/", x) %>%
    gsub("\\/[[:digit:]]{1,2}\\-[[:digit:]]{1,2}\\.", "/", .)
}) %>%
  unlist(recursive = F)

lapply(seq_along(list_rename), function(x) {
    file.rename(list_rename[x], new_name[x])
})

# remove everything ------------------------------------------------------------

rm(list = ls())
