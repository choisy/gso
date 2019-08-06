# Packages ---------------------------------------------------------------------
library(zip)

# Data -------------------------------------------------------------------------

folder_zip <- grep(".R|.txt|Age Group", dir("data-raw/"), value = TRUE,
                   invert = TRUE)
path_to <- paste0("data-raw/gso_" ,format(Sys.Date(), "%d%m%Y"), "/")
dir.create(path_to)
## Move files into the gso folder
lapply(folder_zip, function(x) {
  path <- paste0("data-raw/", x, "/")
  from_files <- paste0(path, dir(path))
  to_folder <- paste0(path_to, basename(path), "/")
  to_files <- paste0(to_folder, dir(path))
  dir.create(to_folder)
  file.rename(from_files, to_files)
  unlink(path, recursive = TRUE)
})
## Compressed to folder created
library(zip)
zip::zipr(paste0("data-raw/", basename(path_to), ".zip"), path_to)
unlink(path_to, recursive = TRUE)

# remove everything ------------------------------------------------------------

rm(list = ls())
