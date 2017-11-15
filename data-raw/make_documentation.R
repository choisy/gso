library(magrittr)
library(Rd2roxygen)
library(dplyr)
library(roxygen2)

# Prerequisite -----------------------------------------------------------------
load(file = "data/data_frame_summary.rda")


# Functions --------------------------------------------------------------------
# From a data frame, return one vector of length one containing:
# An object of class \\code{data.frame} with `THE DIMENSION OF THE DF`.
# ANd a list of the columns name and the class of each column.
make_format <- function(df){
  paste(roxygen2::object_format(df), "\n \\itemize{",
        lapply(seq_len(dim(df)[2]), function(x) {
          paste("\\item \\code{",names(df)[x], "} ",
                paste0("A column of class : ",
                       class(df[, x]), "."))
        }) %>% paste0(collapse = "\n"), "}")
}

# Load data --------------------------------------------------------------------

list_tab <- dir("data/") %>%
  grep("pop_size|data_frame_summary", ., value = T, invert = T)

cat <- data_frame_summary$category %>% unique
cat_file <- cat %>%
  unlist %>% as.vector() %>%
  tolower() %>%
  gsub("[[:punct:]]", "", .) %>%
  gsub(" ", "_", .)

# Create a Rd file for each category, containing the documentation for each data
# frame.
tot_rd <-  lapply(seq_along(cat), function(y){
  df_tot <- data_frame_summary[which(data_frame_summary$category == cat[y]), ]

  list_cat <- list_tab[which(gsub(".rda", "", list_tab) %in% df_tot$data_name)]

  if(length(list_cat) > 0){
    all_rd <-  lapply(seq_along(list_cat), function(x){
      load(paste0("data/",list_cat[x]))
      df <- get(gsub(".rda", "", list_cat[x]))
      test <- list(title = filter(data_frame_summary,
                                  data_name == gsub(".rda", "",
                                                    list_cat[x])) %>%
                     .$`data frame`,
                   format = make_format(df),
                   desc = paste0("Contains the data expressed : ",
                                 data_frame_summary[which(
                                   data_frame_summary$data_name ==
                                     gsub(".rda", "", list_cat[x])),
                                   "time range"],
                                 ", by ", data_frame_summary[which(
                                   data_frame_summary$data_name ==
                                     gsub(".rda", "", list_cat[x])),
                                   "time resolution"],
                                 " and by ", data_frame_summary[which(
                                   data_frame_summary$data_name ==
                                     gsub(".rda", "", list_cat[x])),
                                   "spatial resolution"], "."),
                   source = "General Statistical Office of Vietnam \n
                   (\\url{http://gso.gov.vn/Default_en.aspx?tabid=491})"
      )
      test <- capture_output(cat(create_roxygen(test), sep = "\n"))
      test <- paste0(test, "\n'", gsub(".rda", "", list_cat[x]) %>% as.character(),
                     "'\n")
    })
    writeLines(unlist(all_rd),con=paste0("R/", cat_file[y], ".R"), sep="\n")
  }
})

# erase everything #############################################################

rm(list = ls())
