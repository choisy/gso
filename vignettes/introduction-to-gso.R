## ---- eval=F-------------------------------------------------------------
#  devtools::install_github("choisy/gso")

## ------------------------------------------------------------------------
library(gso)

## ----eval=F--------------------------------------------------------------
#  write.table(gso::data_frame_summary, "summary.csv", quote = F, sep = ",", row.names = F)
#  write.table(gso::agriculture_1, "agriculture.csv", quote = F, sep = ",", row.names = F)

## ------------------------------------------------------------------------
head(data_frame_summary)
str(data_frame_summary)

## ----eval=F--------------------------------------------------------------
#  ?data_frame_summary

## ---- message=FALSE------------------------------------------------------
library(dplyr)

## ------------------------------------------------------------------------
avail_data <- filter(data_frame_summary, priority == 1)
dim(avail_data)
head(avail_data)

## ------------------------------------------------------------------------
unique(avail_data$category) # gives you the list of category available
agri_forest_fish <- filter(data_frame_summary, category == "Agriculture, Forestry and Fishery")
str(agri_forest_fish)

## ---- eval = F-----------------------------------------------------------
#  View(avail_data)
#  # Call the data frame
#  agriculture_1

## ------------------------------------------------------------------------
head(pop_size)
str(pop_size)

## ------------------------------------------------------------------------
?pop_size

## ------------------------------------------------------------------------
head(agriculture_1)

## ------------------------------------------------------------------------
str(agriculture_1)

