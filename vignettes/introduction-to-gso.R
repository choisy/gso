## ---- eval=F-------------------------------------------------------------
#  devtools::install_github("choisy/gso")

## ------------------------------------------------------------------------
library(gso)
library(dplyr) # for data manipulation

## ------------------------------------------------------------------------
head(content)

## ----eval=F--------------------------------------------------------------
#  ?content

## ---- message=FALSE------------------------------------------------------
#install.packages("dplyr")
#install.packages("tidyr")
library(dplyr)
library(tidyr)
glimpse(content)

## ------------------------------------------------------------------------
agri_forest_fish <- filter(content, category == "Agriculture, Forestry and Fishery")
glimpse(agri_forest_fish)

## ------------------------------------------------------------------------
sel <- grep("hiv", content$data_frame, ignore.case = TRUE)
content[sel, ] # permit to print more information
df_hiv <- unnest(content[sel, "data"])

## ---- eval = F-----------------------------------------------------------
#  View(content)

