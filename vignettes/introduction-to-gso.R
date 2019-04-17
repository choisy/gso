## ---- eval=F-------------------------------------------------------------
#  devtools::install_github("choisy/gso")

## ------------------------------------------------------------------------
library(gso)

## ------------------------------------------------------------------------
head(content)

## ----eval=F--------------------------------------------------------------
#  ?content

## ------------------------------------------------------------------------
agri_forest_fish <- subset(content, category == "Agriculture, Forestry and Fishery", data)
agri_forest_fish <- agri_forest_fish$data[[1]]
str(agri_forest_fish)

## ------------------------------------------------------------------------
sel <- grep("hiv", content$data_frame, ignore.case = TRUE)
content[sel, ] # permit to print more information
df_hiv <- content[sel, "data"]
df_hiv <- df_hiv$data[[1]]
head(df_hiv)

## ---- eval = F-----------------------------------------------------------
#  View(content)

