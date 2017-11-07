# Packages: ####################################################################
library(magrittr) # for the " %>% " pipe
library(devtools) # for "use_data"

# Average Population by province: ##############################################
# Loading the file:
pop_size <- read.csv2("data-raw/Population and Employment/Average population by province.csv",skip=1,na.strings="..",as.is=T)
# Excluding the non-interesting rows:
exclude <- c(" WHOLE COUNTRY","Red River Delta","Northern midlands and mountain areas",
             "Northern Central area and Central coastal area","Central Highlands",
             "South East","Mekong River Delta")
pop_size <- pop_size[-grep(paste(exclude,collapse="|"),pop_size[,1]),]
# The provinces:
province <- sub(" +"," ",pop_size[,1])
pop_size <- pop_size[,-1]
#province <- rep(province,ncol(pop_size))
# Separating, male, female, urban and rural:
var_names <- names(pop_size)
fct <- function(keyword) {
  df <- pop_size[,grep(keyword,var_names)]
  year <- as.numeric(gsub("[^0-9]","",names(df)))
  setNames(data.frame(rep(year,each=nrow(df)),
                      rep(province,ncol(df)),
                      1000*as.numeric(unname(unlist(df))),stringsAsFactors=F),
           c("year","province",tolower(keyword)))
}
pop_size <- lapply(c("Total","Male","Female","Urban","Rural"),fct)
pop_size <- Reduce(function(x,y)merge(x,y,all=T),pop_size)
# Renaming some provinces
conversion <- matrix(c(#"Ba Ria - Vung Tau", "Vung Tau - Ba Ria",
                       #"Ha Noi",            "Hanoi",
                       "Ho Chi Minh city",  "Ho Chi Minh",
                       "Thua Thien-Hue",    "Thua Thien - Hue"),ncol=2,byrow=T)
fct <- function(x,tab) {
  for(i in 1:nrow(tab)) x <- sub(tab[i,1],tab[i,2],x)
  x
}
pop_size$province <- fct(pop_size$province,conversion)
# Passing to integers:
for(i in c(1,3:7)) pop_size[,i] <- as.integer(pop_size[,i])


# Saving to disk: ##############################################################
#devtools::use_data(pop_size,overwrite=T)
# Done in "data_creation_province"

# Cleaning: ####################################################################
rm(list=ls())
