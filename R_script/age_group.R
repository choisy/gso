library(tabulizer)
library(tidyr)
library(purrr)

all <- extract_tables("age_group_province.pdf", method = "decide")
text <- extract_text("age_group_province.pdf", 
                     area = list(c(81.60, 66.52, 166.79, 546.24))) %>% 
  gsub("[[:digit:]]*", "", .) %>% gsub("\n*", "", .) %>% strsplit(",") %>% 
  purrr::map(1) %>% 
  stringr::str_extract("(?<=\\.).{1,}") %>% 
  gsub("[[:punct:]]*", "", .) %>% 
  gsub("^ ", "", .) %>% 
  tolower 

transl_test <- data_frame(origin = text) %>% 
  mutate(transl_or = dictionary::vn_province[origin]) 
 
vect_or <- transl_test %>% filter(is.na(transl_or) == TRUE) %>% .$origin %>% na.omit
vect_tranls <- c("hanoi", "ha giang", "cao bang", "tuyen quang", "lao cai", 
                 "lai chau", "son la", "yen bai", "hoa binh", "thai nguyen",
                 "lang son", "quang ninh", "bac giang", "phu tho", "hai duong",
                 "hai phong", "hung yen", "thanh hoa", "nghe an", "quang binh",
                 "quang tri", "da nang", "quang nam", "quang ngai", "phu yen",
                 "khanh hoa", "kon tum", "gia lai", "dak lak", "dak nong",
                 "lam dong", "binh phuoc", "tay ninh", "binh duong", "dong nai",
                 "ba ria vung tau", "ho chi minh", "long an", "tien giang",
                 "vinh long", "dong thap", "an giang", "kien giang", "can tho",
                 "hau giang", "soc trang", "bac lieu") %>% 
  dictionary::vn_province[.]
hash <- dictionary::vn_province %>% c(setNames(vect_tranls, vect_or))
transl_test %<>% mutate(transl_or = hash[origin]) %>% 
  fill(transl_or) %>% 
  mutate(number = rep(c(1, 2), length(text) / 2),
         transl_or = paste(transl_or, number, sep = "_"))

all_tables <-  setNames(all, transl_test$transl_or) %>% 
  map(as.data.frame) %>% 
  map(filter, grepl("Total", V1) == FALSE)

make_colnames <- function(df){
  year <- 2014 + (dim(df)[2] - 2)
  colname_v <- c("age", 2014:year)
  colnames(df) <- colname_v
  df <- df[-1, ]
}

all_tables_2 <- keep(all_tables, names(all_tables) %>% grepl(2, .))  %>%
  map(make_colnames) 
all_tables_1 <- keep(all_tables, names(all_tables) %>% grepl(1, .)) %>% 
  map(separate, V1, c("V1", "2014", "2014d"), sep = " ") %>% 
  map(unite, "V2a", "2014", "2014d", sep = " ")  %>%
  map(make_colnames) 

total <- 
  append(all_tables_1, all_tables_2) %>% 
  map(mutate_all, funs(gsub("NA NA", NA, .))) %>% 
  tibble(province = c(names(all_tables_1), names(all_tables_2))) %>% 
  mutate(province = gsub("_.$", "", province)) %>% 
  group_by(province)  %>% 
  nest %>%
  mutate(data = map(data, unlist, FALSE) %>% 
           map(bind_rows)) %>% 
  unnest %>% 
  mutate(key = age %>% gsub("[[:digit:]]|[[:punct:]]", NA, .)) %>% 
  fill(key) %>% 
  tidyr::gather(year, value, -province, -age, -key) %>% 
  mutate(value = gsub(" ", "", value) %>% as.numeric) %>% 
  filter(is.na(value) == FALSE)

write.csv(total, file = "~/Desktop/vn_agegroup.csv", sep = ";", row.names = FALSE)



  