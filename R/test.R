# test merge sum ---------------------------------------------------------------
test <- get(p_list[88])
df <- get(p_list[88]) %>%
  spread_merge_province(FUN = sum,
                        from = "1980-01-01", to = "2015-12-31")

# FROM >= 1992

# Ha Noi
df %>%
  filter(province == "Ha Noi", year == 2007, key == "Total") %>% .$value ==
  test %>% filter(province == "Ha Noi", year == 2007) %>% .["Total"] +
  test %>% filter(province == "Ha Tay", year == 2007) %>% .["Total"]

# Can Tho
df %>%
  filter(province == "Can Tho", year == 2005, key == "Total") %>% .$value ==
  test %>% filter(province == "Can Tho", year == 2005) %>% .["Total"] +
  test %>% filter(province == "Hau Giang", year == 2005) %>% .["Total"]

# Dack Lak
df %>%
  filter(province == "Dack Lak", year == 2006, key == "Total") %>% .$value ==
  test %>% filter(province == "Dak Lak", year == 2006) %>% .["Total"] +
  test %>% filter(province == "Dak Nong", year == 2006) %>% .["Total"]

# Yen Bai
df %>%
  filter(province == "Yen Bai", year == 2004, key == "Total") %>% .$value ==
  test %>% filter(province == "Yen Bai", year == 2004) %>% .["Total"]


# FROM < 1992

# Hau Giang
df %>%
  filter(province == "Hau Giang", year == 2002, key == "Total") %>% .$value ==
  test %>% filter(province == "Can Tho", year == 2002) %>% .["Total"] +
  test %>% filter(province == "Soc Trang", year == 2002) %>% .["Total"]

# Hau Giang
df %>%
  filter(province == "Hau Giang", year == 2005, key == "Total") %>% .$value ==
  test %>% filter(province == "Can Tho", year == 2005) %>% .["Total"] +
  test %>% filter(province == "Soc Trang", year == 2005) %>% .["Total"] +
  test %>% filter(province == "Hau Giang", year == 2005) %>% .["Total"]

# Ha Noi
df %>%
  filter(province == "Ha Noi", year == 2010, key == "Total") %>% .$value ==
  test %>% filter(province == "Ha Noi", year == 2010) %>% .["Total"] +
  test %>% filter(province == "Hoa Binh", year == 2010) %>% .["Total"]

# Ha Noi
df %>%
  filter(province == "Ha Noi", year == 2007, key == "Total") %>% .$value ==
  test %>% filter(province == "Ha Noi", year == 2007) %>% .["Total"] +
  test %>% filter(province == "Ha Tay", year == 2007) %>% .["Total"] +
  test %>% filter(province == "Hoa Binh", year == 2007) %>% .["Total"]


# From <= 1989

# Binh Tri Thien
df %>%
  filter(province == "Binh Tri Thien", year == 2007, key == "Total") %>%
  .$value ==
  test %>% filter(province == "Quang Binh", year == 2007) %>% .["Total"] +
  test %>% filter(province == "Quang Tri", year == 2007) %>% .["Total"] +
  test %>% filter(province == "Thua Thien - Hue", year == 2007) %>% .["Total"]



# test NA-----------------------------------------------------------------------
test <- get(p_list[33])
df <- get(p_list[33]) %>%
  spread_merge_province(FUN = sum,
                        from = "1980-01-01", to = "2015-12-31")

testthat::expect_identical(df %>%
  filter(province == "Hau Giang", year > 1994) %>%
  select(value) %>% unlist %>% as.vector,
  c(66.6, 62.6, 58.1, 55.3,rep(NA, 17)))

# test provinces names ---------------------------------------------------------


testthat::expect_equal(
  mean(df$province %in%
         dictionary::province_year$`1979` %>% unique), 1)


