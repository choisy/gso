test <- get(p_list[88])
df <- get(p_list[88]) %>%
  spread_merge_province(FUN = sum,
                        from = "1992-01-01", to = "2010-12-31",
                        df2 = pop_size, args = NULL)

# FROM >= 1992 ------------------------------------------------------------------

# Ha Noi
df %>% filter(province == "Ha Noi", year == 2007, key == "Total") %>% .$value ==
  test %>% filter(province == "Ha Noi", year == 2007) %>% .["Total"] +
  test %>% filter(province == "Ha Tay", year == 2007) %>% .["Total"]

# Can Tho
df %>% filter(province == "Can Tho", year == 2005, key == "Total") %>% .$value ==
  test %>% filter(province == "Can Tho", year == 2005) %>% .["Total"] +
  test %>% filter(province == "Hau Giang", year == 2005) %>% .["Total"]

# Dack Lak
df %>% filter(province == "Dack Lak", year == 2006, key == "Total") %>% .$value ==
  test %>% filter(province == "Dak Lak", year == 2006) %>% .["Total"] +
  test %>% filter(province == "Dak Nong", year == 2006) %>% .["Total"]

# Yen Bai
df %>% filter(province == "Yen Bai", year == 2004, key == "Total") %>% .$value ==
  test %>% filter(province == "Yen Bai", year == 2004) %>% .["Total"]


# FROM < 1992 ------------------------------------------------------------------

# Hau Giang
df %>% filter(province == "Hau Giang", year == 2002, key == "Total") %>% .$value ==
  test %>% filter(province == "Can Tho", year == 2002) %>% .["Total"] +
  test %>% filter(province == "Soc Trang", year == 2002) %>% .["Total"]

# Hau Giang
df %>% filter(province == "Hau Giang", year == 2005, key == "Total") %>% .$value ==
  test %>% filter(province == "Can Tho", year == 2005) %>% .["Total"] +
  test %>% filter(province == "Soc Trang", year == 2005) %>% .["Total"] +
  test %>% filter(province == "Hau Giang", year == 2005) %>% .["Total"]

# Ha Noi
df %>% filter(province == "Ha Noi", year == 2010, key == "Total") %>% .$value ==
  test %>% filter(province == "Ha Noi", year == 2010) %>% .["Total"] +
  test %>% filter(province == "Hoa Binh", year == 2010) %>% .["Total"]

# Ha Noi
df %>% filter(province == "Ha Noi", year == 2007, key == "Total") %>% .$value ==
  test %>% filter(province == "Ha Noi", year == 2007) %>% .["Total"] +
  test %>% filter(province == "Ha Tay", year == 2007) %>% .["Total"] +
  test %>% filter(province == "Hoa Binh", year == 2007) %>% .["Total"]


# From <= 1989 -----------------------------------------------------------------

# Binh Tri Tien
df %>% filter(province == "Binh Tri Tien", year == 2007, key == "Total") %>% .$value ==
  test %>% filter(province == "Quang Binh", year == 2007) %>% .["Total"] +
  test %>% filter(province == "Quang Tri", year == 2007) %>% .["Total"] +
  test %>% filter(province == "Thua Thien - Hue", year == 2007) %>% .["Total"]


# test provinces names ---------------------------------------------------------

prov92 <- c("An Giang", "Ba Ria - Vung Tau", "Bac Thai", "Ben Tre", "Binh Dinh",
            "Binh Thuan", "Can Tho", "Cao Bang", "Dack Lak", "Dong Nai", "Dong Thap",
            "Gia Lai", "Ha Bac", "Ha Giang", "Ha Noi", "Ha Tinh", "Hai Hung",
            "Hai Phong", "Ho Chi Minh", "Hoa Binh", "Khanh Hoa",
            "Kien Giang", "Kon Tum", "Lai Chau", "Lam Dong", "Lang Son", "Lao Cai",
            "Long An",  "Minh Hai", "Nam Ha", "Nghe An", "Ninh Binh", "Ninh Thuan",
            "Phu Yen", "Quang Binh", "Quang Nam - Da Nang", "Quang Ngai",
            "Quang Ninh", "Quang Tri", "Soc Trang", "Son La", "Song Be", "Tay Ninh",
            "Thai Binh", "Thanh Hoa", "Thua Thien - Hue", "Tien Giang", "Tra Vinh",
            "Tuyen Quang", "Vinh Long", "Vinh Phu", "Yen Bai")

prov04 <- c("An Giang", "Ba Ria - Vung Tau", "Bac Giang", "Bac Kan", "Bac Lieu",
            "Bac Ninh", "Ben Tre",  "Binh Dinh", "Binh Duong", "Binh Phuoc",
            "Binh Thuan", "Ca Mau", "Can Tho", "Cao Bang", "Da Nang", "Dak Lak",
            "Dak Nong", "Dien Bien", "Dong Nai", "Dong Thap", "Gia Lai",
            "Ha Giang", "Ha Nam", "Ha Noi", "Ha Tay", "Ha Tinh", "Hai Duong",
            "Hai Phong", "Hau Giang", "Ho Chi Minh", "Hoa Binh", "Hung Yen",
            "Khanh Hoa", "Kien Giang", "Kon Tum", "Lai Chau", "Lam Dong",
            "Lang Son", "Lao Cai", "Long An", "Nam Dinh", "Nghe An",
            "Ninh Binh", "Ninh Thuan", "Phu Tho", "Phu Yen", "Quang Binh",
            "Quang Nam", "Quang Ngai", "Quang Ninh", "Quang Tri", "Soc Trang",
            "Son La", "Tay Ninh", "Thai Binh", "Thai Nguyen", "Thanh Hoa",
            "Thua Thien - Hue", "Tien Giang", "Tra Vinh", "Tuyen Quang",
            "Vinh Long", "Vinh Phuc", "Yen Bai")

testthat::expect_equal(
  mean(df$province %in%
         c("Dack Lak",prov92 %>% unique)), 1)


