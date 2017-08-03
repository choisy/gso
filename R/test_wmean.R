# test merge weighted mean------------------------------------------------------
test <- get(p_list[33])
pop_size %<>% select(province,year,total)
df <- get(p_list[33]) %>%
  spread_merge_province(FUN = weighted.mean,
                        from = "1980-01-01", to = "2015-12-31",
                        df2 = pop_size, args = "total")

# FROM >= 1992

# Ha Noi
df %>%
  filter(province == "Ha Noi", year == 2007) %>% .$value ==
  weighted.mean(c(
    test %>% filter(province == "Ha Noi", year == 2007) %>% .[,3],
    test %>% filter(province == "Ha Tay", year == 2007) %>% .[,3]),
    c(pop_size %>% filter(province == "Ha Noi", year == 2007) %>% .["total"],
    pop_size %>% filter(province == "Ha Tay", year == 2007) %>% .["total"])
  )

# Can Tho NA
df %>%
  filter(province == "Can Tho", year == 2005) %>% .$value ==
  weighted.mean(c(
    test %>% filter(province == "Can Tho", year == 2005) %>% .[,3],
    test %>% filter(province == "Hau Giang", year == 2005) %>% .[,3]),
    c(pop_size %>% filter(province == "Can Tho", year == 2005) %>% .["total"],
      pop_size %>% filter(province == "Hau Giang", year == 2005) %>% .["total"])
  )

# Dack Lak
df %>%
  filter(province == "Dack Lak", year == 2006) %>% .$value ==
  weighted.mean(c(
    test %>% filter(province == "Dak Lak", year == 2006) %>% .[,3],
    test %>% filter(province == "Dak Nong", year == 2006) %>% .[,3]),
    c(pop_size %>% filter(province == "Dak Lak", year == 2006) %>% .["total"],
      pop_size %>% filter(province == "Dak Nong", year == 2006) %>% .["total"])
  )

# Yen Bai
df %>%
  filter(province == "Yen Bai", year == 2004) %>% .$value ==
  weighted.mean(
    test %>% filter(province == "Yen Bai", year == 2004) %>% .[,3],
    pop_size %>% filter(province == "Yen Bai", year == 2004) %>% .["total"])


# FROM < 1992

# Hau Giang NA
df %>%
  filter(province == "Hau Giang", year == 2002) %>% .$value ==
  weighted.mean(c(
    test %>% filter(province == "Can Tho", year == 2002) %>% .[,3],
    test %>% filter(province == "Soc Trang", year == 2002) %>% .[,3]),
    c(pop_size %>% filter(province == "Can Tho", year == 2002) %>% .["total"],
      pop_size %>% filter(province == "Soc Trang", year == 2002) %>% .["total"])
  )

# Hau Giang NA
df %>%
  filter(province == "Hau Giang", year == 2005) %>% .$value ==
  weighted.mean(c(
    test %>% filter(province == "Can Tho", year == 2005) %>% .[,3],
    test %>% filter(province == "Hau Giang", year == 2005) %>% .[,3],
    test %>% filter(province == "Soc Trang", year == 2005) %>% .[,3]),
    c(pop_size %>% filter(province == "Can Tho", year == 2005) %>% .["total"],
      pop_size %>% filter(province == "Hau Giang", year == 2005) %>% .["total"],
      pop_size %>% filter(province == "Soc Trang", year == 2005) %>% .["total"])
  )

# Ha Noi
df %>%
  filter(province == "Ha Noi", year == 2010) %>% .$value ==
  weighted.mean(c(
    test %>% filter(province == "Ha Noi", year == 2010) %>% .[,3],
    test %>% filter(province == "Hoa Binh", year == 2010) %>% .[,3]),
    c(pop_size %>% filter(province == "Ha Noi", year == 2010) %>% .["total"],
      pop_size %>% filter(province == "Hoa Binh", year == 2010) %>% .["total"]))

# Ha Noi
df %>%
  filter(province == "Ha Noi", year == 2007) %>% .$value ==
  weighted.mean(c(
    test %>% filter(province == "Ha Noi", year == 2007) %>% .[,3],
    test %>% filter(province == "Ha Tay", year == 2007) %>% .[,3],
    test %>% filter(province == "Hoa Binh", year == 2007) %>% .[,3]),
    c(pop_size %>% filter(province == "Ha Noi", year == 2007) %>% .["total"],
      pop_size %>% filter(province == "Ha Tay", year == 2007) %>% .["total"],
      pop_size %>% filter(province == "Hoa Binh", year == 2007) %>% .["total"])
  )

# From <= 1989

# Binh Tri Thien
df %>%
  filter(province == "Binh Tri Thien", year == 2007) %>% .$value ==
  weighted.mean(c(
    test %>% filter(province == "Quang Binh", year == 2007) %>% .[,3],
    test %>% filter(province == "Quang Tri", year == 2007) %>% .[,3],
    test %>% filter(province == "Thua Thien - Hue", year == 2007) %>% .[,3]),
    c(pop_size %>% filter(province == "Quang Binh", year == 2007) %>% .["total"],
      pop_size %>% filter(province == "Quang Tri", year == 2007) %>% .["total"],
      pop_size %>% filter(province == "Thua Thien - Hue", year == 2007) %>%
        .["total"])
  )
