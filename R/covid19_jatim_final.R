library(httr)
resp <- GET("https://data.covid19.go.id/public/api/update.json")
status_code (resp)
identical(resp$status_code, status_code(resp))

library(httr)

resp <- GET("https://data.covid19.go.id/public/api/update.json")
cov_id_raw <- content(resp, as = "parsed", simplifyVector = TRUE) 

length(cov_id_raw)
names(cov_id_raw)
cov_id_update <- cov_id_raw$update
lapply(cov_id_update, names)
cov_id_update$penambahan$tanggal
cov_id_update$penambahan$jumlah_sembuh
cov_id_update$penambahan$jumlah_meninggal
cov_id_update$total$jumlah_positif
cov_id_update$total$jumlah_meninggal

resp_jatim <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_TIMUR.json")
cov_jatim_raw <- content(resp_jatim, as = "parsed", simplifyVector = TRUE)

names(cov_jatim_raw)
cov_jatim_raw$kasus_total
cov_jatim_raw$meninggal_persen
cov_jatim_raw$sembuh_persen

cov_jatim <- cov_jatim_raw$list_perkembangan
str(cov_jatim)
head(cov_jatim)

library(dplyr)

new_cov_jatim  <-
  cov_jatim %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )

str(new_cov_jatim)

library(ggplot2)
library(hrbrthemes)

ggplot(new_cov_jatim, aes(x = tanggal, y = kasus_baru)) +
  geom_col()

library(ggplot2)
library(hrbrthemes)

ggplot(new_cov_jatim, aes(tanggal, kasus_baru)) +
  geom_col(fill = "salmon") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Positif COVID-19 di Jawa Timur",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

library(ggplot2)
library(hrbrthemes)

ggplot(new_cov_jatim, aes(tanggal, sembuh)) +
  geom_col(fill = "olivedrab2") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Sembuh Dari COVID-19 di Jawa Timur",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

library(ggplot2)
library(hrbrthemes)

ggplot(new_cov_jatim, aes(tanggal, meninggal)) +
  geom_col(fill = "darkslategray4") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Meninggal Akibat COVID-19 di Jawa Timur",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

library(dplyr)
library(lubridate)

cov_jatim_pekanan <- new_cov_jatim %>% 
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt = kasus_baru,
    name = "jumlah"
  )

glimpse(cov_jatim_pekanan)

library(dplyr)

cov_jatim_pekanan <-
  cov_jatim_pekanan %>% 
  mutate(
    jumlah_pekanlalu = dplyr::lag(jumlah, 1),
    jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
    lebih_baik = jumlah < jumlah_pekanlalu
  )

glimpse(cov_jatim_pekanan)

library(ggplot2)
library(hrbrthemes)

ggplot(cov_jatim_pekanan, aes(pekan_ke, jumlah, fill = lebih_baik)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(breaks = 9:40, expand = c(0, 0)) +
  scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Pekanan Positif COVID-19 di Jawa Timur",
    subtitle = "Kolom hijau menunjukan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

library(dplyr)

cov_jatim_akumulasi <- 
  new_cov_jatim %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )

tail(cov_jatim_akumulasi)

library(ggplot2)

ggplot(data = cov_jatim_akumulasi, aes(x = tanggal, y = akumulasi_aktif)) +
  geom_line()

library(ggplot2)

ggplot(data = cov_jatim_akumulasi, aes(x = tanggal)) +
  geom_line(aes(x = tanggal, y = akumulasi_aktif), color = "blue") +
  geom_line(aes(x = tanggal, y = akumulasi_sembuh), color = "green") +
  geom_line(aes(x = tanggal, y = akumulasi_meninggal), color = "red")

library(dplyr)
library(tidyr)

dim(cov_jatim_akumulasi)

cov_jatim_akumulasi_pivot <- 
  cov_jatim_akumulasi %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>% 
  mutate(
    kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )

dim(cov_jatim_akumulasi_pivot)

glimpse(cov_jatim_akumulasi_pivot)

cov_jatim_akumulasi_pivot <-
  cov_jatim_akumulasi %>%
  pivot_longer(
    cols = -tanggal,
    names_to = "kategori",
    names_prefix = "akumulasi_",
    values_to = "jumlah"
  )

dim(cov_jatim_akumulasi_pivot)

glimpse(cov_jatim_akumulasi_pivot)

library(ggplot2)
library(hrbrthemes)

ggplot(cov_jatim_akumulasi_pivot, aes(tanggal, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ),
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Jawa Timur",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )