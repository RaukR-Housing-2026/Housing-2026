library(pxweb)
library(tidyverse)

# SCB JO1104A — "Antal dagar i tusental som fritidsfiske bedrivits efter område, tabellinnehåll och år"
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__JO__JO1104__JO1104A/F1ny1/
url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/JO/JO1104/JO1104A/F1ny1"

query <- list(
  Omrade2      = as.character(21:32),  # skip 'total'
  ContentsCode = "*",
  Tid          = "*"
)

fisk <- pxweb_get(url, query) |>
  as.data.frame(column.name.type = "text", variable.value.type = "code")

# ---- Hand-rolled crosswalk: ICES-delområde -> angränsande svenska län ----
# Not an official SCB mapping. Catch is split evenly across adjacent counties.
crosswalk <- tribble(
  ~area_code, ~area_name,        ~region_code,
    "21",  "Kattegatt",            c(halland = "13", vastra_gotaland = "14"),
    "22",  "Bälten",               c(skane = "12"),
    "23",  "Öresund",              c(skane = "12"),
    "24",  "Arkonahavet",          c(skane = "12"),
    "25",  "Bornholmshavet",       c(skane = "12", blekinge = "10"),
    "26",  "Östra Gotlandshavet",  c(gotland = "09", kalmar = "08"),
    "27",  "Västra Gotlandshavet", c(kalmar = "08", gotland = "09", ostergotland = "05"),
    "28",  "Gotlandsdjupet",       c(gotland = "09"),
    "29",  "Ålands hav/Skärgårdshavet", c(stockholm = "01", uppsala = "03",
      sodermanland = "04", gotland = "09"),
    "30",  "Bottenhavet",          c(gavleborg = "21", vasternorrland = "22", uppsala = "03"),
    "31",  "Bottenviken",          c(vasterbotten = "24", norrbotten = "25")
      # 32 = Finska viken — inget svenskt län
) |>
mutate(region_name = map(region_code, names)) |>
unnest(c(region_code, region_name))

lan_alla <- c("01","03","04","05","06","07","08","09","10","12","13",
              "14","17","18","19","20","21","22","23","24","25")

lan_inland <- c("06", "07", "17", "18", "19", "20", "23")

fisk_tidy <- fisk |>
  rename(area_code = 1, year = 2) |>
  mutate(year = as.integer(year))

fisk_lan <- fisk_tidy %>% ( function(df) {
    
  num_cols  <- setdiff(names(df)[map_lgl(df, is.numeric)], "year")
  
  df %>%
    inner_join(crosswalk, by = "area_code", relationship = "many-to-many") %>%
    mutate(across(all_of(num_cols), ~ .x / n()), .by = c(area_code, year)) |>
    summarise(across(all_of(num_cols), ~ sum(.x, na.rm = TRUE)), .by = c(area_code, year, region_code, region_name)) |>
    complete(region_code = lan_alla, year) |>
    arrange(region_code, year) } )
