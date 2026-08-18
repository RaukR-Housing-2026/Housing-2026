library(pxweb)
library(tidyverse)



# SCB BO0501C — Försäljning av bostadsrätter efter län och år
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BO__BO0501__BO0501C/FastprisBRFRegionAr/
url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0501/BO0501C/FastprisBRFRegionAr"



lan <- c(
  stockholm       = "01",
  uppsala         = "03",
  sodermanland    = "04",
  ostergotland    = "05",
  jonkoping       = "06",
  kronoberg       = "07",
  kalmar          = "08",
  gotland         = "09",
  blekinge        = "10",
  skane           = "12",
  halland         = "13",
  vastra_gotaland = "14",
  varmland        = "17",
  orebro          = "18",
  vastmanland     = "19",
  dalarna         = "20",
  gavleborg       = "21",
  vasternorrland  = "22",
  jamtland        = "23",
  vasterbotten    = "24",
  norrbotten      = "25"
)

antal      <- "BO0501R6"
medelpris  <- "BO0501R7"  # i tkr
medianpris <- "BO0501R8"  # i tkr
alla_ar    <- "*"

query <- list(
  Region       = unname(lan),
  ContentsCode = c(antal, medelpris, medianpris),
  Tid          = alla_ar
)

brf_lan <- pxweb_get(url, query) |>
  as.data.frame(column.name.type = "text", variable.value.type = "text")

brf_lan



# Tidy: snake_case English names, year as integer, prices in full SEK
brf_lan_tidy <- brf_lan |>
  rename(
    region       = region,
    year         = år,
    n_sales      = `Antal`,
    mean_price   = `Medelpris i tkr`,
    median_price = `Medianpris i tkr`
  ) |>
  mutate(
    year = as.integer(year),
    mean_price   = mean_price * 1000,   # tkr -> SEK
    median_price = median_price * 1000
  )

brf_lan_tidy