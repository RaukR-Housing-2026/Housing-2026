library(pxweb)
library(tidyverse)

url_salary <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0110/HE0110I/Tab1InkDesoRegso"

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

median_salary <- "0000089U"
medel_salary <- "0000089T"
antal_person <- "0000089O"
alla_ar <- "*"

query_salary <- list(
  Region       = unname(lan),
  Kon = c("1", "2", "1+2"),
  InkomstTyp = c("SaFörInk", "NeInk"),
  ContentsCode = c(antal_person, medel_salary, median_salary),
  Tid          = alla_ar
)

salary_lan <- pxweb_get(url_salary, query_salary) |>
  as.data.frame(column.name.type = "text", variable.value.type = "text")

#Tidy the data
salary_lan_tidy <- salary_lan |>
  rename(
    region       = region,
    year         = år,
    n_persons      = `Antal personer totalt`,
    mean_salary   = `Medelvärde, tkr`,
    median_salary = `Medianvärde, tkr`,
    sex = `kön`,
    income = `inkomstslag`
  ) |>
  mutate(
    year = as.integer(year),
    mean_salary   = mean_salary * 1000,   # tkr -> SEK
    median_salary = median_salary * 1000,
    sex = case_when(sex == "män" ~ "men",
                    sex == "kvinnor" ~ "women",
                    sex == "totalt" ~ "total"),
    income = case_when(income == "nettoinkomst" ~ "netto_income",
                       income == "sammanräknad förvärvsinkomst" ~ "total_earned_income") # after and before tax 
  ) |>
  separate(
    region,
    into = c("region_code", "region"),
    sep = " ",
    extra = "merge"
  ) |> 
  arrange(region, year)

#removes all unessecary varialbes used for the quary to create the correct dataframe
rm(alla_ar, antal_person, medel_salary, median_salary, lan, query_salary)

  