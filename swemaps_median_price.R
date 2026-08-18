source("fetch_and_tidy_lan_data_api.R")

library(tidyverse)
library(swemaps2)
library(gganimate)
library(gifski)

#swemaps 2 does not use län suffix so we remover it before left_join

brf_map <- county |>
  left_join(
    brf_lan_tidy |> mutate(ln_namn = str_remove(region, " län$")),
    by = "ln_namn"
  )

price_limits <- range(brf_map$median_price, na.rm = TRUE)

#static plots

plot_year <- function(yr) {
  brf_map |>
    filter(year == yr) |>
    ggplot(aes(fill = median_price)) +
    geom_sf() +
    geom_sf_text(aes(label = ln_namn), size = 3, check_overlap = TRUE) +
    scale_fill_viridis_c(limits = price_limits, labels = scales::label_number()) +
    labs(title = paste("Median bostadsrätt price,", yr), fill = "SEK") +
    theme_swemap()
}

#plot looped on year 

for (yr in sort(unique(brf_map$year))) {
  print(plot_year(yr))
}

#animated plot 

county_labels <- county |>
  sf::st_point_on_surface() |>
  sf::st_coordinates() |>
  as_tibble() |>
  mutate(ln_namn = county$ln_namn)

# {closest_state} in the title shows the current year.
anim <- brf_map |>
  ggplot(aes(fill = median_price)) +
  geom_sf() +
  geom_text(data = county_labels, aes(X, Y, label = ln_namn),
            size = 3, inherit.aes = FALSE) +
  scale_fill_viridis_c(limits = price_limits, labels = scales::label_number()) +
  labs(title = "Median bostadsrätt price, {closest_state}", fill = "SEK") +
  theme_swemap() +
  transition_states(year, transition_length = 1, state_length = 2)

animate(anim, fps = 24, width = 700, height = 900, res = 100,
        renderer = gifski_renderer())

anim_save("figs/median_price_by_year.gif")
