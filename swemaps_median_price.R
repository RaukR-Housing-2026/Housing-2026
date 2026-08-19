source("fetch_and_tidy_lan_data_api.R")

library(tidyverse)
library(swemaps2)
library(gganimate)
library(gifski)
library(paletteer)

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
    scale_fill_gradientn(colours = rev(paletteer::paletteer_c("grDevices::Reds 3", 30)),
                         limits = price_limits, labels = scales::label_number(),
                         na.value = "grey85") +
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

# big year "counter" to the right of the map — one row per year, so
# transition_states() swaps the label in sync with the map frames
bb <- sf::st_bbox(county)
year_label <- tibble(
  year = sort(unique(brf_map$year)),
  X = bb[["xmax"]] + 0.32 * (bb[["xmax"]] - bb[["xmin"]]),
  Y = (bb[["ymin"]] + bb[["ymax"]]) / 2
)

anim <- brf_map |>
  ggplot(aes(fill = median_price)) +
  geom_sf() +
  geom_text(data = county_labels, aes(X, Y, label = ln_namn),
            size = 3, inherit.aes = FALSE) +
  geom_text(data = year_label, aes(X, Y, label = year),
            size = 24, fontface = "bold", colour = "grey25",
            inherit.aes = FALSE) +
  expand_limits(x = bb[["xmax"]] + 0.62 * (bb[["xmax"]] - bb[["xmin"]])) +
  scale_fill_gradientn(colours = rev(paletteer::paletteer_c("grDevices::Reds 3", 30)),
                       limits = price_limits, labels = scales::label_number(),
                       na.value = "grey85") +
  labs(title = "Median bostadsrätt price", fill = "SEK") +
  theme_swemap() +
  # transparent canvas so the gif floats on the site's photo background
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA)) +
  transition_states(year, transition_length = 1, state_length = 2)

# wider canvas to make room for the year counter on the right
animate(anim, fps = 24, width = 1050, height = 900, res = 100,
        renderer = gifski_renderer(), bg = "transparent")

anim_save("figs/median_price_by_year.gif")
