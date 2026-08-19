source("fetch_and_tidy_hemnet.R")

library(tidyverse)
library(swemaps2)
library(gganimate)
library(gifski)

# Two animated kommun maps: apartments only ("Lägenhet") and all housing
# forms combined ("Alla", precomputed at listing level in the fetch script).
# Each gets its own color scale — the price levels differ, and note the
# "Alla" map also reflects each kommun's housing MIX, not just prices.
# hemnet_kommun_map_tidy is keyed by SCB kn_kod, so it joins straight onto
# the swemaps2 kommun polygons. Cells with fewer than 20 sales are dropped
# as too noisy; 290 kommuner is too many for readable labels, so none.

# every kommun x year combination, so kommuner without data keep their
# polygon (NA fill) instead of leaving holes in the map
prep_map <- function(form) {
  form_data <- hemnet_kommun_map_tidy |>
    filter(housing_form == form, n_sales >= 20)
  municipality |>
    cross_join(tibble(year = sort(unique(form_data$year)))) |>
    left_join(form_data, by = c("kn_kod", "kn_namn", "year"))
}

# single static year, for interactive use: plot_year("Lägenhet", 2023)

plot_year <- function(form, yr) {
  kommun_map <- prep_map(form)
  kommun_map |>
    filter(year == yr) |>
    ggplot(aes(fill = median_price)) +
    geom_sf() +
    scale_fill_gradientn(colours = rev(paletteer::paletteer_c("grDevices::Reds 3", 30)),
                         limits = range(kommun_map$median_price, na.rm = TRUE),
                         labels = scales::label_number(),
                         na.value = "grey85") +
    labs(title = paste0("Median price, ", form, " (Hemnet), ", yr),
         fill = "SEK") +
    theme_swemap()
}

#animated plot, one gif per housing form

# {closest_state} in the title shows the current year.
animate_form <- function(form, gif_name) {
  kommun_map <- prep_map(form)
  anim <- kommun_map |>
    ggplot(aes(fill = median_price)) +
    geom_sf() +
    scale_fill_gradientn(colours = rev(paletteer::paletteer_c("grDevices::Reds 3", 30)),
                         limits = range(kommun_map$median_price, na.rm = TRUE),
                         labels = scales::label_number(),
                         na.value = "grey85") +
    labs(title = paste0("Median price, ", form, " (Hemnet), {closest_state}"),
         fill = "SEK") +
    theme_swemap() +
    transition_states(year, transition_length = 1, state_length = 2)

  animate(anim, fps = 24, width = 700, height = 900, res = 100,
          renderer = gifski_renderer())
  anim_save(gif_name)
}

animate_form("Lägenhet", "figs/kommun_median_price_by_year.gif")
animate_form("Alla", "figs/kommun_median_price_all_forms_by_year.gif")
