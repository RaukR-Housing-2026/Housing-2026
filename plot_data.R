library(swemaps2)
library(rnaturalearth)
library(rnaturalearthhires)
library(stringr)
library(sf)
library(ggplot2)
library(magrittr)

df <- source("fetch_and_tidy_lan_data_api.R", local = TRUE)$value
fish_df <- source("fetch_and_tidy_fishing_data.R", local = TRUE)$value



df <- df %>%
    rename(län = region) %>% 
    full_join(swemaps2::county %>%
                    mutate(län = ln_namn %>% str_c(" län"),
               .keep = 'unused')) %>%
    full_join(fish_df, join_by(ln_kod == region_code, year) )

neighbours <-
    ne_countries(
        country=c('denmark','finland','norway'),
        scale = 'large',
        returnclass = 'sf'
        ) %>%
    st_transform(3006) # Transform to Swedish standard.

## Crop to Swedish surroundings
neighbours <- neighbours %>%
    st_transform(st_crs(swemaps2::county)) %>%
    st_crop(st_bbox(swemaps2::county)  %>% 
    st_as_sfc() )


### Define water negatively (as not land)
land <-
    st_union(
        bind_rows(
            st_geometry(swemaps2::county) %>% st_sf(),
            st_geometry(neighbours) %>% st_sf()
        )
    )
water <-
    st_difference(
        st_as_sfc(
            st_bbox(swemaps2::county) ),
        land )



map <- ggplot() +
    geom_sf(data = df, aes(geometry=geometry, fill=median_price) ) +
    geom_sf(data = neighbours, fill='grey' ) +
    geom_sf(data = water, fill='blue3' ) +
    swemaps2::theme_swemap()
