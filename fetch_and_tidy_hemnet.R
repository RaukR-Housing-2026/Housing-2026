library(tidyverse)
library(swemaps2)

# Hemnet sold listings scrape (~1.1M rows), KTH ID2223 student project
# https://github.com/pierrelefevre/hempriser
#
# Downloads the raw files once into data-raw/ (gitignored), pinned to the
# commit that added them, and tidies them into ONE exported data frame:
#
#   hemnet_kommun_map_tidy — n_sales / mean_price / median_price (SEK) per
#   kommun x housing_form x year (2012-2023), keyed by SCB's kn_kod so it
#   joins straight onto the swemaps2 kommun polygons:
#   municipality |> left_join(..., by = c("kn_kod", "kn_namn"))
#   housing_form "Alla" rows hold all forms combined.
#
# Everything else is an intermediate and gets rm()'d at the end — see the
# note there if a downstream script ever needs one of them.

sha      <- "f4cc34f8c7f34dd30f6f1465dd8f13ee31d5d9ad"
base_url <- paste0("https://raw.githubusercontent.com/pierrelefevre/hempriser/",
                   sha, "/dataset/raw")
raw_dir  <- "data-raw/hemnet"

hemnet_files <- c(
  "listings.csv.gz",               # main table, no coordinates
  "listings_with_location.csv.gz", # subset with lat/long (WGS84)
  "locations.csv.gz",              # lookup decoding the numeric location ids
  "inflation.csv.gz"               # monthly Swedish CPI since 1980
)

# Download once, skip files already on disk (listings.csv.gz is ~73 MB)
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
options(timeout = max(600, getOption("timeout")))

for (f in hemnet_files) {
  dest <- file.path(raw_dir, f)
  if (!file.exists(dest)) {
    download.file(paste0(base_url, "/", f), dest, mode = "wb")
  }
}

# Location lookup: id -> named kommun/län/city/district/street
hemnet_locations_tidy <- read_csv(
  file.path(raw_dir, "locations.csv.gz"),
  col_types = cols(
    id             = col_integer(),
    fullName       = col_character(),
    parentFullName = col_character(),
    type           = col_character()
  )
) |>
  rename(
    location_id      = id,
    full_name        = fullName,
    parent_full_name = parentFullName
  )

kommun_names <- hemnet_locations_tidy |>
  filter(type == "MUNICIPALITY") |>
  select(municipality_id = location_id, municipality = full_name)

lan_names <- hemnet_locations_tidy |>
  filter(type == "COUNTY") |>
  select(county_id = location_id, county = full_name)

# Shared tidying for both listings files: snake_case English names, prices in
# SEK, and named kommun/län joined in from the lookup. The names are Hemnet's
# own, genitive style ("Göteborgs kommun") — they do NOT match SCB/swemaps2
# kommun names ("Göteborg"); the crosswalk below solves that. housing_form
# stays Swedish ("Lägenhet", "Villa", ...).
tidy_listings <- function(listings) {
  listings |>
    rename(
      district_id         = district,
      municipality_id     = municipality,
      county_id           = county,
      city_id             = city,
      listing_id          = id,
      final_price         = finalPrice,
      asking_price        = askingPrice,
      living_area         = livingArea,
      sold_at             = soldAt,
      construction_year   = constructionYear,
      renovation_year     = renovationYear,
      running_costs       = runningCosts,
      housing_form        = housingForm,
      has_elevator        = hasElevator,
      has_balcony         = hasBalcony,
      housing_cooperative = housingCooperative
    ) |>
    mutate(
      fee           = na_if(fee, 0),           # 0 = not applicable
      running_costs = na_if(running_costs, 0), # 0 = not applicable
      sold_year     = as.integer(year(sold_at))
    ) |>
    left_join(kommun_names, by = "municipality_id") |>
    left_join(lan_names, by = "county_id")
}

# Main listings table (lat/long/predicted are empty here, so skipped)
hemnet_listings_tidy <- read_csv(
  file.path(raw_dir, "listings.csv.gz"),
  col_types = cols(
    `_id`              = col_skip(),
    url                = col_character(),
    district           = col_integer(),
    municipality       = col_integer(),
    county             = col_integer(),
    city               = col_integer(),
    id                 = col_character(),  # some ids are 19 digits, too big for integer/double
    finalPrice         = col_double(),
    askingPrice        = col_double(),
    fee                = col_double(),
    livingArea         = col_double(),
    rooms              = col_double(),
    soldAt             = col_datetime(),
    constructionYear   = col_integer(),
    renovationYear     = col_integer(),
    runningCosts       = col_double(),
    housingForm        = col_character(),
    hasElevator        = col_logical(),
    hasBalcony         = col_logical(),
    createdAt          = col_datetime(),
    housingCooperative = col_character(),
    lat                = col_skip(),
    long               = col_skip(),
    predicted          = col_skip()
  )
) |>
  rename(created_at = createdAt) |>
  tidy_listings()

# Subset with coordinates (WGS84), for the spatial join below
hemnet_listings_geo_tidy <- read_csv(
  file.path(raw_dir, "listings_with_location.csv.gz"),
  col_types = cols(
    url                = col_character(),
    district           = col_integer(),
    municipality       = col_integer(),
    county             = col_integer(),
    city               = col_integer(),
    id                 = col_character(),  # some ids are 19 digits, too big for integer/double
    finalPrice         = col_double(),
    askingPrice        = col_double(),
    fee                = col_double(),
    livingArea         = col_double(),
    rooms              = col_double(),
    soldAt             = col_datetime(),
    constructionYear   = col_integer(),
    renovationYear     = col_integer(),
    runningCosts       = col_double(),
    housingForm        = col_character(),
    hasElevator        = col_logical(),
    hasBalcony         = col_logical(),
    lat                = col_double(),
    long               = col_double(),
    housingCooperative = col_character()
  )
) |>
  tidy_listings()

# The exported table is built in three steps:
#   1. spatially join the geocoded listings (~36%) onto the kommun polygons
#   2. learn a crosswalk Hemnet municipality_id -> SCB kn_kod from step 1 by
#      majority vote (>99% unanimous in the median kommun). Hemnet's ids
#      share no key with SCB's codes and the genitive names don't match, so
#      the geometry teaches us the translation once
#   3. assign every listing its kommun via the crosswalk — one rule for all,
#      trusting Hemnet's own kommun label — and aggregate
# Years before 2012 are dropped (~18 sales, plus scrape-noise dates to 1900).

# 1. spatial join (~7% of points are waterfront homes just outside the
# generalized coastline polygons — they simply don't get a vote in step 2)
listings_pts <- hemnet_listings_geo_tidy |>
  filter(sold_year >= 2012) |>
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  sf::st_transform(sf::st_crs(municipality)) |>
  sf::st_join(municipality |> select(kn_kod, kn_namn))

# 2. majority-vote crosswalk
kommun_xwalk <- listings_pts |>
  sf::st_drop_geometry() |>
  filter(!is.na(kn_kod)) |>
  count(municipality_id, kn_kod, kn_namn) |>
  slice_max(n, n = 1, by = municipality_id, with_ties = FALSE) |>
  select(municipality_id, kn_kod, kn_namn)

# 3. the export. The "Alla" rows (all forms combined) are computed from the
# listing level too — a median of the per-form medians would be wrong.
# Hemnet covers advertised sales only, so levels sit below SCB's registry
# figures — compare trends, not absolute counts, and mind n_sales: small
# kommun x form x year cells are noisy.
kommun_sales <- hemnet_listings_tidy |>
  filter(sold_year >= 2012) |>
  left_join(kommun_xwalk, by = "municipality_id")

hemnet_kommun_map_tidy <- bind_rows(
  kommun_sales |>
    summarise(
      n_sales      = n(),
      mean_price   = mean(final_price, na.rm = TRUE),
      median_price = median(final_price, na.rm = TRUE),
      .by = c(kn_kod, kn_namn, housing_form, sold_year)
    ),
  kommun_sales |>
    summarise(
      n_sales      = n(),
      mean_price   = mean(final_price, na.rm = TRUE),
      median_price = median(final_price, na.rm = TRUE),
      .by = c(kn_kod, kn_namn, sold_year)
    ) |>
    mutate(housing_form = "Alla")
) |>
  rename(year = sold_year) |>
  arrange(kn_kod, housing_form, year)

#removes all unessecary variables used to download and tidy the data — the
#script exports a single df: hemnet_kommun_map_tidy. Trim this rm() if a
#downstream script ever needs an intermediate, e.g. hemnet_listings_tidy for
#listing-level detail or hemnet_listings_geo_tidy for coordinates. The raw
#inflation.csv.gz (monthly Swedish CPI since 1980) is downloaded but not
#tidied here — read it from data-raw/ if prices ever need deflating.
rm(sha, base_url, raw_dir, hemnet_files, f, dest, hemnet_locations_tidy,
   kommun_names, lan_names, tidy_listings, hemnet_listings_tidy,
   hemnet_listings_geo_tidy, listings_pts, kommun_xwalk, kommun_sales)
