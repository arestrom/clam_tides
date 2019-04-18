#================================================================
# Prep and test for shiny tide program
#
# AS 2019-04-11
#================================================================

# Read sites
sites = readRDS("sites.rds")

# filter to only those I need
wa_sites = sites %>%
  filter(Y > 46.21 & Y < 49.1 & X > -124.76 & X < -121.91) %>%
  select(station = Station, lon = X, lat = Y)

# Convert to sf object...no need for time-zone info
wa_sites = st_as_sf(wa_sites, coords = c("lon", "lat"), crs = 4326)

# Get tide correction data
tide_corr = readRDS("www/tide_corr.rds")

# Get rid of unneeded beaches
tide_corr = tide_corr %>%
  mutate(bidn = as.character(bidn)) %>%
  filter(!beach_name == "Unnamed beach") %>%
  select(bidn, beach_name, tide_station, lt_corr)

# Filter to beaches in 2018 flight data
flt_path = glue("C:/data/RStudio/Shiny/flight_proof/gis/2018")
beach_polys = read_sf(glue("{flt_path}/BIDN_2018.shp"))
st_crs(beach_polys)$epsg

# Prep polys
beach_polys = beach_polys %>%
  select(bidn = BIDN, poly_name = name, geometry)

# Get vector of beaches to keep from 2018 beach_polys
beach_keep = unique(beach_polys$bidn)
tide_corr = tide_corr %>%
  filter(bidn %in% beach_keep) %>%
  arrange(beach_name)

# Copy names to beach_polys
beach_names = tide_corr %>%
  select(bidn, beach_name)
beach_polys = beach_polys %>%
  left_join(beach_names, by = "bidn") %>%
  select(bidn, beach_name, geometry) %>%
  arrange(beach_name)

# Pull out names from wa_sites
wa_sites = wa_sites %>%
  mutate(station_name = remisc::get_text_item(station, 1, ",")) %>%
  mutate(bidn = seq(100000, 100000 + nrow(wa_sites) - 1)) %>%
  select(bidn, station_name, geometry)

# Simplify PT
wa_sites$station_name[wa_sites$station_name == "Port Townsend (Point Hudson)"] = "Port Townsend"

# # Output wa_sites to rds
# saveRDS(wa_sites, "www/wa_stations.rds")

# Create wa_beaches with tide corrections and stations
wa_beaches = beach_polys %>%
  left_join(tide_corr, by = c("bidn", "beach_name")) %>%
  select(bidn, beach_name, low_correction = lt_corr,
         station_name = tide_station, geometry)

# Pull out needed data from wa_stations
wa_non_beach = wa_sites %>%
  st_drop_geometry() %>%
  mutate(beach_name = station_name) %>%
  mutate(low_correction = 0L) %>%
  select(bidn, beach_name, low_correction, station_name)

# Add beach predictions high and low
wa_beach = wa_beaches %>%
  st_drop_geometry() %>%
  select(bidn, beach_name, low_correction, station_name)

# Pull out combined locations
beaches_stations = rbind(wa_beach, wa_non_beach)

# Arrange
beaches_stations = beaches_stations %>%
  arrange(beach_name)

# # Output to rds
# saveRDS(wa_beaches, "www/wa_beaches.rds")
# saveRDS(beaches_stations, "www/beaches_stations.rds")

# Prep select inputs
station_list = beaches_stations %>%
  filter(as.integer(bidn) < 200000) %>%
  select(beach_name) %>%
  arrange(beach_name)
station_list = as.list(station_list$beach_name)

# Prep select inputs
beach_list = beaches_stations %>%
  filter(as.integer(bidn) >= 200000) %>%
  select(beach_name) %>%
  arrange(beach_name)
beach_list = as.list(beach_list$beach_name)

# Create selectize list for multiple high-low sites
beach_list = list("Beaches" = beach_list,
                  `Tide stations` = station_list)








