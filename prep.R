#================================================================
# Prep and test for shiny tide program
#
# AS 2019-04-11
#================================================================

# Read sites
sites = readRDS("www/sites.rds")

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

# # Output wa_beaches to rds
# saveRDS(wa_beaches, "www/wa_beaches.rds")
