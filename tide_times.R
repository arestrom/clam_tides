#===============================================================================
# Get tide times from shellfish_archive tides_two table
#
# Needed tide times to two decimals. Will update other DBs later
#
# AS 2019-03-07
#===============================================================================

# Clear workspace
rm(list=ls(all=TRUE))

# Libraries
library(dplyr)
library(DBI)
library(odbc)
library(glue)
library(sf)
library(stringi)
library(lubridate)
library(mapview)
library(maptools)

#======================================================================================
# Set global variables and options
#======================================================================================

# Keep connections pane from opening
options("connectionObserver" = NULL)

# Set options
options(digits=14)

#======================================================================================
# Define functions
#======================================================================================

# Function to convert tide time to minutes
hm_to_min = function(x) {
  hr = as.integer(substr(x, 1, 2))
  hr = hr * 60
  mn = as.integer(substr(x, 4, 5))
  mins = hr + mn
  mins
}

#===========================================================
# Tide data ----
#===========================================================

# Get tide times data
qry = glue("select distinct t.low_tide_datetime as tide_datetime, pl.location_name as tide_station, ",
           "t.tide_time_minutes as tide_time, t.tide_height_feet as tide_height, ",
           "ts.tide_strata_code as tide_strata ",
           "from tide_two as t ",
           "left join point_location as pl ",
           "on t.tide_station_location_id = pl.point_location_id ",
           "left join tide_strata_lut as ts ",
           "on t.tide_strata_id = ts.tide_strata_id ",
           "where date_part('year', t.low_tide_datetime) >= 2000 ",
           "and date_part('year', t.low_tide_datetime) <= 2056")

# Run the query
db_con = dbConnect(odbc::odbc(), timezone = "UTC", dsn = "local_shellfish_archive")
tide_times = dbGetQuery(db_con, qry)
dbDisconnect(db_con)

# Calculate night and daytime
Seattle = matrix(c(-122.3383, 47.605), nrow=1)
Seattle_sp = SpatialPoints(Seattle, proj4string=CRS("+proj=longlat +datum=WGS84"))

# Explicitly convert timezones
tide_times = tide_times %>%
  mutate(tide_datetime = with_tz(tide_datetime, tzone = "America/Los_Angeles")) %>%
  mutate(sunrise = sunriset(Seattle_sp, tide_datetime, direction="sunrise", POSIXct.out=TRUE)[,2]) %>%
  mutate(sunset = sunriset(Seattle_sp, tide_datetime, direction="sunset", POSIXct.out=TRUE)[,2]) %>%
  mutate(tide_datetime = format(tide_datetime)) %>%
  mutate(tide_date = substr(tide_datetime, 1, 10)) %>%
  select(tide_date, tide_station, tide_datetime, tide_time, tide_height, tide_strata, sunrise, sunset)

# Check if any tide_times strata differ by date...should only be Seattle strata...All Ok
time_check = tide_times %>%
  select(tide_date, tide_strata) %>%
  distinct() %>%
  group_by(tide_date) %>%
  mutate(n_seq = row_number()) %>%
  ungroup() %>%
  filter(n_seq > 1)

# Get the tide correction data
qry = glue("select distinct bb.beach_number as bidn, b.local_beach_name as beach_name, ",
           "pl.location_name as tide_station, b.low_tide_correction_minutes as lt_corr ",
           "from beach as b ",
           "inner join beach_boundary_history as bb ",
           "on b.beach_id = bb.beach_id ",
           "left join point_location as pl ",
           "on b.tide_station_location_id = pl.point_location_id ",
           "order by bb.beach_number")

# Run the query
db_con = dbConnect(odbc::odbc(), timezone = "UTC", dsn = "local_shellfish")
tide_corr = dbGetQuery(db_con, qry)
dbDisconnect(db_con)

# Check for duplicated BIDNs
chk_dup_beach = tide_corr %>%
  filter(duplicated(bidn)) %>%
  left_join(tide_corr, by = "bidn")

# Report if any duplicated beach_ids or BIDNs
if (nrow(chk_dup_beach) > 0) {
  cat("\nWARNING: Duplicated BIDNs. Do not pass go!\n\n")
} else {
  cat("\nNo duplicated BIDNs. Ok to proceed.\n\n")
}

# # Save to RData file
# saveRDS(tide_times, "www/tide_times.rds")

# # Save to RData file
# saveRDS(tide_corr, "tide_corr.rds")

# # Read .rds tide data
# tide_times = readRDS("tide_times.rds")
# tide_corr = readRDS("tide_corr.rds")



