# base_script.R

#===========================================================================================
# Notes:
#
# Dygraph shading: https://stackoverflow.com/questions/31449429/r-how-to-shade-week-end-period-with-dygraphs
#                  http://databasefaq.com/index.php/answer/20331/r-dygraphs-dyshading-r-dygraph
#
#
#  ToDo:
#  1.
#
# AS 2019-04-02
#===========================================================================================

# Clear workspace
rm(list=ls(all=TRUE))

# Libraries
library(dplyr)
library(glue)
library(tibble)
library(lubridate)
library(shiny)
library(shinythemes)
library(leaflet)
library(sf)
library(markdown)
library(tibble)
library(rtide)
library(dygraphs)
library(DT)
library(xts)
library(maptools)

# Set options
options(digits=14)

# Set max upload size to 10MB...should handle all zero shapefiles
options(shiny.maxRequestSize=10*1024^2)

# Read .rds tide data
tide_times = readRDS("www/tide_times.rds")
wa_beaches = readRDS("www/wa_beaches.rds")
wa_stations = readRDS("www/wa_stations.rds")

#======================================================================
# Define functions
#======================================================================

# Function to pull out item of text
get_text_item = function (x, item = 2, sep = " ") {
  get_list_item <- function(x, item = 2) {
    if (is.na(x[item])) {
      x = NA
    } else {
      x = x[item]
    }
    x
  }
  nms = strsplit(x, sep)
  unlist(lapply(nms, get_list_item, item))
}

# Function to do the shading in dygraphs
add_shades <- function(x, periods, ...) {
  for( period in periods ) {
    x <- dyShading(x, from = period$from , to = period$to, ... )
  }
  x
}

# Check crs of wa_beaches
st_crs(wa_beaches)$epsg
class(wa_beaches)

# Output leaflet bidn map
m = leaflet(wa_beaches) %>%
  fitBounds(-122.12, 47.0, -124.79, 49.0) %>%
  addPolygons(group = "Beaches",
              fillOpacity = 0.4,
              label = ~paste0(beach_name, " (", bidn, ")"),
              labelOptions = labelOptions(noHide = FALSE),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addMarkers(data = wa_stations,
             group = "Tide stations",
             #clusterOptions = markerClusterOptions(),
             label = ~station_name,
             labelOptions = labelOptions(noHide = FALSE)) %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
  addProviderTiles("Esri.OceanBasemap", group = "Esri Ocean basemap") %>%
  addLayersControl(position = 'bottomright',
                   baseGroups = c("Esri World Imagery", "Esri Ocean basemap"),
                   overlayGroups = c("Beaches", "Tide stations"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup("Tide stations")

m

# Select the station
selected_bidn = "270170"
#selected_bidn = "100000"

# Select the unit
#selected_unit = "meters"
selected_unit = "feet"

# Select the interval for predictions in minutes
selected_interval = 10L

# Select the date
selected_start = "2019-04-13"
selected_end = "2019-04-14"

# Get the tide_station data
if (selected_bidn %in% wa_beaches$bidn) {
  station_data = wa_beaches %>%
    st_drop_geometry() %>%
    filter(bidn == selected_bidn) %>%
    select(station_name, lt_corr = low_correction)
} else {
  station_data = wa_stations %>%
    st_drop_geometry() %>%
    mutate(lt_corr = 0L) %>%
    select(station_name, lt_corr)
}

# Pull out tide_station
selected_station = station_data$station_name

tide_data = rtide::tide_height(
    selected_station,
    # Pad one day at either end
    from = as.Date(selected_start) - 1,
    to = as.Date(selected_end) + 1,
    minutes = selected_interval,
    tz = "America/Los_Angeles")

# Add time correction for beach, correct for selected unit, round
tide_pred = tide_data %>%
  mutate(station_name = get_text_item(Station, item = 1, sep = ",")) %>%
  mutate(selected_unit = selected_unit) %>%
  mutate(pred_height = if_else(selected_unit == "feet",
                               TideHeight * 3.28084, TideHeight)) %>%
  mutate(pred_height = round(pred_height, 3)) %>%
  left_join(station_data, by = "station_name") %>%
  mutate(tide_datetime = DateTime + minutes(lt_corr)) %>%
  select(station_name, station_name, tide_datetime, pred_height)

# Trim to closest minute_interval before and after selected start and end
start_pad = with_tz(as.POSIXct(format(as.Date(selected_start) - minutes(selected_interval)),
                               tzone = "America/Los_Angeles"))
end_pad = with_tz(as.POSIXct(format(as.Date(selected_end) + minutes(selected_interval)),
                               tzone = "America/Los_Angeles"))
# Apply pad filters
dy_pred = tide_pred %>%
  filter(tide_datetime >= start_pad & tide_datetime <= end_pad) %>%
  select(pred_height, tide_datetime) %>%
  setNames(c(selected_unit, "datetime"))

# Calculate night and daytime
Seattle = matrix(c(-122.3383, 47.605), nrow=1)
Seattle_sp = SpatialPoints(Seattle, proj4string=CRS("+proj=longlat +datum=WGS84"))

# Calculate sunrise and sunset times
dy_list = dy_pred %>%
  mutate(sunrise = sunriset(Seattle_sp, datetime, direction="sunrise", POSIXct.out=TRUE)[,2]) %>%
  mutate(sunset = sunriset(Seattle_sp, datetime, direction="sunset", POSIXct.out=TRUE)[,2]) %>%
  mutate(daytime = if_else(datetime >= sunrise & datetime < sunset, "light", "dark"))

# Identify breaks between day and night in dy_list
i = 1
dy_list$daybreak = 0L
while (i <= length(dy_list$daybreak)) {
  if (dy_list$daytime[i] == "dark" & i == 1L) {
    dy_list$daybreak[i] = 0L
    i = i + 1
  }
  if (dy_list$daytime[i - 1] == "dark" & dy_list$daytime[i] == "light") {
    dy_list$daybreak[i] = 1L
  }
  if (dy_list$daytime[i] == "light" & dy_list$daytime[i + 1] == "dark") {
    dy_list$daybreak[i] = 1L
  }
  i = i + 1
}

# Calculate shading intervals as a list
day_time = dy_list$datetime[dy_list$daybreak == 1L & dy_list$daytime == "light"]
ok_periods = 0
i=1
j=1
while (i < (length(dy_list) - 1)){
  ok_periods[j] = list(list(from = day_time[i], to = day_time[i + 1]))
  i = i + 2
  j = j + 1
}

# Prep for dygraph
dy_pred = xts::xts(dy_pred, order.by = dy_pred$datetime)
dy_pred$datetime = NULL

# Compute pad range for dygraph scale
pad = (max(tide_pred$pred_height) - min(tide_pred$pred_height)) / 7
pad_range <- c(min(tide_pred$pred_height) - pad, max(tide_pred$pred_height) + pad)

# Calculate current_time for axis
now_time = Sys.time()

# Plot dygraph
dygraph(dy_pred, height = "200px") %>%
  dyOptions(strokeWidth = 1.5, drawGrid = F, includeZero = F,
            useDataTimezone = T, drawGapEdgePoints = T, rightGap = 15) %>%
  dyRangeSelector() %>%
  dyAxis("y", valueRange = pad_range,
         label = selected_unit) %>%
  dyEvent(x = now_time, label = "Current time", labelLoc = "bottom") %>%
  add_shades(ok_periods, color = "#FFFFCC" ) %>%
  dyLegend()

#=====================================================
# Generate table of all beaches and stations
#=====================================================

# Pull out needed data from wa_stations
wa_non_beach = wa_stations %>%
  st_drop_geometry() %>%
  mutate(beach_name = station_name) %>%
  mutate(low_correction = 0L) %>%
  select(bidn, beach_name, low_correction, station_name)

# Add beach predictions high and low
wa_beach = wa_beaches %>%
  st_drop_geometry() %>%
  select(bidn, beach_name, low_correction, station_name)

# Pull out combined locations
combined_locations = rbind(wa_beach, wa_non_beach)

#============================================================
# Add data for selected beach
#============================================================

# Pull out single_select station or beach
single_station = combined_locations %>%
  distinct() %>%
  filter(bidn %in% selected_bidn) %>%
  select(bidn, beach_name)

# We don't want to include end_date
calc_start = as.POSIXct(selected_start, tz = "America/Los_Angeles")
calc_end = as.POSIXct(selected_end, tz = "America/Los_Angeles") - 1

# Format tide_table for output...join single_station to get name
tide_minutes = tide_pred %>%
  mutate(bidn = selected_bidn) %>%
  left_join(single_station, by = "bidn") %>%
  mutate(tide_date = strftime(tide_datetime, format = "%a %B %d, %Y")) %>%
  mutate(tide_time = strftime(tide_datetime, format = "%H:%M %p")) %>%
  mutate(tide_height = round(pred_height, 2)) %>%
  filter(between(tide_datetime, calc_start, calc_end)) %>%
  select(bidn, beach_name, tide_date, tide_time, tide_height)

#=========================================================================
# End of predictions....rest is just from database
#=========================================================================

# Define multi-selected_bidns
multi_selected_bidns = c("270170", "270470", "270200", "270286")

# Define multiple date vector selects
range_start = as.POSIXct("2019-04-11", tz = "America/Los_Angeles")
range_end = as.POSIXct("2019-04-18", tz = "America/Los_Angeles") - 1

# Add multi-select option for strata
selected_strata = c("HIGH", "LOW", "ELOW")

# Output table of high and low tides with sunrise, sunset, tide strata
tide_high_low = tide_times %>%
  filter(between(tide_date, range_start, range_end)) %>%
  mutate(sunrise = strftime(sunrise, format = "%H:%M")) %>%
  mutate(sunset = strftime(sunset, format = "%H:%M")) %>%
  select(tide_date, tide_station, sea_tide_datetime, tide_time,
         tide_height, tide_strata, sunrise, sunset)

# Pull out only needed locations....use multi-select here
multi_station = combined_locations %>%
  distinct() %>%
  filter(bidn %in% multi_selected_bidns) %>%
  select(bidn, beach_name, low_correction, tide_station = station_name)

# Add the high-low data
multi_station = multi_station %>%
  left_join(tide_high_low, by = "tide_station") %>%
  mutate(beach_time_minutes = tide_time + low_correction) %>%
  mutate(low_tide_at_beach = tide_date + minutes(beach_time_minutes)) %>%
  mutate(low_tide_at_beach = strftime(low_tide_at_beach, format = "%H:%M")) %>%
  mutate(low_tide_at_station = tide_date + minutes(tide_time)) %>%
  mutate(low_tide_at_station = strftime(low_tide_at_station, format = "%H:%M")) %>%
  mutate(beach = paste0(beach_name, " (", bidn, ")")) %>%
  select(tide_date, beach, low_tide_at_beach, tide_height_at_station = tide_height,
         tide_strata, sunrise, sunset, tide_station, low_tide_at_station,
         station_time_minutes = tide_time, low_tide_correction = low_correction,
         beach_time_minutes) %>%
  filter(tide_strata %in% selected_strata) %>%
  arrange(tide_date, beach, low_tide_at_beach)


# mutate(tide_date = strftime(tide_date, format = "%a %B %d, %Y")) %>%

# # Verify between
# x = as.POSIXct("2019-04-11", tz = "America/Los_Angeles")
# y = as.POSIXct("2019-04-19", tz = "America/Los_Angeles")
# dts = tibble(survey_date = seq(x, y, by = 30))
# head(dts)
#
# strt = as.POSIXct("2019-04-12", tz = "America/Los_Angeles")
# end = as.POSIXct("2019-04-13", tz = "America/Los_Angeles") - 1
#
# # Identify end points...between will include the end_point
# dts_between = dts %>%
#   filter(between(survey_date, strt, end))








