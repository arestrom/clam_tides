#======================================================================================
# global.R
#
#
# Notes:
#  1. Consider creating smaller functions for common items below
#
# AS 2019-03-27
#======================================================================================

# Libraries
library(dplyr)
library(glue)
library(tibble)
library(lubridate)
library(shiny)
library(shinythemes)
library(leaflet)
library(maptools)
library(sf)
library(markdown)
library(tibble)
library(rtide)
library(dygraphs)
library(DT)

# Set options
options(digits=14)

# Set max upload size to 10MB...should handle all zero shapefiles
options(shiny.maxRequestSize=10*1024^2)

# Read .rds tide data
tide_times = readRDS("www/tide_times.rds")
tide_corr = readRDS("www/tide_corr.rds")
wa_beaches = readRDS("www/wa_beaches.rds")
wa_stations = readRDS("www/wa_stations.rds")
beaches_stations = readRDS("www/beaches_stations.rds")

#===================================================
# Inputs for map tab
#===================================================

# Prep select inputs for map tab
station_list = beaches_stations %>%
  filter(as.integer(bidn) < 200000) %>%
  select(beach_name) %>%
  arrange(beach_name)
station_list = as.list(station_list$beach_name)

# Prep select inputs
beach_list = beaches_stations %>%
  filter(as.integer(bidn) >= 200000) %>%
  mutate(beach_name = paste0(beach_name, " (", bidn, ")")) %>%
  select(beach_name) %>%
  arrange(beach_name)
beach_list = as.list(beach_list$beach_name)

# Create selectize list for multiple high-low sites
beach_list = list(`Tide stations` = station_list,
                  "Beaches" = beach_list)

#===================================================
# Inputs for high-low tab
#===================================================

# Prep select inputs for map tab
high_station_list = beaches_stations %>%
  filter(as.integer(bidn) < 200000) %>%
  select(beach_name) %>%
  filter(beach_name %in% c("Port Townsend", "Seattle")) %>%
  arrange(beach_name)
high_station_list = as.list(high_station_list$beach_name)

# Prep select inputs
high_beach_list = beaches_stations %>%
  filter(as.integer(bidn) >= 200000) %>%
  mutate(beach_name = paste0(beach_name, " (", bidn, ")")) %>%
  select(beach_name) %>%
  arrange(beach_name)
high_beach_list = as.list(high_beach_list$beach_name)

# Create selectize list for multiple high-low sites
high_beach_list = list(`Tide stations` = high_station_list,
                  "Beaches" = high_beach_list)


# Create beach data for matching select input with corrections
beach_data = beaches_stations %>%
  mutate(beach_name = if_else(as.integer(bidn) > 200000L,
                              paste0(beach_name, " (", bidn, ")"),
                              beach_name))

# Create dataset of sunrise and sunset times
sun_times = tide_times %>%
  select(datetime = tide_date, sunrise, sunset)

# Data needed for sunrise sunset calculations
seattle_mat = matrix(c(-122.3383, 47.605), nrow=1)
seattle_sp = SpatialPoints(seattle_mat, proj4string=CRS("+proj=longlat +datum=WGS84"))

# Create label for wa_beaches
wa_beaches = wa_beaches %>%
  mutate(beach_label = paste0(beach_name, " (", bidn, ")"))

#======================================================================================
# Define functions
#======================================================================================

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

# Identify breaks between day and night in dy_list
day_break = function(dy_list = dy_list) {
  dy_list$daybreak = 0L
  i = 1
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
  dy_list$daybreak
}

# # Identify breaks between day and night in dy_list
# dy_list$daybreak = 0L; i = 1
# while (i <= length(dy_list$daybreak)) {
#   if (dy_list$daytime[i] == "dark" & i == 1L) {
#     dy_list$daybreak[i] = 0L
#     i = i + 1
#   }
#   if (dy_list$daytime[i - 1] == "dark" & dy_list$daytime[i] == "light") {
#     dy_list$daybreak[i] = 1L
#   }
#   if (dy_list$daytime[i] == "light" & dy_list$daytime[i + 1] == "dark") {
#     dy_list$daybreak[i] = 1L
#   }
#   i = i + 1
# }

# # Function to convert tide time to minutes
# hm_to_min = function(x) {
#   hr = as.integer(substr(x, 1, 2))
#   hr = hr * 60
#   mn = as.integer(substr(x, 4, 5))
#   mins = hr + mn
#   mins
# }


