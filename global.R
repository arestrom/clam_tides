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
sites = readRDS("www/sites.rds")

#======================================================================================
# Define functions
#======================================================================================

inline = function (x) {
  tags$div(style = "display:inline-block;", x)
}

# Function to generate errors or warnings from reading shapefiles
try_catch_we <- function(expr) {
  W <- NULL
  whandler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = whandler),
       warning = W)
}

# Function to convert tide time to minutes
hm_to_min = function(x) {
  hr = as.integer(substr(x, 1, 2))
  hr = hr * 60
  mn = as.integer(substr(x, 4, 5))
  mins = hr + mn
  mins
}


