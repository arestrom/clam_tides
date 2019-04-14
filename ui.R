#=======================================================================================
# Shiny app to predict tides
#
# Notes:
#  1.
#
# AS 2019-04-11
#=======================================================================================

# Define UI for application that draws a histogram
shinyUI(navbarPage(theme = shinytheme("sandstone"),
                   "Clam Tides",
                   id = "tab_being_displayed",
                   tabPanel("Map View",
                            sidebarLayout(
                              sidebarPanel(width = 2,
                                           # Select beach
                                           div(selectizeInput(inputId = "clam_beach", label = "Select beach",
                                                              choices = tide_corr$beach_name),
                                               style = "font-size: 80%; height: 40px;"),
                                           div(dateInput(inputId = "tide_date", label = "Select the date",
                                                         value = Sys.Date(), format = "mm/dd/yy",
                                                         min = "2000-01-01", max = "2056-12-31"),
                                               style = "font-size: 80%; height: 200px"),
                                           br(),
                                           br(),
                                           img(src = "clam_flight.jpg", width = "75%"),
                                           br(),
                                           br(),
                                           img(src = "wdfw.png", width = "75%")
                              ),
                              mainPanel(width = 10,
                                # Style errors
                                tags$style(type = "text/css",
                                           ".shiny-output-error-validation {
                                           color: #1c3e75; font-weight: normal; font-size: 1.18em;}"
                                ),
                                fluidRow(

                                  column(width = 5,
                                         # map output height 655
                                         leafletOutput("beach_map", height = 650)
                                  ),
                                  column(width = 7,
                                         div(DT::DTOutput("tide_graph"), style = "font-size: 80%"),
                                         br(),
                                         div(DT::DTOutput("tide_report"), style = "font-size: 80%")
                                  )
                                )
                              )
                            )
                   ),
                   tabPanel("Historical",
                            sidebarLayout(
                              sidebarPanel(width = 2,
                                     # Select primary tide stations in WA
                                     selectizeInput(inputId = "primary_stations", label = "Select the tide station",
                                                    choices = c("Seattle" = "seattle",
                                                                "Port Townsend" = "port_townsend"),
                                                    selected = "seattle"),
                                     dateRangeInput(inputId = "tide_range", label = "Select date range to export",
                                               format = "mm/dd/yy",
                                               start = paste0(format(Sys.Date(), format = "%Y"), "-01-01"),
                                               end = paste0(format(Sys.Date(), format = "%Y"), "-12-31"),
                                               min = "2000-01-01", max = "2056-12-31")
                              ),
                              mainPanel(width = 10,
                                     # Table showing tide predictions
                                     div(DT::DTOutput("tide_range_report"), style = "font-size: 80%")
                              )
                            )
                   ),
                   tabPanel("About",
                            fluidRow(
                              column(width = 3,
                                     img(src = "aerial.jpg", width = "85%"),
                                     br(),
                                     br(),
                                     includeMarkdown("www/credits.Rmd")
                              ),
                              column(offset = 1,
                                     width = 8,
                                     img(src = "diggers.jpg", width = "50%"),
                                     br(),
                                     includeMarkdown("www/about.Rmd")
                              )
                            )
                   )
))

