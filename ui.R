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
                   tabPanel("Tide plot",
                            sidebarLayout(
                              sidebarPanel(width = 2,

                                           # # Set height of inputs
                                           # tags$style(".shiny-input-container  {line-height: 5px; height : 25px}"),
                                           # tags$style(".form-control  {height: 25px;}"),

                                           # Select beach
                                           selectizeInput(inputId = "map_beach_select",
                                                          label = "Predict tide for:",
                                                          choices = beach_list,
                                                          selected = "Port Townsend"),
                                           dateInput(inputId = "map_date_one",
                                                          label = "From:",
                                                          format = "D M dd yyyy",
                                                          value = Sys.Date()),
                                           dateInput(inputId = "map_date_two",
                                                     label = "To:",
                                                     format = "D M dd yyyy",
                                                     value = Sys.Date()),
                                           radioButtons(inputId = "map_tide_unit",
                                                        label = "Units",
                                                        choices = c("feet", "meters"),
                                                        selected = "feet",
                                                        inline = TRUE),
                                           selectizeInput(inputId = "time_interval",
                                                          label = "Time interval (minutes)",
                                                          choices = c(1, 6, 15, 30, 60),
                                                          selected = 15),
                                           br(),
                                           br(),
                                           img(src = "buster.jpg", width = "75%"),
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

                                  column(width = 4,
                                         # map output height 655
                                         leafletOutput("beach_map", height = 550),
                                         br(),
                                         div(DT::DTOutput("tide_report"), style = "font-size: 80%")

                                  ),
                                  column(width = 7,
                                         # verbatimTextOutput("check_val"),
                                         tableOutput("tides"),
                                         dygraphOutput("tide_graph", height = "300px"),
                                         br(),
                                         br(),
                                         div(DT::DTOutput("map_high_low"), style = "font-size: 80%")

                                  )
                                )
                              )
                            )
                   ),
                   tabPanel("High-Low only",
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                     # Select primary tide stations in WA
                                     selectizeInput(inputId = "out_beach_select",
                                                    label = "High-Low for:",
                                                    multiple = TRUE,
                                                    choices = beach_list,
                                                    selected = "Seattle"),
                                     dateRangeInput(inputId = "out_tide_range",
                                                    label = "From (no limit)",
                                                    format = "D M dd yyyy",
                                                    start = paste0(format(Sys.Date(), format = "%Y"), "-03-01"),
                                                    end = paste0(format(Sys.Date(), format = "%Y"), "-09-01"),
                                                    min = "1970-01-01", max = "2056-12-31"),
                                     radioButtons(inputId = "out_tide_unit",
                                                  label = "Units",
                                                  choices = c("Feet", "Meters"),
                                                  selected = "Feet",
                                                  inline = TRUE),
                                     selectizeInput(inputId = "out_strata",
                                                    label = "Tide strata",
                                                    multiple = TRUE,
                                                    choices = c("ELOW", "LOW", "HIGH", "PLUS", "XPLUS", "NIGHT"),
                                                    selected = c("ELOW", "HIGH", "LOW"))
                              ),
                              mainPanel(width = 9,
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

