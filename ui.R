#=======================================================================================
# Shiny app to predict tides
#=======================================================================================

# Define UI for application that draws a histogram
shinyUI(navbarPage(theme = shinytheme("sandstone"),
                   "Clam Tides",
                   id = "tab_being_displayed",
                   tabPanel("Tide plot",
                            sidebarLayout(
                              sidebarPanel(width = 2,
                                           # Set style for inputs
                                           tags$style(type = "text/css",
                                                      ".form-control {
                                                       width: 100%; height: 24px;
                                                       padding-top: 0px;
                                                       padding-bottom: 0px;
                                                       font-size: 12px;
                                                       margin-bottom: 0px;}"),
                                           tags$style(type = "text/css",
                                                      "form-group.shiny-input-container {
                                                       width: 100%;
                                                       height: 24px;
                                                       padding-top: 0px;
                                                       padding-bottom: 0px;
                                                       font-size: 12px;
                                                       margin-bottom: 0px;}"),
                                           tags$style(type = "text/css",
                                                      ".selectize-input.items.full.has-options.has-items {
                                                       font-size: 12px;
                                                       min-height: 24px;
                                                       padding-top: 0px;
                                                       padding-bottom: 0px;
                                                       margin-bottom: 0px;}"),
                                           tags$style(type = "text/css",
                                                      ".control-label {
                                                       font-size: 12px;}"),
                                           tags$style(type = "text/css",
                                                      ".shiny-options-group {
                                                       font-size: 12px;}"),

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
                                                     value = Sys.Date() + 1),
                                           radioButtons(inputId = "map_tide_unit",
                                                        label = "Units",
                                                        choices = c("feet", "meters"),
                                                        selected = "feet",
                                                        inline = TRUE),
                                           selectizeInput(inputId = "time_interval",
                                                          label = "Time interval (minutes)",
                                                          choices = c(1, 6, 15, 30, 60),
                                                          selected = 15),
                                           div(style="display:inline-block; text-align: center;",
                                               img(src = "buster.jpg", width = "65%")),
                                           br(),
                                           br(),
                                           div(style="display:inline-block; text-align: center;",
                                               img(src = "wdfw.png", width = "65%"))
                              ),
                              mainPanel(width = 10,

                                # Style errors
                                tags$style(type = "text/css",
                                           ".shiny-output-error-validation {
                                            color: #1c3e75;
                                            font-weight: normal;
                                            font-size: 1.18em;}"),
                                # Style dygraph
                                tags$style(type = "text/css",
                                           ".dygraph-title {
                                            font-size: 55%;
                                            font-weight: bold;
                                            font-style: italic;}"),
                                tags$style(type = "text/css",
                                           ".dygraph-label  {
                                            font-size: 12px;}"),
                                tags$style(type = "text/css",
                                           ".dygraph-axis-label {
                                            font-size: 12px;}"),

                                fluidRow(

                                  column(width = 5,
                                         # map output height 625
                                         leafletOutput("beach_map", height = "625px")
                                  ),
                                  column(width = 7,
                                         #verbatimTextOutput("check_val"),
                                         dygraphOutput("tide_graph", height = "350px"),
                                         br(),
                                         br(),
                                         div(DT::DTOutput("tide_report"), style = "font-size: 80%")

                                  )
                                )
                              )
                            )
                   ),
                   tabPanel("High-Low only",
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                     # Select primary tide stations in WA
                                     selectizeInput(inputId = "high_beach_select",
                                                    label = "High-Low tides for:",
                                                    multiple = TRUE,
                                                    choices = high_beach_list,
                                                    selected = "Port Townsend"),
                                     dateInput(inputId = "high_date_one",
                                               label = "From:",
                                               format = "D M dd yyyy",
                                               value = Sys.Date()),
                                     dateInput(inputId = "high_date_two",
                                               label = "To:",
                                               format = "D M dd yyyy",
                                               value = Sys.Date() + 1),
                                     radioButtons(inputId = "high_tide_unit",
                                                  label = "Units",
                                                  choices = c("feet", "meters"),
                                                  selected = "feet",
                                                  inline = TRUE),
                                     selectizeInput(inputId = "high_strata",
                                                    label = "Tide strata",
                                                    multiple = TRUE,
                                                    choices = c("ELOW", "LOW", "HIGH", "PLUS", "XPLUS", "NIGHT"),
                                                    selected = c("ELOW", "HIGH", "LOW"))
                              ),
                              mainPanel(width = 9,
                                     # Table showing tide predictions
                                     # tableOutput("high"),
                                     div(DT::DTOutput("high_low_report"), style = "font-size: 80%")
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
                                     img(src = "ocean_two.jpg", width = "60%"),
                                     br(),
                                     includeMarkdown("www/about.Rmd")
                              )
                            )
                   )
))

