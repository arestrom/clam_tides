# server.R

#===========================================================================================
# Notes:
#  1. Input to rtide::tide_height() must be class Date.
#  2. as.Date() converts everything to UTC....but you can set a tz
#  3. Output from rtide::tide_height() is a POSIXct object in timezone tz
#  4. Tested all meter to feet conversion values. Current five decimal
#     value seems most appropriate to match noaa predictions. Only some
#     rounding error difference at 0.01.
#
#  ToDo:
#  1. Add station tide time to map_high_low...Done
#  2. Add correction = zero to all stations lower than 200000...Done
#  3. Test that output from stations compute correctly...Done
#  4. Add progress bar. See: https://gallery.shinyapps.io/085-progress/...Done
#
# AS 2019-04-23
#===========================================================================================

# Server code
shinyServer(function(input, output, session) {

  # Output leaflet bidn map
  output$beach_map <- renderLeaflet({
    m = leaflet() %>%
      setView(-122.72, 48.0, zoom = 8) %>%
      #fitBounds(-122.12, 47.0, -123.23, 49.0) %>%
      addPolygons(data = wa_beaches,
                  group = "Beaches",
                  fillOpacity = 0.4,
                  label = ~beach_label,
                  layerId = ~beach_label,
                  labelOptions = labelOptions(noHide = FALSE),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addMarkers(data = wa_stations,
                 group = "Tide stations",
                 #clusterOptions = markerClusterOptions(),
                 label = ~station_name,
                 layerId = ~station_name,
                 labelOptions = labelOptions(noHide = FALSE)) %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
      addProviderTiles("Esri.OceanBasemap", group = "Esri Ocean basemap") %>%
      addLayersControl(position = 'bottomright',
                       baseGroups = c("Esri World Imagery", "Esri Ocean basemap"),
                       overlayGroups = c("Beaches", "Tide stations"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup("Tide stations")
    m
  })

  # map_station reactive value
  map_location <- reactiveValues(map_beach = NULL)

  # Observer to record drop-down selection
  observeEvent(input$map_beach_select, {
    map_location$map_beach = input$map_beach_select
  })

  # Observer to record polygon map click
  observeEvent(input$beach_map_shape_click, {
    map_location$map_beach = input$beach_map_shape_click
  })

  # Observer to record marker map click
  observeEvent(input$beach_map_marker_click, {
    map_location$map_beach = input$beach_map_marker_click
  })

  # Update selectizeInput
  observe({
    selected_map_location = map_location$map_beach[[1]]
    # Update
    updateSelectizeInput(session, "map_beach_select",
                         choices = beach_list,
                         selected = selected_map_location)
  })

  # Filter to selected beach
  map_station = reactive({
    req(input$map_beach_select)
    beach_data %>%
      filter(beach_name == input$map_beach_select) %>%
      select(beach_name, station_name, low_correction)
  })

  time_interval = reactive({
    req(input$time_interval)
    as.integer(input$time_interval)
  })

  tide_data = reactive({
    req(input$map_date_one)
    req(input$map_date_two)
    validate(
      need(input$map_date_two >= input$map_date_one,
           "Error: End date must be greater than or equal to the start date."),
      need(as.integer(input$map_date_two - input$map_date_one) < 30,
           "Error: Please limit predictions to a 30 day span.")
    )
    rtide::tide_height(
      stations = map_station()$station_name,
      # Pad one day at either end
      from = input$map_date_one,
      to = input$map_date_two,
      minutes = time_interval(),
      tz = "America/Los_Angeles")
  })

  # Add time correction for beach, correct for selected unit, round
  tide_pred = reactive({
    req(input$map_tide_unit)
    tide_data() %>%
      mutate(station_name = map_station()$station_name) %>%
      mutate(selected_unit = input$map_tide_unit) %>%
      mutate(pred_height = if_else(selected_unit == "feet",
                                   TideHeight * 3.28084, TideHeight)) %>%
      mutate(pred_height = round(pred_height, 2)) %>%
      mutate(beach_name = input$map_beach_select) %>%
      mutate(low_correction = map_station()$low_correction) %>%
      mutate(tide_datetime = DateTime + minutes(low_correction)) %>%
      select(beach_name, station_name, tide_datetime, pred_height)
  })

  dy_pred = reactive({
    tide_pred() %>%
      select(pred_height, tide_datetime) %>%
      setNames(c(input$map_tide_unit, "datetime"))
  })

  # Calculate sunrise and sunset times....Need to calculate...cant join...cause only high-low
  dy_list = reactive({
    dy_pred() %>%
      mutate(sunrise = sunriset(seattle_sp, datetime, direction="sunrise", POSIXct.out=TRUE)[,2]) %>%
      mutate(sunset = sunriset(seattle_sp, datetime, direction="sunset", POSIXct.out=TRUE)[,2]) %>%
      mutate(daytime = if_else(datetime >= sunrise & datetime < sunset, "light", "dark")) %>%
      mutate(daybreak = day_break(.)) %>%
      filter(daytime == "light" & daybreak == 1L)
  })

  day_periods = reactive({
    day_time = 0; i=1; j=1
    while (i <= (length(dy_list()$daybreak) - 1)) {
      day_time[j] = list(list(from = dy_list()$datetime[i], to = dy_list()$datetime[i + 1]))
      i = i + 2; j = j + 1
    }
    day_time
  })

  # Input data for dygraph
  dy_input = reactive({
    dy_data = xts::xts(dy_pred(), order.by = dy_pred()$datetime)
    dy_data$datetime = NULL
    dy_data
  })

  pad_range = reactive({
    max_pad = max(tide_pred()$pred_height)
    min_pad = min(tide_pred()$pred_height)
    pad = (max_pad - min_pad) / 7
    c(min_pad - pad, max_pad + pad)
  })

  # Reactive for dygraph
  tide_plot = reactive({
    ref_station = tide_pred()$station_name[1]
    graph_title = glue("Tide predictions in {time_interval()} min increments for {input$map_beach_select}. ",
                       "Tide height ({input$map_tide_unit}) is for {ref_station} reference station.")
    dygraph(dy_input(), main = graph_title, height = "10px") %>%
      dyOptions(strokeWidth = 1.5, drawGrid = F, includeZero = F,
                useDataTimezone = T, drawGapEdgePoints = T, rightGap = 15) %>%
      dyRangeSelector() %>%
      dyAxis("y", valueRange = pad_range(),
             label = input$map_tide_unit) %>%
      dyEvent(x = Sys.time(), label = "Current time", labelLoc = "bottom") %>%
      add_shades(day_periods(), color = "#FFFFCC" ) %>%
      dyLegend()
  })

  # Plot tides
  output$tide_graph <- renderDygraph({
    req(input$map_date_one)
    req(input$map_date_two)
    validate(
      need(input$map_date_two >= input$map_date_one,
           "Error: End date must be greater than or equal to the start date."),
      need(as.integer(input$map_date_two - input$map_date_one) < 30,
           "Error: Please limit predictions to a 30 day span.")
    )
    # Calculate the number of rows in tide_data()
    n_duration = interval(input$map_date_one, input$map_date_two)
    n_minutes = as.integer(dminutes(n_duration)) / (3600 * 60)

    # Set progress bar
    progress = Progress$new(session, min = 1, max = n_minutes)
    on.exit(progress$close())

    # Just output message without bar for now
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')

    for (i in 1:n_minutes) {
      progress$set(value = i)
    }
    tide_plot()
  })

  tide_rep = reactive({
    tide_pred() %>%
      mutate(station_name = map_station()$station_name) %>%
      mutate(beach_name = map_station()$beach_name) %>%
      mutate(tide_date = strftime(tide_datetime, format = "%a %B %d, %Y")) %>%
      mutate(tide_time = strftime(tide_datetime, format = "%H:%M")) %>%
      mutate(tide_height = round(pred_height, 2)) %>%
      select(tide_date, beach = beach_name, beach_tide = tide_time,
             ref_station = station_name, ref_height = tide_height)
  })

  # Output daily predicted tides
  output$tide_report = renderDT({
    ref_station = tide_pred()$station_name[1]
    tide_title = glue("Tide predictions in {time_interval()} min increments for {input$map_beach_select}. ",
                      "Tide height ({input$map_tide_unit}) is for {ref_station} reference station.")
    # Generate table
    datatable(tide_rep(),
              extensions = 'Buttons',
              options = list(dom = 'Blftp',
                             pageLength = 10,
                             lengthMenu = c(5, 10, 20, 40, 60, 100, 500, 5000),
                             scrollX = T,
                             buttons = c('excel', 'print'),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                               "}")),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left; color: black;',
                'Table 1: ', htmltools::em(htmltools::strong(tide_title)))) %>%
      formatRound("ref_height", 2)
  })

  # # Get highs and lows for map page
  # map_high_low = reactive({
  #   tide_times %>%
  #     filter(tide_station == map_station()$station_name) %>%
  #     mutate(tide_date = as.Date(tide_date)) %>%
  #     mutate(beach = map_station()$beach_name) %>%
  #     mutate(tide_corr = map_station()$low_correction) %>%
  #     filter(between(tide_date, input$map_date_one, input$map_date_two)) %>%
  #     mutate(beach_time = tide_time + tide_corr) %>%
  #     mutate(char_date = format(tide_date)) %>%
  #     mutate(beach_date = as.POSIXct(char_date, tz = "America/Los_Angeles")) %>%
  #     mutate(beach_datetime = beach_date + minutes(beach_time)) %>%
  #     mutate(beach_tide = strftime(beach_datetime, format = "%H:%M")) %>%
  #     mutate(station_tide = beach_date + minutes(tide_time)) %>%
  #     mutate(station_tide = strftime(station_tide, format = "%H:%M")) %>%
  #     mutate(sunrise = strftime(sunrise, format = "%H:%M")) %>%
  #     mutate(sunset = strftime(sunset, format = "%H:%M")) %>%
  #     select(tide_date, beach, beach_tide, sunrise, sunset,
  #            strata = tide_strata, station = tide_station,
  #            station_tide, low_correction = tide_corr)
  # })
  #
  # # Output daily predicted tides
  # output$map_high_low = renderDT({
  #   tide_title = glue("High and low tides ({input$map_tide_unit}) for {input$map_beach_select}")
  #   # Generate table
  #   datatable(map_high_low(),
  #             extensions = 'Buttons',
  #             options = list(dom = 'Blftp',
  #                            pageLength = 10,
  #                            lengthMenu = c(5, 10, 20, 40, 60, 100, 500, 5000),
  #                            scrollX = T,
  #                            buttons = c('excel', 'print'),
  #                            initComplete = JS(
  #                              "function(settings, json) {",
  #                              "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
  #                              "}")),
  #             caption = htmltools::tags$caption(
  #               style = 'caption-side: top; text-align: left; color: black;',
  #               'Table 2: ', htmltools::em(htmltools::strong(tide_title))))
  # })

  # Output table of high and low tides with sunrise, sunset, tide strata
  high_low = reactive({
    tide_times %>%
      mutate(tide_date = as.Date(tide_date)) %>%
      filter(between(tide_date, input$high_date_one, input$high_date_two)) %>%
      mutate(sunrise = strftime(sunrise, format = "%H:%M")) %>%
      mutate(sunset = strftime(sunset, format = "%H:%M")) %>%
      select(tide_date, station_name = tide_station, tide_time, tide_height,
             tide_strata, sunrise, sunset)
  })

  # Pull out only needed locations....use multi-select here
  multi_station = reactive({
    beach_data %>%
      filter(station_name %in% c("Port Townsend", "Seattle")) %>%
      filter(beach_name %in% input$high_beach_select) %>%
      left_join(high_low(), by = "station_name") %>%
      mutate(beach_minutes = tide_time + low_correction) %>%
      mutate(tide_date = as.POSIXct(format(tide_date), tz = "America/Los_Angeles")) %>%
      mutate(low_tide = tide_date + minutes(beach_minutes)) %>%
      mutate(low_tide = strftime(low_tide, format = "%H:%M")) %>%
      mutate(station_tide = tide_date + minutes(tide_time)) %>%
      mutate(station_tide = strftime(station_tide, format = "%H:%M")) %>%
      mutate(high_tide_unit = input$high_tide_unit) %>%
      mutate(tide_height = if_else(high_tide_unit == "meters",
                                   tide_height / 3.28084, tide_height)) %>%
      mutate(tide_height = round(tide_height, 2)) %>%
      select(tide_date, beach = beach_name, beach_tide = low_tide,
             sunrise, sunset, strata = tide_strata, ref_station = station_name,
             ref_tide = station_tide, ref_height = tide_height, low_correction) %>%
      filter(strata %in% input$high_strata) %>%
      arrange(tide_date, beach, beach_tide) %>%
      mutate(tide_date = strftime(tide_date, format = "%a %B %d, %Y"))
  })

  # Output daily predicted tides
  output$high_low_report = renderDT({
    high_low_title = glue("Time of high and low tides for selected beaches. ",
                          "Strata were computed based on Seattle tides. Height ({input$high_tide_unit}) ",
                          "is for the reference station")
    # Generate table
    datatable(multi_station(),
              extensions = 'Buttons',
              options = list(dom = 'Blftp',
                             pageLength = 10,
                             lengthMenu = c(5, 10, 20, 40, 60, 100, 500, 5000),
                             scrollX = T,
                             buttons = c('excel', 'print'),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                               "}")),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left; color: black;',
                'Table 2: ', htmltools::em(htmltools::strong(high_low_title)))) %>%
      formatRound("ref_height", 2)
  })

  # output$check_val = renderText(map_location$map_beach[[1]])
  #output$check_val = renderText(unlist(start_pad()))

  # output$tides = renderTable(
  #   map_high_low() %>%
  #     mutate(tide_date = format(tide_date))
  # )

  # output$high = renderTable(
  #   multi_station()
  # )

  # output$tides = renderTable(
  #   dy_input()
  #   )

  # # close the R session when Chrome closes
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("no")
  # })
})
