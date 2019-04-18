# server.R

#===========================================================================================
# Notes:
#  1. Input to rtide::tide_height() must be class Date.
#  2. as.Date() converts everything to UTC
#  3. Output from rtide::tide_height() is a POSIXct object in timezone tz
#
#
#  ToDo:
#  1. Add station tide time to map_high_low
#  2. Add correction = zero to all stations lower than 200000
#  3. Test that output from stations compute correctly.
#
# AS 2019-04-02
#===========================================================================================

# Server code
shinyServer(function(input, output, session) {

  # Output leaflet bidn map
  output$beach_map <- renderLeaflet({
    m = leaflet() %>%
      setView(-122.72, 48, zoom = 10) %>%
      #fitBounds(-122.12, 47.0, -123.23, 49.0) %>%
      addPolygons(data = wa_beaches,
                  group = "Beaches",
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
                       options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup("Tide stations")
    m
  })

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
           "Error: End date must be greater than the start date.")
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
                                   TideHeight * 3.28, TideHeight)) %>%
    mutate(pred_height = round(pred_height, 3)) %>%
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
    dygraph(dy_input(), height = "10px") %>%
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
    tide_plot()
  })

  tide_rep = reactive({
    tide_pred() %>%
      mutate(beach_name = map_station()$station_name) %>%
      mutate(tide_date = strftime(tide_datetime, format = "%a %B %d, %Y")) %>%
      mutate(tide_time = strftime(tide_datetime, format = "%H:%M")) %>%
      mutate(tide_height = round(pred_height, 2)) %>%
      select(tide_date, tide_time, tide_height)
  })

  # Output daily predicted tides
  output$tide_report = renderDT({
    tide_title = glue("Tide prediction for {input$map_beach_select}")
    # Generate table
    datatable(tide_rep(),
              extensions = 'Buttons',
              options = list(dom = 'Blftp',
                             pageLength = 5,
                             lengthMenu = c(5, 10, 20, 40, 60, 100, 500, 5000),
                             scrollX = T,
                             buttons = c('excel', 'print'),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                               "}")),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left; color: black;',
                'Table 1: ', htmltools::em(htmltools::strong(tide_title))))
  })

  # Get highs and lows for map page
  map_high_low = reactive({
    tide_times %>%
      filter(tide_station == map_station()$station_name) %>%
      mutate(tide_date = as.Date(tide_date)) %>%
      mutate(beach_name = map_station()$beach_name) %>%
      mutate(tide_corr = map_station()$low_correction) %>%
      filter(between(tide_date, input$map_date_one, input$map_date_two)) %>%
      mutate(beach_time = tide_time + tide_corr) %>%
      mutate(char_date = format(tide_date)) %>%
      mutate(beach_date = as.POSIXct(char_date, tz = "America/Los_Angeles")) %>%
      mutate(beach_datetime = beach_date + minutes(beach_time)) %>%
      mutate(beach_datetime = format(beach_datetime)) %>%
      mutate(sunrise = strftime(sunrise, format = "%H:%M")) %>%
      mutate(sunset = strftime(sunset, format = "%H:%M")) %>%
      select(tide_date, beach_name, tide_station, sea_tide_datetime, tide_time,
             tide_corr, beach_time, char_date, beach_datetime, tide_height,
             tide_strata, sunrise, sunset)
  })




  output$check_val = renderText(tz(input$map_date_one))
  #output$check_val = renderText(unlist(start_pad()))

  output$tides = renderTable(
    map_high_low() %>%
      mutate(tide_date = format(tide_date)) %>%
      mutate(sea_tide_datetime = format(sea_tide_datetime))
  )

  # output$tides = renderTable(
  #   tide_pred() %>%
  #     mutate(tide_datetime = format(tide_datetime))
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
