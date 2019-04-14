# server.R

#===========================================================================================
# Notes:
#
#
#  ToDo:
#  1.
#
# AS 2019-04-02
#===========================================================================================

# Server code
shinyServer(function(input, output, session) {

  # Output leaflet bidn map
  output$beach_map <- renderLeaflet({
    m = leaflet() %>%
      addTiles() %>%
      fitBounds(-122.12, 47.0, -124.79, 49.0) %>%
      addProviderTiles("Esri.OceanBasemap", group = "Esri Ocean basemap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Open Street Map") %>%
      addLayersControl(position = 'bottomright',
                       baseGroups = c("Esri Ocean basemap", "Esri World Imagery",
                                      "Open Street Map"),
                       options = layersControlOptions(collapsed = TRUE))
    m
  })

  # Filter stations
  site_filter = reactive({
    sites %>%
      filter(Station == location)
  })

  # filter_data <- reactive({
  #   sites[which(sites$Station == station$location), ]
  # })

  # Pull out labels
  station_label <- reactive({
    filter_data()$Station
  })

  pretty_label <- reactive({
    strsplit(station_label(), ",")[[1]][1] %>%
      sub(" ", "", .)
  })

  unit_label <- reactive({
    if(input$units == "meters") {
      return("Tide Height (m)")
    }
    "Tide Height (ft)"
  })

  tide_data <- reactive({
    req(station$location)
    data <- filter_data()
    station <- station_label()
    tz <- data$TZ

    data <- rtide::tide_height(
      station, from = input$from, to = input$to,
      minutes = input$interval, tz = tz)

    data$TideHeight %<>% round(2)
    data$TimeZone <- tz

    if(input$units == "meters"){
      return(data)
    }
    data$TideHeight <- round(data$TideHeight * 3.3333, 2)
    data
  })

  tide_plot <- reactive({
    data <- tide_data()
    time <- Sys.time()
    time %<>% lubridate::with_tz(tz = data$TimeZone[1])

    pad <- (max(data$TideHeight) - min(data$TideHeight))/7
    padrange <- c(min(data$TideHeight) - pad, max(data$TideHeight) + pad)

    data  <- data[,c('TideHeight', 'DateTime')] %>%
      setNames(c(unit_label(), "Date-Time"))
    xtsdata <- xts::xts(data, order.by = data$`Date-Time`)
    xtsdata$`Date-Time` <- NULL

    dygraph(xtsdata, height = "10px") %>%
      dyOptions(strokeWidth = 1.5, drawGrid = F, includeZero = F,
                useDataTimezone = T, drawGapEdgePoints = T, rightGap = 15) %>%
      dyRangeSelector() %>%
      dyAxis("y", valueRange = padrange,
             label = unit_label()) %>%
      dyEvent(x = time, label = "Current time", labelLoc = "bottom") %>%
      dyLegend()
  })

  tide_table <- reactive({
    data <- tide_data()
    data$Time <- strftime(data$DateTime, format = "%H:%M %p")
    data$Date <- strftime(data$DateTime, format = "%B %d, %Y")
    data[,c("Date", "Time", "TideHeight")] %>%
      setNames(c("Date", "Time", unit_label()))
  })

  ############### --------------- Reactive Values --------------- ###############
  station <- reactiveValues(location = NULL)

  observeEvent(input$search, {
    station$location <- input$search
  })

  observeEvent(input$leaflet_marker_click, {
    station$location <- input$leaflet_marker_click$id
  })

  ############### --------------- Observers --------------- ###############
  observeEvent(c(input$leaflet_marker_click, input$search), {
    req(station$location)
    toggleModal(session, "modal", "open")
  })

  # zoom to site on click or search
  observeEvent(input$zoom_to,
               {if(nrow(filter_data()) == 0L){return()}
                 leafletProxy('leaflet') %>%
                   setView(lat = filter_data()$Y, lng = filter_data()$X, zoom = click_zoom)})

  ############### --------------- Leaflet --------------- ###############
  # Zoom control
  observeEvent(input$map_zoom_out ,{
    leafletProxy("leaflet") %>%
      setView(lat  = (input$leaflet_bounds$north + input$leaflet_bounds$south) / 2,
              lng  = (input$leaflet_bounds$east + input$leaflet_bounds$west) / 2,
              zoom = input$leaflet_zoom - 1)
  })
  observeEvent(input$map_zoom_in ,{
    leafletProxy("leaflet") %>%
      setView(lat  = (input$leaflet_bounds$north + input$leaflet_bounds$south) / 2,
              lng  = (input$leaflet_bounds$east + input$leaflet_bounds$west) / 2,
              zoom = input$leaflet_zoom + 1)
  })

  # map
  output$leaflet <- leaflet::renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%

      setView(lat = initial_lat, lng = initial_long, zoom = initial_zoom) %>%

      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 1), group = "Satelite") %>%
      addTiles(urlTemplate = mapbox_moon, group = "Basemap") %>%

      addLayersControl(
        baseGroups = c("Basemap", "Satelite"),
        options = layersControlOptions(collapsed = TRUE),
        position = leaf.pos) %>%

      addMarkers(
        data = sites,
        lng = sites$X,
        lat = sites$Y,
        label = sites$Station,
        layerId = sites$Station,
        icon = makeIcon(
          iconUrl = "input/marker.png",
          iconWidth = 3.8*3, iconHeight = 5.1*3
        ),
        group = 'sites',
        clusterOptions = markerClusterOptions(showCoverageOnHover = F)
      ) %>%
      addEasyButton(easyButton(icon = "ion-arrow-shrink", position = leaf.pos,
                               title = "Reset View", onClick = JS(paste0("function(btn, map){ map.setView(new L.LatLng(", initial_lat, ", ", initial_long, "), ", initial_zoom, ", { animation: true });}"))))
    # leaflet::addMiniMap(position = "bottomleft",
    #                     zoomLevelOffset = -8,
    #                     toggleDisplay = T,
    #                     autoToggleDisplay = T, aimingRectOptions = list(weight = 1),
    #                     tiles =  mapbox_moon)  %>%
  })

  ############### --------------- Outputs --------------- ###############
  # plot
  output$plot <- renderDygraph({
    tide_plot()
  })

  # # close the R session when Chrome closes
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("no")
  # })
})
