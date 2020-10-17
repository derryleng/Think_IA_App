track_visualiser_server <- function(input, output, session, con) {
  
  ns <- session$ns
  
  # tbl_Flight_Plan
  flightplan <- reactive({
    " SELECT
      	t1.Flight_Plan_ID,
      	t1.FP_Date,
      	t1.FP_Time,
      	t1.Callsign,
      	t1.SSR_Code,
      	t1.Aircraft_Type,
      	t3.Wake AS \"Wake_Vortex\",
      	t1.Origin,
      	t1.Destination,
      	t1.STAR,
      	t1.SID,
      	t1.Landing_Runway,
      	t1.Departure_Runway,
      	t2.Time_At_4DME,
      	t2.Time_At_1DME
      FROM tbl_Flight_Plan AS t1
      LEFT JOIN (
      	SELECT Flight_Plan_ID AS Flight_Plan_ID, Time_At_4DME, Time_At_1DME FROM tbl_Flight_Plan_Derived
      ) AS t2 ON t1.Flight_Plan_ID = t2.Flight_Plan_ID
      LEFT JOIN (
      	SELECT * FROM tbl_Aircraft_Type_To_Wake
      ) AS t3 ON t1.Aircraft_Type = t3.Aircraft_Type
    " %>% sqlQuery(con,.) %>% as.data.table()
  })
  
  # tbl_Polygon
  volumes <- reactive({
    " SELECT * FROM tbl_Polygon
      LEFT JOIN (
        SELECT Volume_Name AS V2, Min_Altitude, Max_Altitude FROM tbl_Volume
      ) AS t ON Volume_Name = V2
    " %>% sqlQuery(con,.) %>% as.data.table() %>% .[,!c("V2")]
  })

  # tbl_Path_Leg
  legs <- reactive({
    " SELECT * FROM tbl_Path_Leg
    " %>% sqlQuery(con,.) %>% as.data.table()
  })
  
  # Render PLT map tiles
  output$pltmap <- renderLeaflet({
    x <- leaflet(options = leafletOptions(zoomControl = F, preferCanvas = T)) %>%
      setView(lng = map_centre()$Lon, lat = map_centre()$Lat, zoom = 10)
    tile_providers <- list(
      `Esri Satellite` = "Esri.WorldImagery",
      `CartoDB Light` = "CartoDB.Positron",
      `CartoDB Light 2` = "CartoDB.PositronNoLabels",
      `CartoDB Dark` = "CartoDB.DarkMatter",
      `CartoDB Dark 2` = "CartoDB.DarkMatterNoLabels",
      `OSM Mapnik` = "OpenStreetMap.Mapnik"
    )
    for (i in 1:length(tile_providers)) {
      x <- x %>% addProviderTiles(providers[[tile_providers[[i]]]], options = providerTileOptions(noWrap = T), group = names(tile_providers)[i])
    }
    x <- x %>% addLayersControl(baseGroups = names(tile_providers), options = layersControlOptions(collapsed = T))
  })
  
  # Map centering
  map_centre <- reactive({
    " SELECT
      	Grid_Projection_Origin_Lat/PI()*180 AS Lat,
      	Grid_Projection_Origin_Lon/PI()*180 AS Lon
      FROM tbl_Adaptation_Data
    " %>% sqlQuery(con,.) %>% as.data.table()
  })
  
  time_range <- reactive({
    sprintf(
      " SELECT MIN(Track_Time) AS Min_Time, MAX(Track_Time) AS Max_Time
        FROM tbl_Radar_Track_Point
        WHERE Flight_Plan_ID IN ('%s')
      ",
      paste(input$pltmap_fpid, collapse = "','")
    ) %>%
      sqlQuery(con,.) %>%
      as.data.table()
  })
  
  observe({
    updateSliderInput(
      session,
      ns("pltmap_time_range"),
      min = time_range()$Min_Time,
      max = time_range()$Max_Time,
      value = c(time_range()$Min_Time, time_range()$Max_Time)
    )
  })
  
  # Subsetted tbl_Radar_Track_Point
  tracks_full <- eventReactive(input$pltmap_plot_tracks, {
    sprintf(
      " SELECT * FROM tbl_Radar_Track_Point
        LEFT JOIN (
          SELECT Radar_Track_Point_ID AS Radar_Track_Point_ID_2, Corrected_Mode_C, Range_To_Threshold, Range_To_ILS, Path_Leg
          FROM tbl_Radar_Track_Point_Derived
        ) AS t ON Radar_Track_Point_ID = Radar_Track_Point_ID_2
        WHERE Flight_Plan_ID IN ('%s')
      ",
      paste(input$pltmap_fpid, collapse = "','")
    ) %>%
      sqlQuery(con,.) %>%
      as.data.table()  %>%
      .[is.na(Path_Leg), Path_Leg := "NA"]
  })
  
  # Subsetted tbl_Radar_Track_Point
  tracks <- reactive({
    if (!is.null(input$pltmap_time_range)) {
      tracks_full()[Track_Time >= input$pltmap_time_range[1] & Track_Time <= input$pltmap_time_range[2]]
    } else {
      tracks_full()
    }
  })
  
  # PLT Map Top Left Dropdown & Screenshot Buttons
  output$pltmap_filters_ui <- renderUI({
    div(
      class = "pltmap-filters",
      dropdown(
        div(style = "font-weight: bold; padding-bottom: 10px", "Display Tracks"),
        pickerInput(ns("pltmap_fpdate"), "Select Date", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
        pickerInput(ns("pltmap_fpid"), "Select FP ID", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
        pickerInput(ns("pltmap_legs"), "Filter By Path Leg", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
        div(
          style = "display: flex; justify-content: space-between;",
          actionButton(ns("pltmap_clear_tracks"), "Clear tracks"),
          actionButton(ns("pltmap_plot_tracks"), "Plot tracks")
        ),
        hr(),
        pickerInput(ns("pltmap_volumes"), "Display Volumes", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
        style = "minimal", icon = icon("plane"),
        tooltip = tooltipOptions(title = "Display Options", placement = "right")
      ),
      div(style = "height: 5px"),
      dropdown(
        div(style = "text-align: center", tags$b("Track Marker Settings")),
        sliderTextInput(ns("pltmap_marker_radius"), "Radius", choices=seq(1, 50, 1), selected=5, width="220px"),
        sliderTextInput(ns("pltmap_marker_weight"), "Weight", choices=seq(1, 50, 1), selected=5, width="220px"),
        sliderTextInput(ns("pltmap_marker_opacity"), "Opacity", choices=seq(0, 1, 0.01), selected=0.85, width="220px"),
        sliderTextInput(ns("pltmap_marker_fillopacity"), "Fill Opacity", choices=seq(0, 1, 0.01), selected=0.85, width="220px"),
        pickerInput(ns("pltmap_colour"), "Colour Data", c("Path_Leg", "Mode_C", "Corrected_Mode_C"), selected="Path Leg", width="220px"),
        pickerInput(ns("pltmap_marker_palette"), "Colour Palette", rownames(brewer.pal.info), selected="Spectral", options = list(`live-search` = T), width="220px"),
        style = "minimal", icon = icon("bullseye"),
        tooltip = tooltipOptions(title = "Marker Settings", placement = "right")
      ),
      div(style = "height: 5px"),
      dropdown(
        div(style = "text-align: center", tags$b("Volume Polygon Settings")),
        div(style = "height: 5px;"),
        div(
          style = "display: flex; flex-direction: row; flex-wrap: wrap; justify-content: space-around; height: 320px; width: 460px",
          sliderTextInput(ns("pltmap_volume_weight"), "Weight", choices=seq(1, 50, 1), selected=5, width="220px"),
          sliderTextInput(ns("pltmap_volume_highlightweight"), "Highlight Weight", choices=seq(1, 50, 1), selected=3, width="220px"),
          sliderTextInput(ns("pltmap_volume_opacity"), "Opacity", choices=seq(0, 1, 0.01), selected=0.5, width="220px"),
          sliderTextInput(ns("pltmap_volume_highlightopacity"), "Highlight Opacity", choices=seq(0, 1, 0.01), selected=0.5, width="220px"),
          sliderTextInput(ns("pltmap_volume_fillopacity"), "Fill Opacity", choices=seq(0, 1, 0.01), selected=0.1, width="220px"),
          sliderTextInput(ns("pltmap_volume_highlightfillopacity"), "Highlight Fill Opacity", choices=seq(0, 1, 0.01), selected=0.5, width="220px"),
          sliderTextInput(ns("pltmap_volume_dash"), "Dash Size", choices=seq(1, 50, 1), selected=5, width="220px")
        ),
        div(
          style = "display: inline-flex; flex-direction: column; flex-wrap: wrap; justify-content: space-between; height: 80px; width: 460px",
          div(style = "padding-left: 23px;", tags$b("Colour")),
          div(
            style = "display: flex; justify-content: center; vertical-align: middle; margin-top: -18px;",
            div(style = "padding: 26px 3px 0 10px", "R"),
            textInput(ns("pltmap_volume_colour_r"), "", 255, width="46px"),
            div(style = "padding: 26px 3px 0 10px", "G"),
            textInput(ns("pltmap_volume_colour_g"), "", 0, width="46px"),
            div(style = "padding: 26px 3px 0 10px", "B"),
            textInput(ns("pltmap_volume_colour_b"), "", 0, width="46px")
          ),
          div(style = "padding-left: 23px;", tags$b("Highlight Colour")),
          div(
            style = "display: flex; justify-content: center; vertical-align: middle; margin-top: -18px;",
            div(style = "padding: 26px 3px 0 10px", "R"),
            textInput(ns("pltmap_volume_highlightcolour_r"), "", 128, width="46px"),
            div(style = "padding: 26px 3px 0 10px", "G"),
            textInput(ns("pltmap_volume_highlightcolour_g"), "", 0, width="46px"),
            div(style = "padding: 26px 3px 0 10px", "B"),
            textInput(ns("pltmap_volume_highlightcolour_b"), "", 0, width="46px")
          )
        ),
        style = "minimal", icon = icon("draw-polygon"),
        tooltip = tooltipOptions(title = "Polygon Settings", placement = "right")
      ),
      div(style = "height: 5px"),
      dropdown(
        div(
          style = "width: calc(100vw - 110px); height: 100%;",
          DT::dataTableOutput(ns("plt_tracks"))
        ),
        style = "minimal", icon = icon("route"),
        tooltip = tooltipOptions(title = "Plotted Tracks Table", placement = "right")
      ),
      div(style = "height: 5px"),
      actionBttn(ns("pltmap_toggle_timefilter"), NULL, style = "minimal", icon = icon("clock")),
      div(style = "height: 5px"),
      actionBttn(ns("pltmap_clearmarker"), NULL, style = "minimal", icon = icon("eraser")),
      div(style = "height: 5px"),
      downloadButton(ns("pltmap_screenshot"), NULL, class = "bttn-minimal")
    )
  })
  
  # Update Date Filter Choices
  observeEvent(flightplan(), {
    updatePickerInput(
      session,
      "pltmap_fpdate",
      choices = flightplan()$FP_Date %>% as.character() %>% unique() %>% .[order(as.Date(., format="%d/%m/%Y"))]
    )
  })
  
  # Update FPID Filter Choices
  observeEvent(input$pltmap_fpdate, {
    track_fpid_check <- sprintf(
      " SELECT DISTINCT Flight_Plan_ID FROM tbl_Radar_Track_Point
        WHERE Track_Date IN ('%s')
      ", paste(input$pltmap_fpdate %>% as.character(), collapse = "','")
    ) %>% sqlQuery(con,.) %>% unlist() %>% as.vector() %>% as.character()
    pltmap_fpid_choices <- flightplan()[FP_Date %in% input$pltmap_fpdate, c("Flight_Plan_ID", "Callsign")][order(Flight_Plan_ID)] %>% unique()
    updatePickerInput(
      session,
      "pltmap_fpid",
      choices = pltmap_fpid_choices$Flight_Plan_ID %>% as.character(),
      choicesOpt = list(
        subtext = pltmap_fpid_choices$Callsign %>% as.character(),
        style = ifelse(
          pltmap_fpid_choices$Flight_Plan_ID %>% as.character() %in% track_fpid_check, 
          "border-left: 5px solid green;",
          "border-left: 5px solid red;"
        )
      )
    )
  })
  
  # Update Volume Filter Choices
  observeEvent(volumes(), {
    updatePickerInput(
      session,
      "pltmap_volumes",
      choices = volumes()$Volume_Name %>% as.character() %>% unique()
    )
  })
  
  # Update Path Leg Filter Choices
  observeEvent(legs(), {
    pltmap_legs_choices <- legs()$Path_Leg_Name %>% as.character() %>% c(., "NA")
    updatePickerInput(
      session,
      "pltmap_legs",
      choices = pltmap_legs_choices,
      selected = pltmap_legs_choices,
      choicesOpt = list(subtext = legs()$Path_Leg_Type %>% as.character())
    )
  })
  
  # PLT Map Track Point Labels
  pltmap_lab <- reactive({
    if (dim(tracks())[1] != 0 & dim(tracks())[2] != 0) {
      do.call(
        sprintf, c(
          list(paste(paste0("<b>", names(tracks()), "</b>: %s"), collapse = "<br/>")),
          lapply(names(tracks()), function(x) {
            if (grepl("Lat", x) | grepl("Lon", x)) {
              paste0(round(tracks()[[x]] * 180 / pi, 5), " (Cartesian)")
            } else if (grepl("HDG", x) | grepl("Angle", x)) {
              paste0(round(tracks()[[x]] * 180 / pi, 2), " (deg)")
            } else if (grepl("SPD", x) | grepl("IAS", x) | grepl("TAS", x)) {
              paste0(round(tracks()[[x]] / 1852 * 3600, 2), " (kts)")
            } else if (grepl("Range", x)) {
              paste0(round(tracks()[[x]] / 1852, 2), " (NM)")
            } else if (grepl("Mode_C", x)) {
              paste0(round(tracks()[[x]] / 0.3048, 2), " (ft)")
            } else if (grepl("X_Pos", x) | grepl("Y_Pos", x)) {
              paste0(round(tracks()[[x]], 2), " (m)")
            } else {
              tracks()[[x]]
            }
          })
        )
      ) %>% lapply(htmltools::HTML)
    } else {
      NULL
    }
  })
  
  # Update PLT Map Volume Colour Selection (Red)
  observeEvent(input$pltmap_volume_colour_r, {
    x <- input$pltmap_volume_colour_r %>% as.numeric() %>% ifelse(is.na(.), -1, .)
    if (x > 255 | x < 0) updateTextInput(session, "pltmap_volume_colour_r", value=0)
  })
  
  # Update PLT Map Volume Colour Selection (Green)
  observeEvent(input$pltmap_volume_colour_g, {
    x <- input$pltmap_volume_colour_g %>% as.numeric() %>% ifelse(is.na(.), -1, .)
    if (x > 255 | x < 0) updateTextInput(session, "pltmap_volume_colour_g", value=0)
  })
  
  # Update PLT Map Volume Colour Selection (Blue)
  observeEvent(input$pltmap_volume_colour_b, {
    x <- input$pltmap_volume_colour_b %>% as.numeric() %>% ifelse(is.na(.), -1, .)
    if (x > 255 | x < 0) updateTextInput(session, "pltmap_volume_colour_b", value=0)
  })
  
  # Update PLT Map Volume Highlight Colour Selection (Red)
  observeEvent(input$pltmap_volume_highlightcolour_r, {
    x <- input$pltmap_volume_highlightcolour_r %>% as.numeric() %>% ifelse(is.na(.), -1, .)
    if (x > 255 | x < 0) updateTextInput(session, "pltmap_volume_highlightcolour_r", value=0)
  })
  
  # Update PLT Map Volume Highlight Colour Selection (Green)
  observeEvent(input$pltmap_volume_highlightcolour_g, {
    x <- input$pltmap_volume_highlightcolour_g %>% as.numeric() %>% ifelse(is.na(.), -1, .)
    if (x > 255 | x < 0) updateTextInput(session, "pltmap_volume_highlightcolour_g", value=0)
  })
  
  # Update PLT Map Volume Highlight Colour Selection (Blue)
  observeEvent(input$pltmap_volume_highlightcolour_b, {
    x <- input$pltmap_volume_highlightcolour_b %>% as.numeric() %>% ifelse(is.na(.), -1, .)
    if (x > 255 | x < 0) updateTextInput(session, "pltmap_volume_highlightcolour_b", value=0)
  })
  
  # Get PLT Map Volume Colour Based on RGB Selection
  pltmap_volume_colour <- reactive({
    r <- input$pltmap_volume_colour_r
    g <- input$pltmap_volume_colour_g
    b <- input$pltmap_volume_colour_b
    return(paste0("rgb(",r,",",g,",",b,")"))
  })
  
  # Get PLT Map Volume Highlight Colour Based on RGB Selection
  pltmap_volume_highlightcolour <- reactive({
    r <- input$pltmap_volume_highlightcolour_r
    g <- input$pltmap_volume_highlightcolour_g
    b <- input$pltmap_volume_highlightcolour_b
    return(paste0("rgb(",r,",",g,",",b,")"))
  })
  
  # Placeholder for additional PLT map elements displayed (for screenshotting)
  update_pltmap <- reactiveValues(moved = F, markers = NULL, volumes = NULL)
  
  # Plot tracks
  observe({
    if (any(is.na(input$pltmap_legs))) {
      d <- tracks()[Path_Leg %in% input$pltmap_legs | is.na(Path_Leg)]
    } else {
      d <- tracks()[Path_Leg %in% input$pltmap_legs]
    }
    p <- leafletProxy("pltmap", data=d) %>% clearGroup("Tracks") %>% removeControl("Legend")
    pal <- if (input$pltmap_colour == "Path_Leg") {
      colorFactor(brewer.pal(11, input$pltmap_marker_palette), domain=legs()$Path_Leg_Name)
    } else {
      colorNumeric(brewer.pal(11, input$pltmap_marker_palette), domain=d[[input$pltmap_colour]])
    }
    p %>% addCircleMarkers(
      lng = ~Lon*180/pi,
      lat = ~Lat*180/pi,
      color = ~pal(eval(parse(text=input$pltmap_colour))),
      label=pltmap_lab(),
      labelOptions=labelOptions(textsize="13px", direction="auto"),
      weight=input$pltmap_marker_weight,
      radius=input$pltmap_marker_radius,
      stroke=T,
      opacity=input$pltmap_marker_opacity,
      fillOpacity=input$pltmap_marker_fillopacity,
      group="Tracks"
    ) %>%
      addLegend(
        position = "bottomleft",
        title = input$pltmap_colour,
        pal = pal,
        values = ~eval(parse(text=input$pltmap_colour)),
        opacity = 0.85,
        layerId = "Legend"
      )
    update_pltmap$markers <- d
  })
  
  observeEvent(input$pltmap_clear_tracks, {
    leafletProxy("pltmap") %>% clearGroup("Tracks") %>% removeControl("Legend")
  })
  
  # Plot volumes
  observe({
    p <- leafletProxy("pltmap") %>% clearGroup("Volumes")
    # pal <- colorFactor(brewer.pal(11, "Spectral"), domain=volumes()$Volume_Name)
    for (i in input$pltmap_volumes) {
      p %>% addPolygons(
        data = Polygon(
          volumes()[Volume_Name %in% i, c("Longitude","Latitude")][,':='(Latitude=Latitude*180/pi, Longitude=Longitude*180/pi)]
        ),
        weight = input$pltmap_volume_weight,
        opacity = input$pltmap_volume_opacity,
        fillOpacity = input$pltmap_volume_fillopacity,
        color = pltmap_volume_colour(),
        dashArray = paste0(input$pltmap_volume_dash),
        label = i,
        labelOptions = labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
        highlight = highlightOptions(
          weight = input$pltmap_volume_highlightweight,
          color = pltmap_volume_highlightcolour(),
          dashArray = "",
          opacity = input$pltmap_volume_highlightopacity,
          fillOpacity = input$pltmap_volume_highlightfillopacity,
          bringToFront = F
        ),
        group = "Volumes"
      )
    }
    update_pltmap$volumes <- volumes()[Volume_Name %in% input$pltmap_volumes]
  })
  
  # Map screenshot functionality
  output$pltmap_screenshot <- downloadHandler(
    filename = function() {
      paste0("PLT_Map_", gsub(" ", "_", gsub("-|:", "", as.character(Sys.time()))),".png")
    },
    content = function(file) {
      
      p <- leaflet(options = leafletOptions(zoomControl = F, preferCanvas = T))
      
      # if (input$pltmap_groups == "Esri Satellite") {
      #   p <- p %>% addProviderTiles(providers$Esri.WorldImagery, options=providerTileOptions(noWrap=TRUE))
      # } else if (input$pltmap_groups == "CartoDB Light") {
      #   p <- p %>% addProviderTiles(providers$CartoDB.Positron, options=providerTileOptions(noWrap=TRUE))
      # } else if (input$pltmap_groups == "CartoDB Light 2") {
      #   p <- p %>% addProviderTiles(providers$CartoDB.PositronNoLabels, options=providerTileOptions(noWrap=TRUE))
      # } else if (input$pltmap_groups == "CartoDB Dark") {
      #   p <- p %>% addProviderTiles(providers$CartoDB.DarkMatter, options=providerTileOptions(noWrap=TRUE))
      # } else if (input$pltmap_groups == "CartoDB Dark 2") {
      #   p <- p %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels, options=providerTileOptions(noWrap=TRUE))
      # } else if (input$pltmap_groups == "OSM Mapnik") {
      #   p <- p %>% addProviderTiles(providers$OpenStreetMap.Mapnik, options=providerTileOptions(noWrap=TRUE))
      # }
      
      p <- p %>% addProviderTiles(providers$Esri.WorldImagery, options=providerTileOptions(noWrap=TRUE))
      
      p <- p %>% setView(lng = input$pltmap_center$lng, lat = input$pltmap_center$lat, zoom = input$pltmap_zoom)
      
      if (!is.null(update_pltmap$markers)) {
        pal <- colorFactor(brewer.pal(11, input$pltmap_marker_palette), domain=legs()$Path_Leg_Name)
        p <- p %>%
          clearGroup("Tracks") %>%
          addCircleMarkers(
            data = update_pltmap$markers,
            lng = ~Lon*180/pi,
            lat = ~Lat*180/pi,
            color = ~pal(Path_Leg),
            weight=input$pltmap_marker_weight,
            radius=input$pltmap_marker_radius,
            stroke=T,
            opacity=input$pltmap_marker_opacity,
            fillOpacity=input$pltmap_marker_fillopacity,
            group="Tracks"
          )
        # p <- p %>%
        #   clearGroup("Legend") %>%
        #   addLegend(
        #     position = "bottomleft",
        #     title = input$pltmap_colour,
        #     pal = pal,
        #     values = ~eval(parse(text=input$pltmap_colour)),
        #     opacity = 0.85,
        #     layerId = "Legend"
        #   )
      }
      
      if (!is.null(update_pltmap$volumes)) {
        p <- p %>% clearGroup("Volumes")
        for (i in unique(update_pltmap$volumes$Volume_Name)) {
          p <- p %>% addPolygons(
            data = Polygon(
              update_pltmap$volumes[Volume_Name %in% i, c("Longitude","Latitude")][,':='(Latitude=Latitude*180/pi, Longitude=Longitude*180/pi)]
            ),
            weight = input$pltmap_volume_weight,
            opacity = input$pltmap_volume_opacity,
            fillOpacity = input$pltmap_volume_fillopacity,
            color = pltmap_volume_colour(),
            dashArray = paste0(input$pltmap_volume_dash),
            group = "Volumes"
          )
        }
      }
      
      mapshot(p, file = file, vwidth = input$pltDim[1], vheight = input$pltDim[2], delay = 0)
      
    },
    contentType = "image/png"
  )
  
  # Render subsetted tracks table
  output$plt_tracks <- DT::renderDataTable({
    datatable(
      tracks(),
      rownames = F, selection = "none", style = "bootstrap4", options = list(pageLength = 10, lengthMenu = seq(5, 10, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lf>rt<"dataTables_row"ip>')
    )
  }, server = T)
  
  # Time range filter
  
  output$pltmap_time_range_ui <- renderUI({
    div(
      class = "centered",
      style = "
        position: relative;
        bottom: 85px;
        z-index: 1000;
        height: 0
      ",
      sliderInput(
        ns("pltmap_time_range"),
        NULL,
        min = NA,
        max = NA,
        value = c(NA, NA),
        step = 1,
        round = T,
        animate = animationOptions(interval = 100, loop = T),
        dragRange = T,
        width = "90%"
      )
    )
  })
  
  onclick("pltmap_toggle_timefilter", toggle("pltmap_time_range_ui"))
  
  observeEvent(input$pltmap_click, {
    click <- input$pltmap_click
    clat <- click$lat
    clon <- click$lng
    write_clip(paste(clat, clon), allow_non_interactive = T)
    lab <- sprintf("<b>Latitude</b>: %s<br/><b>Longitude</b>: %s", clat, clon) %>% lapply(htmltools::HTML)
    leafletProxy("pltmap") %>%
      addMarkers(
        clon,
        clat,
        label = lab,
        labelOptions = labelOptions(textsize="13px", direction="auto"),
        options = markerOptions(opacity = 0.7),
        group = "Clicked"
      )
  })
  
  observeEvent(input$pltmap_clearmarker, {
    leafletProxy("pltmap") %>% clearGroup("Clicked")
  })
  
}
