server <- function(input, output, session) {
  
  # Store client session data
  cdata <- session$clientData
  
  # Debug session data display
  onclick("think_logo", showModal(debug_dialogue()))
  output$clientdataText <- renderText({
    cnames <- sort(names(cdata)) %>% .[. %!in% grep("^output_spinner.*$", ., value=T)]
    allvalues <- lapply(cnames, function(name) {
      paste(name, cdata[[name]], sep = " = ")
    })
    paste(allvalues, collapse = "\n")
  })
  
  # Think Logo
  output$think_logo <- renderImage({
    list(
      src = "www/Think_Logo_White.png",
      contentType = "image/png",
      height = 37,
      width = 100
    )
  }, deleteFile = F)
  
  # Loading Spinner Icon
  output$spinner <- renderUI({
    htmltools::HTML('<div class="loader"></div>')
  })
  
  # Start-up show database connection dialogue
  showModal(connection_dialogue())
  
  # Show database connection dialogue on button click
  onclick("db_button", showModal(connection_dialogue()))
  
  # Database connection
  con <- eventReactive(input$db_connect, {
    get_db_connection(input$db_driver, input$db_server, input$db_database, input$db_username, input$db_password)
  })
  
  # Database connection status feedback
  observeEvent(con(), {
    output$db_status <- renderUI({
      if (con() != -1L) {
        removeModal()
      } else {
        div(style="margin: 7px 0 0 6px;", "Error", icon("times-circle"))
      }
    })
  })
  
  # ----------------------------------------------------------------------- #
  # Database Tab ------------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  # Get custom query
  query <- eventReactive(input$db_execute, {
    input$db_query %>% sqlQuery(con(),.) %>% as.data.table()
  })
  
  # Render custom query table
  output$db_output <- DT::renderDataTable({
    datatable(
      query(),
      rownames = F, selection = "none", style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lf>rt<"dataTables_row"ip>')
    )
  }, server = T)
  
  # Clear query textbox
  observeEvent(input$db_clear, {
    updateTextAreaInput(session, "db_query", value="")
  })
  
  # Database stats
  
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
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  output$db_fp_table <- DT::renderDataTable({
    datatable(
      flightplan(),
      rownames = F, selection = "none", style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lf>rt<"dataTables_row"ip>')
    )
  }, server = T)
  
  # tbl_Landing_Pair
  landing_pairs <- reactive({
    lp <- "SELECT * FROM tbl_Landing_Pair" %>% sqlQuery(con(),.) %>% as.data.table()
    if (nrow(lp) > 0 & nrow(flightplan()) > 0) {
      lead <- flightplan(); names(lead) <- paste0("Leader_", names(lead))
      foll <- flightplan(); names(foll) <- paste0("Follower_", names(foll))
      x <- tryCatch(
        merge(merge(lp, lead, by = "Leader_Flight_Plan_ID"), foll, by = "Follower_Flight_Plan_ID"),
        error = function(e) NULL
      )
      if (!is.null(x)) {
        setcolorder(x, c(3, 4, 5, 2, seq(6, 19, 1), 1, seq(20, length(x), 1)))
      }
      return(x)
    } else {
      return(lp)
    }
  })
  output$db_lp_table <- DT::renderDataTable({
    datatable(
      landing_pairs(),
      rownames = F, selection = "none", style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lf>rt<"dataTables_row"ip>')
    )
  }, server = T)
  
  # tbl_Polygon
  volumes <- reactive({
    " SELECT * FROM tbl_Polygon
      LEFT JOIN (
        SELECT Volume_Name AS V2, Min_Altitude, Max_Altitude FROM tbl_Volume
      ) AS t ON Volume_Name = V2
    " %>% sqlQuery(con(),.) %>% as.data.table() %>% .[,!c("V2")]
  })
  output$db_volumes <- DT::renderDataTable({
    datatable(
      volumes(),
      rownames = F, selection = "none", style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lf>rt<"dataTables_row"ip>')
    )
  }, server = T)
  
  # tbl_Path_Leg
  legs <- reactive({
    " SELECT * FROM tbl_Path_Leg
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  output$db_legs <- DT::renderDataTable({
    datatable(
      legs(),
      rownames = F, selection = "none", style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lf>rt<"dataTables_row"ip>')
    )
  }, server = T)
  
  # Flight Plan Stats
  
  db_fp_stats <- reactive({
    fp_id <- " SELECT Flight_Plan_ID FROM tbl_Flight_Plan
    " %>% sqlQuery(con(),.) %>% unlist() %>% as.vector()
    fpd_id <- " SELECT Flight_Plan_ID FROM tbl_Flight_Plan_Derived
    " %>% sqlQuery(con(),.) %>% unlist() %>% as.vector()
    both_id <- intersect(fp_id, fpd_id)
    track_fpid <-  " SELECT DISTINCT Flight_Plan_ID FROM tbl_Radar_Track_Point
    " %>% sqlQuery(con(),.) %>% unlist() %>% as.vector()
    if (length(fp_id) > 0) {
      
      fp_gen_list <- list(
        "tbl_Flight_Plan ID count" = length(fp_id),
        "tbl_Flight_Plan orphaned IDs" = paste(setdiff(fp_id, both_id),  collapse = ", "),
        "tbl_Flight_Plan_Derived ID count" = length(fpd_id),
        "tbl_Flight_Plan_Derived orphaned IDs" = paste(setdiff(fpd_id, both_id),  collapse = ", "),
        "IDs missing Time_At_4DME" = length(flightplan()$Time_At_4DME %>% .[is.na(.)]),
        "IDs missing Time_At_1DME" = length(flightplan()$Time_At_1DME %>% .[is.na(.)]),
        "IDs missing WTC" = length(flightplan()$Wake_Vortex %>% .[is.na(.)]),
        "Aircraft Types without WTC" = paste(unique(flightplan()[is.na(Wake_Vortex)]$Aircraft_Type), collapse = ", "),
        "Flightplan IDs missing track point" = length(setdiff(both_id, track_fpid)),
        "Track point IDs missing flightplan" = length(setdiff(track_fpid, both_id))
      )
      
      fp_gen <- data.table(Name = names(fp_gen_list), Value = fp_gen_list)
      
      fp_ac <- as.data.table(table(flightplan()$Aircraft_Type))[order(V1)]
      names(fp_ac) <- c("Aircraft Type", "Count")
      fp_ac$`Percentage (Numeric)` <- fp_ac$Count / sum(fp_ac$Count)
      fp_ac$`Percentage (String)` <- paste0(round(fp_ac$Count / sum(fp_ac$Count) * 100, 3), "%")
      
      fp_wake <- as.data.table(table(flightplan()$Wake_Vortex))
      names(fp_wake) <- c("Wake Cat", "Count")
      fp_wake$`Wake Cat` <- factor(fp_wake$`Wake Cat`, levels = c("J", "H", "UM", "M", "S", "L", LETTERS[1:7], NA))
      fp_wake <- fp_wake[order(`Wake Cat`)]
      fp_wake$`Percentage (Numeric)` <- fp_wake$Count / sum(fp_wake$Count)
      fp_wake$`Percentage (String)` <- paste0(round(fp_wake$Count / sum(fp_wake$Count) * 100, 3), "%")
      
      fp_lrwy <- as.data.table(table(flightplan()$Landing_Runway))[order(V1)]
      names(fp_lrwy) <- c("Landing Runway", "Count")
      fp_lrwy$`Percentage (Numeric)` <- fp_lrwy$Count / sum(fp_lrwy$Count)
      fp_lrwy$`Percentage (String)` <- paste0(round(fp_lrwy$Count / sum(fp_lrwy$Count) * 100, 3), "%")
      
      fp_lrwyt <- as.data.table(table(flightplan()$Landing_Runway, as.numeric(flightplan()$FP_Time) %/% 3600))
      names(fp_lrwyt) <- c("Landing Runway", "Hour", "Count")
      fp_lrwyt$Hour <- as.numeric(fp_lrwyt$Hour)
      fp_lrwyt$Count <- paste0(fp_lrwyt$Count, " (", round(fp_lrwyt$Count / sum(fp_lrwyt$Count) * 100, 3), "%)")
      
      return(list(
        fp_gen = fp_gen,
        fp_ac = fp_ac,
        fp_wake = fp_wake,
        fp_lrwy = fp_lrwy,
        fp_lrwyt = tidyr::spread(fp_lrwyt, Hour, Count)
      ))
    } else {
      return(NA)
    }
  })
  output$db_fp_general_table <- DT::renderDataTable({
    datatable(
      db_fp_stats()[["fp_gen"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  output$db_fp_type_table <- DT::renderDataTable({
    datatable(
      db_fp_stats()[["fp_ac"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  output$db_fp_wake_table <- DT::renderDataTable({
    datatable(
      db_fp_stats()[["fp_wake"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  output$db_fp_lrwy_table <- DT::renderDataTable({
    datatable(
      db_fp_stats()[["fp_lrwy"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  output$db_fp_lrwyt_table <- DT::renderDataTable({
    datatable(
      db_fp_stats()[["fp_lrwyt"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  
  # Landing Pair Stats
  
  observeEvent(landing_pairs(), {
    db_lp_type_choices <- landing_pairs()$Landing_Pair_Type %>% as.character() %>% unique()
    updatePickerInput(
      session,
      "db_lp_type",
      choices = db_lp_type_choices,
      selected = db_lp_type_choices
    )
  })
  
  db_lp_stats <- reactive({
    req(input$db_lp_type)
    lp <- landing_pairs()[Landing_Pair_Type %in% input$db_lp_type]
    lp$Landing_Runway <- ifelse(
      lp$Leader_Landing_Runway == lp$Follower_Landing_Runway,
      as.character(lp$Leader_Landing_Runway),
      paste0(lp$Leader_Landing_Runway, "-", lp$Follower_Landing_Runway)
    )
    lp$Wake_Vortex <- paste0(lp$Leader_Wake_Vortex, "-", lp$Follower_Wake_Vortex)
    
    lp_wake <- as.data.table(table(lp$Wake_Vortex))
    names(lp_wake) <- c("Wake", "Count")
    lp_wake$`Percentage (Numeric)` <- lp_wake$Count / sum(lp_wake$Count)
    lp_wake$`Percentage (String)` <- paste0(round(lp_wake$Count / sum(lp_wake$Count) * 100, 3), "%")
    
    lp_rwy <- as.data.table(table(lp$Landing_Runway))[order(V1)]
    names(lp_rwy) <- c("Runway", "Count")
    lp_rwy$`Percentage (Numeric)` <- lp_rwy$Count / sum(lp_rwy$Count)
    lp_rwy$`Percentage (String)` <- paste0(round(lp_rwy$Count / sum(lp_rwy$Count) * 100, 3), "%")
    
    lp_wakerwy <- as.data.table(table(lp$Wake_Vortex, lp$Landing_Runway))
    names(lp_wakerwy) <- c("Wake", "Runway", "Count")
    lp_wakerwy$Count <- paste0(lp_wakerwy$Count, " (", round(lp_wakerwy$Count / sum(lp_wakerwy$Count) * 100, 3), "%)")
    
    return(list(
      lp_wake = lp_wake,
      lp_rwy = lp_rwy,
      lp_wakerwy = tidyr::spread(lp_wakerwy, Runway, Count)
    ))
  })
  output$db_lp_wake_table <- DT::renderDataTable({
    datatable(
      db_lp_stats()[["lp_wake"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  output$db_lp_lrwy_table <- DT::renderDataTable({
    datatable(
      db_lp_stats()[["lp_rwy"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  output$db_lp_wakerwy_table <- DT::renderDataTable({
    datatable(
      db_lp_stats()[["lp_wakerwy"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  
  # Adaptation Data Views
  
  db_aircraft_adaptation <- reactive({
    " EXEC usp_GI_Get_ORD_Aircraft_Adaptation_Data
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  output$db_aircraft_adaptation_table <- DT::renderDataTable({
    datatable(
      db_aircraft_adaptation(),
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  
  db_dbs_adaptation <- reactive({
    " EXEC usp_GI_Get_ORD_DBS_Adaptation_Data
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  output$db_dbs_adaptation_table <- DT::renderDataTable({
    datatable(
      db_dbs_adaptation(),
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  
  db_runway_adaptation <- reactive({
    " EXEC usp_GI_Get_ORD_Runway_Adaptation_Data 'CYYZ'
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  output$db_runway_adaptation_table <- DT::renderDataTable({
    datatable(
      db_runway_adaptation(),
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  
  db_wake_adaptation <- reactive({
    " EXEC usp_GI_Get_ORD_Wake_Adaptation_Data
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  output$db_wake_adaptation_table <- DT::renderDataTable({
    datatable(
      db_wake_adaptation(),
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  
  # ----------------------------------------------------------------------- #
  # PLT Tab -----------------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  # Render PLT map tiles
  output$pltmap <- renderLeaflet({
    x <- leaflet(options = leafletOptions(zoomControl = F, preferCanvas = T)) %>%
      setView(lng = 0, lat = 0, zoom = 3)
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
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  
  observeEvent(map_centre(), {
    leafletProxy("pltmap") %>% setView(lng = map_centre()$Lon, lat = map_centre()$Lat, zoom = 10)
  })
  
  observeEvent(input$pltmap_fpid, {
    time_range <- sprintf(
      " SELECT MIN(Track_Time) AS Min_Time, MAX(Track_Time) AS Max_Time FROM tbl_Radar_Track_Point
        LEFT JOIN (
          SELECT Radar_Track_Point_ID AS Radar_Track_Point_ID_2, Corrected_Mode_C, Range_To_Threshold, Range_To_ILS, Path_Leg
          FROM tbl_Radar_Track_Point_Derived
        ) AS t ON Radar_Track_Point_ID = Radar_Track_Point_ID_2
        WHERE Flight_Plan_ID IN ('%s')
      ",
      paste(input$pltmap_fpid, collapse = "','")
    ) %>%
      sqlQuery(con(),.) %>%
      as.data.table()
    updateSliderInput(
      session,
      "pltmap_time_range",
      min = time_range$Min_Time,
      max = time_range$Max_Time,
      value = c(time_range$Min_Time, time_range$Max_Time)
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
      sqlQuery(con(),.) %>%
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
        pickerInput("pltmap_fpdate", "Select Date", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
        pickerInput("pltmap_fpid", "Select FP ID", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
        pickerInput("pltmap_legs", "Filter By Path Leg", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
        div(
          style = "display: flex; justify-content: space-between;",
          actionButton("pltmap_clear_tracks", "Clear tracks"),
          actionButton("pltmap_plot_tracks", "Plot tracks")
        ),
        hr(),
        pickerInput("pltmap_volumes", "Display Volumes", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
        style = "minimal", icon = icon("plane"),
        tooltip = tooltipOptions(title = "Display Options", placement = "right")
      ),
      div(style = "height: 5px"),
      dropdown(
        div(style = "text-align: center", tags$b("Track Marker Settings")),
        sliderTextInput("pltmap_marker_radius", "Radius", choices=seq(1, 50, 1), selected=5, width="220px"),
        sliderTextInput("pltmap_marker_weight", "Weight", choices=seq(1, 50, 1), selected=5, width="220px"),
        sliderTextInput("pltmap_marker_opacity", "Opacity", choices=seq(0, 1, 0.01), selected=0.85, width="220px"),
        sliderTextInput("pltmap_marker_fillopacity", "Fill Opacity", choices=seq(0, 1, 0.01), selected=0.85, width="220px"),
        pickerInput("pltmap_colour", "Colour Data", c("Path_Leg", "Mode_C", "Corrected_Mode_C"), selected="Path Leg", width="220px"),
        pickerInput("pltmap_marker_palette", "Colour Palette", rownames(brewer.pal.info), selected="Spectral", options = list(`live-search` = T), width="220px"),
        style = "minimal", icon = icon("bullseye"),
        tooltip = tooltipOptions(title = "Marker Settings", placement = "right")
      ),
      div(style = "height: 5px"),
      dropdown(
        div(style = "text-align: center", tags$b("Volume Polygon Settings")),
        div(style = "height: 5px;"),
        div(
          style = "display: flex; flex-direction: row; flex-wrap: wrap; justify-content: space-around; height: 320px; width: 460px",
          sliderTextInput("pltmap_volume_weight", "Weight", choices=seq(1, 50, 1), selected=5, width="220px"),
          sliderTextInput("pltmap_volume_highlightweight", "Highlight Weight", choices=seq(1, 50, 1), selected=3, width="220px"),
          sliderTextInput("pltmap_volume_opacity", "Opacity", choices=seq(0, 1, 0.01), selected=0.5, width="220px"),
          sliderTextInput("pltmap_volume_highlightopacity", "Highlight Opacity", choices=seq(0, 1, 0.01), selected=0.5, width="220px"),
          sliderTextInput("pltmap_volume_fillopacity", "Fill Opacity", choices=seq(0, 1, 0.01), selected=0.1, width="220px"),
          sliderTextInput("pltmap_volume_highlightfillopacity", "Highlight Fill Opacity", choices=seq(0, 1, 0.01), selected=0.5, width="220px"),
          sliderTextInput("pltmap_volume_dash", "Dash Size", choices=seq(1, 50, 1), selected=5, width="220px")
        ),
        div(
          style = "display: inline-flex; flex-direction: column; flex-wrap: wrap; justify-content: space-between; height: 80px; width: 460px",
          div(style = "padding-left: 23px;", tags$b("Colour")),
          div(
            style = "display: flex; justify-content: center; vertical-align: middle; margin-top: -18px;",
            div(style = "padding: 26px 3px 0 10px", "R"),
            textInput("pltmap_volume_colour_r", "", 255, width="46px"),
            div(style = "padding: 26px 3px 0 10px", "G"),
            textInput("pltmap_volume_colour_g", "", 0, width="46px"),
            div(style = "padding: 26px 3px 0 10px", "B"),
            textInput("pltmap_volume_colour_b", "", 0, width="46px")
          ),
          div(style = "padding-left: 23px;", tags$b("Highlight Colour")),
          div(
            style = "display: flex; justify-content: center; vertical-align: middle; margin-top: -18px;",
            div(style = "padding: 26px 3px 0 10px", "R"),
            textInput("pltmap_volume_highlightcolour_r", "", 128, width="46px"),
            div(style = "padding: 26px 3px 0 10px", "G"),
            textInput("pltmap_volume_highlightcolour_g", "", 0, width="46px"),
            div(style = "padding: 26px 3px 0 10px", "B"),
            textInput("pltmap_volume_highlightcolour_b", "", 0, width="46px")
          )
        ),
        style = "minimal", icon = icon("draw-polygon"),
        tooltip = tooltipOptions(title = "Polygon Settings", placement = "right")
      ),
      div(style = "height: 5px"),
      dropdown(
        div(
          style = "width: calc(100vw - 110px); height: 100%;",
          DT::dataTableOutput("plt_tracks")
        ),
        style = "minimal", icon = icon("route"),
        tooltip = tooltipOptions(title = "Plotted Tracks Table", placement = "right")
      ),
      div(style = "height: 5px"),
      actionBttn("pltmap_toggle_timefilter", NULL, style = "minimal", icon = icon("clock")),
      div(style = "height: 5px"),
      actionBttn("pltmap_clearmarker", NULL, style = "minimal", icon = icon("eraser")),
      div(style = "height: 5px"),
      downloadButton("pltmap_screenshot", NULL, class = "bttn-minimal")
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
    ) %>% sqlQuery(con(),.) %>% unlist() %>% as.vector() %>% as.character()
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
          lapply(names(tracks()), function(x) tracks()[[x]])
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
        "pltmap_time_range",
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
    write_clip(paste(clat, clon))
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
