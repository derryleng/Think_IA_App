source("modules/plt_tools/SummaryStats_DL.R")

# Must be set to same name as input config files
tbl_names <- list(
  tbl_Runway = "03_Populate_tbl_Runway.csv",
  tbl_Volumes = "05_Populate_Airspace_Volumes.csv",
  tbl_Volumes_2 = "05_Populate_Airspace_Volumes_2.csv",
  tbl_Volumes_3 = "05_Populate_Airspace_Volumes_3.csv",
  tbl_Path_Leg = "07_Populate_tbl_Path_Leg.csv",
  tbl_Path_Leg_2 = "07_Populate_tbl_Path_Leg_2.csv",
  tbl_Path_Leg_3 = "07_Populate_tbl_Path_Leg_3.csv",
  tbl_Path_Leg_Transition = "08_Populate_tbl_Path_Leg_Transition.csv",
  tbl_Path_Leg_Transition_2 = "08_Populate_tbl_Path_Leg_Transition_2.csv",
  tbl_Path_Leg_Transition_3 = "08_Populate_tbl_Path_Leg_Transition_3.csv"
)

simple_map_labels <- function(data) {
  labs <- if (dim(data)[1] != 0 & dim(data)[2] != 0) {
    do.call(
      sprintf, c(
        list(paste(paste0("<b>", names(data), "</b>: %s"), collapse = "<br/>")),
        lapply(names(data), function(x) data[[x]])
      )
    ) %>% lapply(htmltools::HTML)
  } else {
    NULL
  }
  return(labs)
}

# This processing should be re-run after PLT adaptation data changes to
#  re-generate PLT data and comparison tables.
#
# Assumes the relevant initialisation scripts have been run
#  and the surveillance and flight data loaded
#  and the baseline processing has been run for the calendar date specified.
#
# For running PLT analysis for specified variants
plt_analysis_run <- function(variants) {
  # Set the processing date (Eg dd/mm/yy or null for all dates).
  query_str <- "
    DECLARE @Log_Date varchar(50)
    SET @Log_Date = null
  "
  # For each variant below, reset path leg values and clear PLT analysis output
  #  and re-generate the PLT path legs and generate
  #  search results for PLT Abnormal Transition Search and Analysis
  query_v1 <- "
    UPDATE tbl_Radar_Track_Point_Derived
    SET Path_Leg = null
    DELETE FROM tbl_PLT_Analysis_Report
    EXEC usp_DF_Derive_RTP_Path_Leg @Log_Date
    EXEC usp_PLT_Generate_Analysis_Report @Log_Date
  "
  query_v2 <- "
    UPDATE tbl_Radar_Track_Point_Derived
    SET Path_Leg_2 = null
    DELETE FROM tbl_PLT_Analysis_Report_2
    EXEC usp_DF_Derive_RTP_Path_Leg_2 @Log_Date
    EXEC usp_PLT_Generate_Analysis_Report_2 @Log_Date
  "
  query_v3 <- "
    UPDATE tbl_Radar_Track_Point_Derived
    SET Path_Leg_3 = null
    DELETE FROM tbl_PLT_Analysis_Report_3
    EXEC usp_DF_Derive_RTP_Path_Leg_3 @Log_Date
    EXEC usp_PLT_Generate_Analysis_Report_3 @Log_Date
  "
  if (1 %in% variants) {
    query_str <- paste0(query_str, query_v1)
  }
  if (2 %in% variants) {
    query_str <- paste0(query_str, query_v2)
  }
  if (3 %in% variants) {
    query_str <- paste0(query_str, query_v3)
  }
  message("Executing PLT analysis run for variants ", paste(variants, collapse = ", "), "...")
  dbSendQuery(dbi_con, query_str)
  message("Finished executing PLT analysis.")
}

# Template ----------------------------------------------------------------

tbl_template <- list(
  tbl_Runway = data.table(
    Runway_Name = "R00",
    Airfield_Name = "ZZZZ",
    Heading = 0,
    Runway_Group = "RXX",
    Approach_Direction = "ZZZZ"
  ),
  tbl_Volumes = data.table(
    Volume_Name = "VOL00",
    Min_Altitude = 0,
    Max_Altitude = 0,
    Start_Dist_From_Threshold = 0,
    End_Dist_From_Threshold = 0,
    Lateral_Dist_Left = 0,
    Lateral_Dist_Right = 0
  ),
  tbl_Path_Leg = data.table(
    Path_Leg_Name = "PL00",
    Landing_Runway = "R00",
    Is_Intercept_Leg = F,
    Is_ILS_Leg = F,
    Is_Landing_Leg = F,
    Path_Leg_Type = "XXX"
  ),
  tbl_Path_Leg_Transition = data.table(
    Current_Path_Leg = "PL00",
    New_Path_Leg = "PL01",
    Min_Heading = 0,
    Max_Heading = 0,
    Volume_Name = "VOL00",
    Min_Sustained_RoCD = 0,
    Runway_Name = "R00",
    Associated_Runway = "R00"
  )
)

map_template <- function(type = 1) {
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
  if (type == 1) {
    x <- x %>% addLayersControl(
      baseGroups = names(tile_providers),
      options = layersControlOptions(collapsed = T)
    )
  } else {
    x <- x %>% addLayersControl(
      baseGroups = names(tile_providers),
      overlayGroups = c("Tracks", "Indicators", "Volumes"),
      options = layersControlOptions(collapsed = T)
    )
  }
  return(x)
}

plt_analysis_error_modal <- function(msg) {
  showModal(modalDialog(
    div(
      style = "text-align: center",
      h3("PLT Analysis Error")
    ),
    hr(),
    div(
      style = "text-align: center",
      tags$b(msg)
    ),
    size = "s",
    easyClose = T
  ))
}

# Server ------------------------------------------------------------------

plt_tools_server <- function(input, output, session, con, dbi_con) {
  
  ns <- session$ns
  
  tbl_Adaptation_Data <- reactive({
    as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Adaptation_Data"))
  })
  
  tbl_Runway <- reactive({
    as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Runway"))
  })
  
  vw_PLT_Detailed_Analysis <- reactive({
    d <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM vw_PLT_Detailed_Analysis"))
    names(d) <- gsub("_", " ", names(d))
    d[is.na(`Non Standard Transition V1`) | `Non Standard Transition V1` == ""]$`Non Standard Transition V1` <- "No Non-Standard Transitions"
    d[is.na(`Non Standard Transition V2`) | `Non Standard Transition V2` == ""]$`Non Standard Transition V2` <- "No Non-Standard Transitions"
    d[is.na(`Non Standard Transition V3`) | `Non Standard Transition V3` == ""]$`Non Standard Transition V3` <- "No Non-Standard Transitions"
    d$`Base Leg Altidude V1` <- round(d$`Base Leg Altidude V1`, 2)
    d$`Base Leg Altidude V2` <- round(d$`Base Leg Altidude V2`, 2)
    d$`Base Leg Altidude V3` <- round(d$`Base Leg Altidude V3`, 2)
    d$`Base Leg Lateral Distance V1` <- round(d$`Base Leg Lateral Distance V1`, 2)
    d$`Base Leg Lateral Distance V2` <- round(d$`Base Leg Lateral Distance V2`, 2)
    d$`Base Leg Lateral Distance V3` <- round(d$`Base Leg Lateral Distance V3`, 2)
    if ("Landing Runway" %in% names(d)) {
      d$`Landing Runway` <- d$`Landing Runway`
    }
    return(d)
  })
  
  volumes <- reactive({
    x <- as.data.table(dbGetQuery(dbi_con, "
      SELECT
          p.Volume_Name,
          Point_Sequence,
          Latitude,
          Longitude,
          Min_Altitude,
          Max_Altitude,
          'V1' AS Variant
      FROM tbl_Polygon AS p
      LEFT JOIN (
      SELECT Volume_Name, Min_Altitude, Max_Altitude FROM tbl_Volume
      ) AS v ON p.Volume_Name = v.Volume_Name
      UNION
      SELECT
          p.Volume_Name,
          Point_Sequence,
          Latitude,
          Longitude,
          Min_Altitude,
          Max_Altitude,
          'V2' AS Variant
      FROM tbl_Polygon_2 AS p
      LEFT JOIN (
      SELECT Volume_Name, Min_Altitude, Max_Altitude FROM tbl_Volume_2
      ) AS v ON p.Volume_Name = v.Volume_Name
      UNION
      SELECT
          p.Volume_Name,
          Point_Sequence,
          Latitude,
          Longitude,
          Min_Altitude,
          Max_Altitude,
          'V3' AS Variant
      FROM tbl_Polygon_3 AS p
      LEFT JOIN (
      SELECT Volume_Name, Min_Altitude, Max_Altitude FROM tbl_Volume_3
      ) AS v ON p.Volume_Name = v.Volume_Name
      ORDER BY Volume_Name, Variant, Point_Sequence                   
    "))
    x$Latitude <- as.numeric(x$Latitude) * 180 / pi
    x$Longitude <- as.numeric(x$Longitude) * 180 / pi
    return(x)
  })
  
  
  # PLT Adaptation Editor | Load UI elements --------------------------------

  tbl <- reactiveValues(
    loaded = F,
    tbl_Runway = tbl_template$tbl_Runway,
    tbl_Volumes = tbl_template$tbl_Volumes,
    tbl_Volumes_2 = tbl_template$tbl_Volumes,
    tbl_Volumes_3 = tbl_template$tbl_Volumes,
    tbl_Path_Leg = tbl_template$tbl_Path_Leg,
    tbl_Path_Leg_2 = tbl_template$tbl_Path_Leg,
    tbl_Path_Leg_3 = tbl_template$tbl_Path_Leg,
    tbl_Path_Leg_Transition = tbl_template$tbl_Path_Leg_Transition,
    tbl_Path_Leg_Transition_2 = tbl_template$tbl_Path_Leg_Transition,
    tbl_Path_Leg_Transition_3 = tbl_template$tbl_Path_Leg_Transition
  )

  observe({
    
    req(tbl$loaded)
    
    shinyjs::show("editor_export")
    
    output$editor_view <- renderUI({
      div(
        style = "display: flex;",
        
        div(
          style = "flex: 1; height: 600px; min-width: 50%; max-width: 50%;",
          div(style = "height: 15px;"),
          div(
            style = "display: flex; justify-content: flex-start; gap: 5px;",
            div(style = "padding: 8px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; min-width: 0;", "Additional Variants:"),
            div(style = "height: 34px;", numericInput(ns("plt_variants"), NULL, value = 0, min = 0, max = 2, step = 1))
          ),
          div(style = "height: 15px;"),
          tabBox(
            width = NULL,
            tabPanel(
              title = "Runways",
              rHandsontableOutput(outputId = ns("DT_tbl_Runway"))
            ),
            tabPanel(
              title = "Volumes",
              rHandsontableOutput(outputId = ns("DT_tbl_Volumes")),
              hidden(
                rHandsontableOutput(outputId = ns("DT_tbl_Volumes_2")),
                rHandsontableOutput(outputId = ns("DT_tbl_Volumes_3"))
              )
            ),
            tabPanel(
              title = "Path Legs",
              rHandsontableOutput(outputId = ns("DT_tbl_Path_Leg")),
              hidden(
                rHandsontableOutput(outputId = ns("DT_tbl_Path_Leg_2")),
                rHandsontableOutput(outputId = ns("DT_tbl_Path_Leg_3"))
              )
            ),
            tabPanel(
              title = "Path Leg Transitions",
              rHandsontableOutput(outputId = ns("DT_tbl_Path_Leg_Transition")),
              hidden(
                rHandsontableOutput(outputId = ns("DT_tbl_Path_Leg_Transition_2")),
                rHandsontableOutput(outputId = ns("DT_tbl_Path_Leg_Transition_3"))
              )
            )
          )
        ),
        
        div(
          style = "flex: 1; height: 600px; padding-top: 40px;",
          div(
            style = "display: flex; background: #D51067; margin-top: -40px",
            div(style = "padding: 10px; color: white", tags$b("PLT Adaptation Preview")),
            # dropdown(
            #   div(style = "font-weight: bold; padding-bottom: 10px", "Runways"),
            #   pickerInput_customised(ns("toggle_tbl_Runway"), NULL, choices = NULL),
            #   style = "minimal", icon = icon("road"),
            #   tooltip = tooltipOptions(title = "Runways", placement = "right")
            # ),
            dropdown(
              div(style = "font-weight: bold; padding-bottom: 10px", "Volumes"),
              pickerInput_customised(ns("toggle_tbl_Volumes"), "Variant 1", choices = NULL),
              hidden(
                pickerInput_customised(ns("toggle_tbl_Volumes_2"), "Variant 2", choices = NULL),
                pickerInput_customised(ns("toggle_tbl_Volumes_3"), "Variant 3", choices = NULL)
              ),
              style = "minimal", icon = icon("vector-square"),
              tooltip = tooltipOptions(title = "Volumes", placement = "right")
            )
          ),
          leafletOutput(ns("map"), height = "563px")
        )
        
      )
    })
    
    output$DT_tbl_Runway <- renderRHandsontable({
      if (!is.null(tbl$tbl_Runway)) rhandsontable(tbl$tbl_Runway)
    })

    output$DT_tbl_Volumes <- renderRHandsontable({
      if (!is.null(tbl$tbl_Volumes)) rhandsontable(tbl$tbl_Volumes)
    })

    output$DT_tbl_Volumes_2 <- renderRHandsontable({
      if (!is.null(tbl$tbl_Volumes_2)) rhandsontable(tbl$tbl_Volumes_2)
    })

    output$DT_tbl_Volumes_3 <- renderRHandsontable({
      if (!is.null(tbl$tbl_Volumes_3)) rhandsontable(tbl$tbl_Volumes_3)
    })

    output$DT_tbl_Path_Leg <- renderRHandsontable({
      if (!is.null(tbl$tbl_Path_Leg)) rhandsontable(tbl$tbl_Path_Leg)
    })

    output$DT_tbl_Path_Leg_2 <- renderRHandsontable({
      if (!is.null(tbl$tbl_Path_Leg_2)) rhandsontable(tbl$tbl_Path_Leg_2)
    })

    output$DT_tbl_Path_Leg_3 <- renderRHandsontable({
      if (!is.null(tbl$tbl_Path_Leg_3)) rhandsontable(tbl$tbl_Path_Leg_3)
    })

    output$DT_tbl_Path_Leg_Transition <- renderRHandsontable({
      if (!is.null(tbl$tbl_Path_Leg_Transition)) rhandsontable(tbl$tbl_Path_Leg_Transition)
    })

    output$DT_tbl_Path_Leg_Transition_2 <- renderRHandsontable({
      if (!is.null(tbl$tbl_Path_Leg_Transition_2)) rhandsontable(tbl$tbl_Path_Leg_Transition_2)
    })

    output$DT_tbl_Path_Leg_Transition_3 <- renderRHandsontable({
      if (!is.null(tbl$tbl_Path_Leg_Transition_3)) rhandsontable(tbl$tbl_Path_Leg_Transition_3)
    })
    
  })

  
  # PLT Adaptation Editor | Create New Adaptation ---------------------------
  
  observeEvent(input$editor_template, {
    tbl$loaded <- T
    tbl$tbl_Runway <- tbl_template$tbl_Runway
    tbl$tbl_Volumes <- tbl_template$tbl_Volumes
    tbl$tbl_Volumes_2 <- tbl_template$tbl_Volumes
    tbl$tbl_Volumes_3 <- tbl_template$tbl_Volumes
    tbl$tbl_Path_Leg <- tbl_template$tbl_Path_Leg
    tbl$tbl_Path_Leg_2 <- tbl_template$tbl_Path_Leg
    tbl$tbl_Path_Leg_3 <- tbl_template$tbl_Path_Leg
    tbl$tbl_Path_Leg_Transition <- tbl_template$tbl_Path_Leg_Transition
    tbl$tbl_Path_Leg_Transition_2 <- tbl_template$tbl_Path_Leg_Transition
    tbl$tbl_Path_Leg_Transition_3 <- tbl_template$tbl_Path_Leg_Transition
  })
  
  
  # PLT Adaptation Editor | Load Existing Application -----------------------
  
  shinyDirChoose(input, "editor_load", roots=getVolumes()(), session=session, restrictions = system.file(package = 'base'))
  
  adaptation_dir <- eventReactive(input$editor_load, {
    if (!is.integer(input$editor_load)) {
      return(parseDirPath(roots=getVolumes()(), input$editor_load))
    }
  })
  
  observeEvent(adaptation_dir(), {
    
    req(adaptation_dir())
    
    tbl$loaded <- T
    
    missing_tbl <- c()
    
    dir_tbl_Runway <- list.files(adaptation_dir(), "03_Populate_tbl_Runway.csv", full.names = T)
    if (length(dir_tbl_Runway) > 0) {
      tbl$tbl_Runway <- fread(dir_tbl_Runway)
    } else {
      missing_tbl <- c(missing_tbl, "03_Populate_tbl_Runway.csv")
    }

    dir_tbl_Volumes <- list.files(adaptation_dir(), "05_Populate_Airspace_Volumes.csv", full.names = T)
    if (length(dir_tbl_Volumes) > 0) {
      tbl$tbl_Volumes <- fread(dir_tbl_Volumes)
    } else {
      missing_tbl <- c(missing_tbl, "05_Populate_Airspace_Volumes.csv")
    }
    
    dir_tbl_Volumes_2 <- list.files(adaptation_dir(), "05_Populate_Airspace_Volumes_2.csv", full.names = T)
    if (length(dir_tbl_Volumes_2) > 0) {
      tbl$tbl_Volumes_2 <- fread(dir_tbl_Volumes_2)
    } else {
      missing_tbl <- c(missing_tbl, "05_Populate_Airspace_Volumes_2.csv")
    }
    
    dir_tbl_Volumes_3 <- list.files(adaptation_dir(), "05_Populate_Airspace_Volumes_3.csv", full.names = T)
    if (length(dir_tbl_Volumes_3) > 0) {
      tbl$tbl_Volumes_3 <- fread(dir_tbl_Volumes_3)
    } else {
      missing_tbl <- c(missing_tbl, "05_Populate_Airspace_Volumes_3.csv")
    }
    
    dir_tbl_Path_Leg <- list.files(adaptation_dir(), "07_Populate_tbl_Path_Leg.csv", full.names = T)
    if (length(dir_tbl_Path_Leg) > 0) {
      tbl$tbl_Path_Leg <- fread(dir_tbl_Path_Leg)
    } else {
      missing_tbl <- c(missing_tbl, "07_Populate_tbl_Path_Leg.csv")
    }
    
    dir_tbl_Path_Leg_2 <- list.files(adaptation_dir(), "07_Populate_tbl_Path_Leg_2.csv", full.names = T)
    if (length(dir_tbl_Path_Leg_2) > 0) {
      tbl$tbl_Path_Leg_2 <- fread(dir_tbl_Path_Leg_2)
    } else {
      missing_tbl <- c(missing_tbl, "07_Populate_tbl_Path_Leg_2.csv")
    }
    
    dir_tbl_Path_Leg_3 <- list.files(adaptation_dir(), "07_Populate_tbl_Path_Leg_3.csv", full.names = T)
    if (length(dir_tbl_Path_Leg_3) > 0) {
      tbl$tbl_Path_Leg_3 <- fread(dir_tbl_Path_Leg_3)
    } else {
      missing_tbl <- c(missing_tbl, "07_Populate_tbl_Path_Leg_3.csv")
    }
    
    dir_tbl_Path_Leg_Transition <- list.files(adaptation_dir(), "08_Populate_tbl_Path_Leg_Transition.csv", full.names = T)
    if (length(dir_tbl_Path_Leg_Transition) > 0) {
      tbl$tbl_Path_Leg_Transition <- fread(dir_tbl_Path_Leg_Transition)
    } else {
      missing_tbl <- c(missing_tbl, "08_Populate_tbl_Path_Leg_Transition.csv")
    }
    
    dir_tbl_Path_Leg_Transition_2 <- list.files(adaptation_dir(), "08_Populate_tbl_Path_Leg_Transition_2.csv", full.names = T)
    if (length(dir_tbl_Path_Leg_Transition_2) > 0) {
      tbl$tbl_Path_Leg_Transition_2 <- fread(dir_tbl_Path_Leg_Transition_2)
    } else {
      missing_tbl <- c(missing_tbl, "08_Populate_tbl_Path_Leg_Transition_2.csv")
    }
    
    dir_tbl_Path_Leg_Transition_3 <- list.files(adaptation_dir(), "08_Populate_tbl_Path_Leg_Transition_3.csv", full.names = T)
    if (length(dir_tbl_Path_Leg_Transition_3) > 0) {
      tbl$tbl_Path_Leg_Transition_3 <- fread(dir_tbl_Path_Leg_Transition_3)
    } else {
      missing_tbl <- c(missing_tbl, "08_Populate_tbl_Path_Leg_Transition_3.csv")
    }
    
    if (length(missing_tbl) > 0) {
      showModal(modalDialog(
        title = div(class = "centered", "WARNING: Files Not Found"),
        HTML("<li>", paste0(missing_tbl, collapse = "</li><li>"), "</li>"),
        size = "m",
        footer = div(class = "centered", modalButton("Dismiss")),
        easyClose = F
      ))
    }
    
  })
  
  
  # PLT Adaptation Editor | Export ------------------------------------------

  shinyDirChoose(input, "editor_export", roots=getVolumes()(), session=session, restrictions = system.file(package = 'base'))
  
  export_dir <- eventReactive(input$editor_export, {
    if (!is.integer(input$editor_export)) {
      return(parseDirPath(roots=getVolumes()(), input$editor_export))
    }
  })
  
  observeEvent(export_dir(), {
    
    req(export_dir())
    
    tbl_names <- list(
      tbl_Runway = "03_Populate_tbl_Runway.csv",
      tbl_Volumes = "05_Populate_Airspace_Volumes.csv",
      tbl_Volumes_2 = "05_Populate_Airspace_Volumes_2.csv",
      tbl_Volumes_3 = "05_Populate_Airspace_Volumes_3.csv",
      tbl_Path_Leg = "07_Populate_tbl_Path_Leg.csv",
      tbl_Path_Leg_2 = "07_Populate_tbl_Path_Leg_2.csv",
      tbl_Path_Leg_3 = "07_Populate_tbl_Path_Leg_3.csv",
      tbl_Path_Leg_Transition = "08_Populate_tbl_Path_Leg_Transition.csv",
      tbl_Path_Leg_Transition_2 = "08_Populate_tbl_Path_Leg_Transition_2.csv",
      tbl_Path_Leg_Transition_3 = "08_Populate_tbl_Path_Leg_Transition_3.csv"
    )
    
    base_export <- tbl_names[!grepl("^.*_[23]{1}$", names(tbl_names))]
    for (i in 1:length(base_export)) {
      tbl[[names(base_export)[i]]] <- hot_to_r(input[[paste0("DT_", names(base_export)[i])]])
      fwrite(tbl[[names(base_export)[i]]], file.path(export_dir(), base_export[i]), col.names = F, quote = F)
    }
    
    if (input$plt_variants > 0) {
      variant_2 <- tbl_names[grepl("^.*_[2]{1}$", names(tbl_names))]
      for (i in 1:length(variant_2)) {
        tbl[[names(variant_2)[i]]] <- hot_to_r(input[[paste0("DT_", names(variant_2)[i])]])
        fwrite(tbl[[names(variant_2)[i]]], file.path(export_dir(), variant_2[i]), col.names = F, quote = F)
      }
    }
    
    if (input$plt_variants > 1) {
      variant_3 <- tbl_names[grepl("^.*_[3]{1}$", names(tbl_names))]
      for (i in 1:length(variant_3)) {
        tbl[[names(variant_3)[i]]] <- hot_to_r(input[[paste0("DT_", names(variant_3)[i])]])
        fwrite(tbl[[names(variant_3)[i]]], file.path(export_dir(), variant_3[i]), col.names = F, quote = F)
      }
    }
    
  })
  

  # PLT Adaptation Editor | Show/Hide Variant Tables ------------------------

  observeEvent(input$plt_variants, {
    if (input$plt_variants == 0) {
      shinyjs::hide("DT_tbl_Volumes_2")
      shinyjs::hide("DT_tbl_Path_Leg_2")
      shinyjs::hide("DT_tbl_Path_Leg_Transition_2")
      shinyjs::hide("DT_tbl_Volumes_3")
      shinyjs::hide("DT_tbl_Path_Leg_3")
      shinyjs::hide("DT_tbl_Path_Leg_Transition_3")
      shinyjs::hide("toggle_tbl_Volumes_2")
      shinyjs::hide("toggle_tbl_Volumes_3")
    } else if (input$plt_variants == 1) {
      shinyjs::show("DT_tbl_Volumes_2")
      shinyjs::show("DT_tbl_Path_Leg_2")
      shinyjs::show("DT_tbl_Path_Leg_Transition_2")
      shinyjs::show("toggle_tbl_Volumes_2")
      shinyjs::hide("DT_tbl_Volumes_3")
      shinyjs::hide("DT_tbl_Path_Leg_3")
      shinyjs::hide("DT_tbl_Path_Leg_Transition_3")
      shinyjs::hide("toggle_tbl_Volumes_3")
    } else if (input$plt_variants == 2) {
      shinyjs::show("DT_tbl_Volumes_2")
      shinyjs::show("DT_tbl_Path_Leg_2")
      shinyjs::show("DT_tbl_Path_Leg_Transition_2")
      shinyjs::show("DT_tbl_Volumes_3")
      shinyjs::show("DT_tbl_Path_Leg_3")
      shinyjs::show("DT_tbl_Path_Leg_Transition_3")
      shinyjs::show("toggle_tbl_Volumes_2")
      shinyjs::show("toggle_tbl_Volumes_3")
    } else if (x > 2 | x < 0) {
      updateTextInput(session, "plt_variants", value = 0)
    }
  })
  
  
  # PLT Adaptation Editor | Hot Tables --------------------------------------

  hot_tbl_Volumes <- reactive({
    req(input$DT_tbl_Volumes)
    return(hot_to_r(input$DT_tbl_Volumes))
  })
  
  hot_tbl_Volumes_2 <- reactive({
    req(input$DT_tbl_Volumes_2)
    return(hot_to_r(input$DT_tbl_Volumes_2))
  })
  
  hot_tbl_Volumes_3 <- reactive({
    req(input$DT_tbl_Volumes_3)
    return(hot_to_r(input$DT_tbl_Volumes_3))
  })
  
  
  # PLT Adaptation Editor | Hot Tables Map Toggle Update --------------------

  observeEvent(hot_tbl_Volumes(), {
    req(hot_tbl_Volumes())
    updatePickerInput(session, "toggle_tbl_Volumes", choices = hot_tbl_Volumes()$Volume_Name)
  })
  
  observeEvent(hot_tbl_Volumes_2(), {
    req(hot_tbl_Volumes_2())
    updatePickerInput(session, "toggle_tbl_Volumes_2", choices = hot_tbl_Volumes_2()$Volume_Name)
  })
  
  observeEvent(hot_tbl_Volumes_3(), {
    req(hot_tbl_Volumes_3())
    updatePickerInput(session, "toggle_tbl_Volumes_3", choices = hot_tbl_Volumes_3()$Volume_Name)
  })
  
  
  # PLT Adaptation Editor | Hot Tables Volume Preview Map -------------------

  output$map <- renderLeaflet({
    map_template(1)
  })
  
  observeEvent(input$toggle_tbl_Volumes, {
    p <- leafletProxy("map") %>% clearGroup("Volumes")
    points <- configVolume_To_pointSequence(hot_tbl_Volumes(), dbi_con)
    for (i in input$toggle_tbl_Volumes) {
      v_i <- points[Volume_Name %in% i]
      p %>% addPolygons(
        data = Polygon(
          data.table(
            Longitude = v_i$Longitude*180/pi,
            Latitude = v_i$Latitude*180/pi
          )
        ),
        highlight = highlightOptions(
          bringToFront = F
        ),
        group = "Volumes"
      )
    }
  })
  
  observeEvent(input$toggle_tbl_Volumes_2, {
    p <- leafletProxy("map") %>% clearGroup("Volumes_2")
    points <- configVolume_To_pointSequence(hot_tbl_Volumes_2(), dbi_con)
    for (i in input$toggle_tbl_Volumes_2) {
      v_i <- points[Volume_Name %in% i]
      p %>% addPolygons(
        data = Polygon(
          data.table(
            Longitude = v_i$Longitude*180/pi,
            Latitude = v_i$Latitude*180/pi
          )
        ),
        highlight = highlightOptions(
          bringToFront = F
        ),
        group = "Volumes_2"
      )
    }
  })
  
  observeEvent(input$toggle_tbl_Volumes_3, {
    p <- leafletProxy("map") %>% clearGroup("Volumes_3")
    points <- configVolume_To_pointSequence(hot_tbl_Volumes_3(), dbi_con)
    for (i in input$toggle_tbl_Volumes_3) {
      v_i <- points[Volume_Name %in% i]
      p %>% addPolygons(
        data = Polygon(
          data.table(
            Longitude = v_i$Longitude*180/pi,
            Latitude = v_i$Latitude*180/pi
          )
        ),
        highlight = highlightOptions(
          bringToFront = F
        ),
        group = "Volumes_3"
      )
    }
  })
  
  
  # PLT Analysis Run --------------------------------------------------------

  observeEvent(input$plt_analysis_run, {
    showModal(modalDialog(
      div(
        style = "text-align: center",
        h3("MODIFY DATABASE WARNING")
      ),
      hr(),
      div(
        style = "text-align: center",
        tags$b("Do you wish to run PLT analysis?"),
        tags$b("This may take some time."),
      ),
      size = "s",
      footer = div(
        class = "centered",
        modalButton("Cancel"),
        div(style = "width: 15px"),
        actionButton(ns("plt_analysis_run_confirm"), "Confirm")
      ),
      easyClose = F
    ))
  })
  
  observeEvent(input$plt_analysis_run_confirm, {
    
    variants <- isolate(input$plt_analysis_variants)
    
    tbl_basenames <- c("tbl_Volumes", "tbl_Path_Leg", "tbl_Path_Leg_Transition")
    
    all_tables_populated <- T
    
    if (length(variants) == 0) {
      plt_analysis_error_modal("Please select at least one variant.")
      all_tables_populated <- F
    }
    
    if (nrow(tbl_Runway()) == 0) {
      plt_analysis_error_modal("tbl_Runway not populated.")
      all_tables_populated <- F
    }
    
    if (1 %in% variants) {
      if (nrow(dbGetQuery(dbi_con, "SELECT TOP (1) * FROM tbl_Volume")) == 0) {
        plt_analysis_error_modal("tbl_Volume not populated.")
        all_tables_populated <- F
      }
      if (nrow(dbGetQuery(dbi_con, "SELECT TOP (1) * FROM tbl_Path_Leg")) == 0) {
        plt_analysis_error_modal("tbl_Path_Leg not populated.")
        all_tables_populated <- F
      }
      if (nrow(dbGetQuery(dbi_con, "SELECT TOP (1) * FROM tbl_Path_Leg_Transition")) == 0) {
        plt_analysis_error_modal("tbl_Path_Leg_Transition not populated.")
        all_tables_populated <- F
      }
    }
    
    if (2 %in% variants) {
      if (nrow(dbGetQuery(dbi_con, "SELECT TOP (1) * FROM tbl_Volume_2")) == 0) {
        plt_analysis_error_modal("tbl_Volume_2 not populated.")
        all_tables_populated <- F
      }
      if (nrow(dbGetQuery(dbi_con, "SELECT TOP (1) * FROM tbl_Path_Leg_2")) == 0) {
        plt_analysis_error_modal("tbl_Path_Leg_2 not populated.")
        all_tables_populated <- F
      }
      if (nrow(dbGetQuery(dbi_con, "SELECT TOP (1) * FROM tbl_Path_Leg_Transition_2")) == 0) {
        plt_analysis_error_modal("tbl_Path_Leg_Transition_2 not populated.")
        all_tables_populated <- F
      }
    }
    
    if (3 %in% variants) {
      if (nrow(dbGetQuery(dbi_con, "SELECT TOP (1) * FROM tbl_Volume_3")) == 0) {
        plt_analysis_error_modal("tbl_Volume_3 not populated.")
        all_tables_populated <- F
      }
      if (nrow(dbGetQuery(dbi_con, "SELECT TOP (1) * FROM tbl_Path_Leg_3")) == 0) {
        plt_analysis_error_modal("tbl_Path_Leg_3 not populated.")
        all_tables_populated <- F
      }
      if (nrow(dbGetQuery(dbi_con, "SELECT TOP (1) * FROM tbl_Path_Leg_Transition_3")) == 0) {
        plt_analysis_error_modal("tbl_Path_Leg_Transition_3 not populated.")
        all_tables_populated <- F
      }
    }
    
    if (all_tables_populated) {
      plt_analysis_run(variants)
      removeModal()
    }
    
  })
  
  
  # PLT Summary Stats -------------------------------------------------------
  
  output$plt_summary_table <- DT::renderDataTable({
    datatable_customised_2(Output_Summary_Stats(dbi_con))
  }, server = T)


  # PLT Detailed Analysis | Level 1 -----------------------------------------

  output$analysis_view_1 <- renderUI({div(
    div(
      style = "padding: 5px; background-color: #f5f5f5; border: 1px solid #e3e3e3; height: 58px;",
      radioButtons(
        ns("filt_change"),
        NULL,
        c("Show all flights" = 1, "Only show flights with changed non-standard transitions" = 2)
      ),
    ),
    
    div(style = "height: 25px"),
    
    uiOutput(ns("analysis_view_2")),
    
    div(style = "height: 25px;"),
    
    DT::dataTableOutput(outputId = ns("plt_detailed_table")),
    
    div(
      class = "centered",
      actionButton(ns("pltvis_flights_view"), "View selected flights")
    ),
    
    hr(),
    checkboxInput(ns("pltvis_vol_sep"), "Toggle separate volume display"),
    uiOutput(ns("pltvis_vol_ui")),
    div(
      style = "display: flex",
      leafletOutput(ns("pltvis_map_1"), height = "940px"),
      leafletOutput(ns("pltvis_map_2"), height = "940px"),
      leafletOutput(ns("pltvis_map_3"), height = "940px")
    ),
    
    hr(),
    checkboxInput(ns("pltvis_g1_zeros"), "Remove zero values"),
    checkboxInput(ns("pltvis_g1_sep"), "Separate Non-Standard Transition Filters"),
    uiOutput(ns("pltvis_g1_ui")),
    plotlyOutput(ns("pltvis_g1")),
    
    hr(),
    plotlyOutput(ns("pltvis_g2")),
    
    hr(),
    checkboxInput(ns("pltvis_g3_sep"), "Separate Non-Standard Transition Filters"),
    uiOutput(ns("pltvis_g3_ui")),
    plotlyOutput(ns("pltvis_g3")),
    
    hr(),
    checkboxInput(ns("pltvis_g4_sep"), "Separate Non-Standard Transition Filters"),
    uiOutput(ns("pltvis_g4_ui")),
    plotlyOutput(ns("pltvis_g4"))
  )})

  plt_dat_1 <- reactive({
    d <- vw_PLT_Detailed_Analysis()
    if (input$filt_change == 1) {
      return(d)
    } else if (input$filt_change == 2) {
      consider_V2 <- any(d$`Non Standard Transition V2` != "No Non-Standard Transitions")
      consider_V3 <- any(d$`Non Standard Transition V3` != "No Non-Standard Transitions")
      if (consider_V2 & !consider_V3) {
        d <- d[`Non Standard Transition V1` != `Non Standard Transition V2`]
      } else if (!consider_V2 & consider_V3) {
        d <- d[`Non Standard Transition V1` != `Non Standard Transition V3`]
      } else if (consider_V2 & consider_V3) {
        d <- d[`Non Standard Transition V1` != `Non Standard Transition V2` |
                 `Non Standard Transition V1` != `Non Standard Transition V3` | 
                 `Non Standard Transition V2` != `Non Standard Transition V3`]
      }
      return(d)
    }
  })

  
  # PLT Detailed Analysis | Level 2 -----------------------------------------

  output$analysis_view_2 <- renderUI({div(
    pickerInput_customised2(
      ns("filt_rwy"),
      "Filter Runway",
      choices = tbl_Runway()$Runway_Name,
      selected = tbl_Runway()$Runway_Name
    ),
    div(style = "height: 5px"),
    checkboxInput(ns("filt_trans_sep"), "Separate non-standard transition filters"),
    uiOutput(ns("analysis_view_3"))
  )})
  
  plt_dat_2 <- reactive({
    d <- plt_dat_1()
    if ("Landing Runway" %in% names(plt_dat_1())) {
      d <- d[`Landing Runway` %in% input$filt_rwy]
    }
    return(d)
  })

  
  # PLT Detailed Analysis | Level 3 -----------------------------------------

  transition_types <- reactive({
    d <- unique(c(
      plt_dat_2()$`Non Standard Transition V1`,
      plt_dat_2()$`Non Standard Transition V2`,
      plt_dat_2()$`Non Standard Transition V3`
    ))
    d <- unique(unlist(strsplit(d, split = ", ")))
    d <- sort(unique(gsub("^([A-z0-9 \\-]+)[:=\\(\\)]?.*$", "\\1", d)))
    d <- gsub("^(.*)[ ]{1,}$", "\\1", d)
    return(d)
  })
  
  output$analysis_view_3 <- renderUI({
    if (input$filt_trans_sep) {
      div(
        pickerInput_customised2(
          ns("filt_trans_v1_s1"),
          "Filter Non-Standard Transitions V1 - Stage 1",
          choices = transition_types(),
          selected = transition_types()[transition_types() %!in% c("No Non-Standard Transitions", "Aircraft not tracked")]
        ),
        uiOutput(ns("filt_trans_v1_s2_ui")),
        
        pickerInput_customised2(
          ns("filt_trans_v2_s1"),
          "Filter Non-Standard Transitions V2 - Stage 1",
          choices = transition_types(),
          selected = transition_types()[transition_types() %!in% c("No Non-Standard Transitions", "Aircraft not tracked")]
        ),
        uiOutput(ns("filt_trans_v2_s2_ui")),
        
        pickerInput_customised2(
          ns("filt_trans_v3_s1"), 
          "Filter Non-Standard Transitions V3 - Stage 1",
          choices = transition_types(),
          selected = transition_types()[transition_types() %!in% c("No Non-Standard Transitions", "Aircraft not tracked")]
        ),
        uiOutput(ns("filt_trans_v3_s2_ui")),
        
        uiOutput(ns("analysis_view_4"))
      )
    } else {
      div(
        pickerInput_customised2(
          ns("filt_trans_s1"),
          "Filter Non-Standard Transitions - Stage 1",
          choices = transition_types(),
          selected = transition_types()[transition_types() %!in% c("No Non-Standard Transitions", "Aircraft not tracked")]
        ),
        uiOutput(ns("filt_trans_s2_ui")),
        
        uiOutput(ns("analysis_view_4"))
      )
    }
  })
  
  output$filt_trans_v1_s2_ui <- renderUI({
    all_choices <- unique(unlist(strsplit(plt_dat_2()$`Non Standard Transition V1`, ", ")))
    filt_choices <- sort(unique(unlist(
      sapply(input$filt_trans_v1_s1, function(x) grep(x, all_choices, ignore.case = T, value = T))
    )))
    pickerInput_customised2(
      ns("filt_trans_v1_s2"),
      "Filter Non-Standard Transitions V1 - Stage 2",
      choices = filt_choices,
      selected = filt_choices
    )
  })
  
  output$filt_trans_v2_s2_ui <- renderUI({
    all_choices <- unique(unlist(strsplit(plt_dat_2()$`Non Standard Transition V2`, ", ")))
    filt_choices <- sort(unique(unlist(
      sapply(input$filt_trans_v2_s1, function(x) grep(x, all_choices, ignore.case = T, value = T))
    )))
    pickerInput_customised2(
      ns("filt_trans_v2_s2"),
      "Filter Non-Standard Transitions V2 - Stage 2",
      choices = filt_choices,
      selected = filt_choices
    )
  })
  
  output$filt_trans_v3_s2_ui <- renderUI({
    all_choices <- unique(unlist(strsplit(plt_dat_2()$`Non Standard Transition V3`, ", ")))
    filt_choices <- sort(unique(unlist(
      sapply(input$filt_trans_v3_s1, function(x) grep(x, all_choices, ignore.case = T, value = T))
    )))
    pickerInput_customised2(
      ns("filt_trans_v3_s2"),
      "Filter Non-Standard Transitions V3 - Stage 2",
      choices = filt_choices,
      selected = filt_choices
    )
  })
  
  output$filt_trans_s2_ui <- renderUI({
    all_choices <- unlist(strsplit(unique(c(
      plt_dat_2()$`Non Standard Transition V1`,
      plt_dat_2()$`Non Standard Transition V2`,
      plt_dat_2()$`Non Standard Transition V3`
    )), ", "))
    filt_choices <- sort(unique(unlist(
      sapply(input$filt_trans_s1, function(x) grep(x, all_choices, ignore.case = T, value = T))
    )))
    pickerInput_customised2(
      ns("filt_trans_s2"),
      "Filter Non-Standard Transitions - Stage 2",
      choices = filt_choices,
      selected = filt_choices
    )
  })
  
  plt_dat_3 <- reactive({
    d <- plt_dat_2()
    if (input$filt_trans_sep) {
      d <- d[grepl(paste(input$filt_trans_v1_s2, collapse = "|"), `Non Standard Transition V1`) |
               grepl(paste(input$filt_trans_v2_s2, collapse = "|"), `Non Standard Transition V2`) |
               grepl(paste(input$filt_trans_v3_s2, collapse = "|"), `Non Standard Transition V3`)]
    } else {
      d <- d[grepl(paste(input$filt_trans_s2, collapse = "|"), `Non Standard Transition V1`) |
               grepl(paste(input$filt_trans_s2, collapse = "|"), `Non Standard Transition V2`) |
               grepl(paste(input$filt_trans_s2, collapse = "|"), `Non Standard Transition V3`)]
    }
    return(d)
  })
  
  # PLT Detailed Analysis | Level 4 -----------------------------------------

  output$analysis_view_4 <- renderUI({
    filt_atd12_range <- c(
      min(plt_dat_3()$`Activation Time Difference V1 V2`, na.rm = T),
      max(plt_dat_3()$`Activation Time Difference V1 V2`, na.rm = T)
    )
    filt_atd13_range <- c(
      min(plt_dat_3()$`Activation Time Difference V1 V3`, na.rm = T),
      max(plt_dat_3()$`Activation Time Difference V1 V3`, na.rm = T)
    )
    div(
      div(style = "height: 25px;"),
      sliderInput(
        ns("filt_atd12"),
        "Filter Activation Time Difference (V1 vs V2)",
        value = filt_atd12_range,
        min = filt_atd12_range[1],
        max = filt_atd12_range[2]
      ),
      sliderInput(
        ns("filt_atd13"),
        "Filter Activation Time Difference (V1 vs V3)",
        value = filt_atd13_range,
        min = filt_atd13_range[1],
        max = filt_atd13_range[2]
      ),
      div(style = "height: 25px;"),
      div(
        class = "centered",
        actionButton(ns("filt_update"), "Update table")
      )
    )
  })
  
  plt_dat_4 <- reactive({
    d <- plt_dat_3()
    d <- d[`Activation Time Difference V1 V2` >= input$filt_atd12[1] &
             `Activation Time Difference V1 V2` <= input$filt_atd12[2] &
             `Activation Time Difference V1 V3` >= input$filt_atd13[1] &
             `Activation Time Difference V1 V3` <= input$filt_atd13[2]]
    return(d)
  })
  
  # PLT Analysis Tools | Detailed Analysis Map ------------------------------

  output$plt_detailed_table <- DT::renderDT({
    req(input$filt_update)
    datatable_customised_3(plt_dat_4())
  }, server = T)
  
  output$pltvis_map_1 <- renderLeaflet(map_template(2))
  output$pltvis_map_2 <- renderLeaflet(map_template(2))
  output$pltvis_map_3 <- renderLeaflet(map_template(2))
  
  # Drawing Volumes
  
  observeEvent(input$pltvis_vol_sep, {
    for (i in 1:3) {
      p <- leafletProxy(paste0("pltvis_map_", i))
      p %>% clearGroup(paste0("volumes_", i))
    }
    if (input$pltvis_vol_sep) {
      vol_v1 <- unique(as.character(volumes()[Variant == "V1"]$Volume_Name))
      vol_v2 <- unique(as.character(volumes()[Variant == "V2"]$Volume_Name))
      vol_v3 <- unique(as.character(volumes()[Variant == "V3"]$Volume_Name))
      output$pltvis_vol_ui <- renderUI({
        div(
          pickerInput_customised(ns("pltvis_vol_1"), "Display Volumes V1", choices = vol_v1),
          pickerInput_customised(ns("pltvis_vol_2"), "Display Volumes V2", choices = vol_v2),
          pickerInput_customised(ns("pltvis_vol_3"), "Display Volumes V3", choices = vol_v3)
        )
      })
    } else {
      vol_all <- unique(as.character(volumes()$Volume_Name))
      output$pltvis_vol_ui <- renderUI({
        pickerInput_customised(ns("pltvis_vol"), "Display Volumes", choices = vol_all)
      })
    }
  })
  
  observeEvent(input$pltvis_vol, {
    for (i in 1:3) {
      p <- leafletProxy(paste0("pltvis_map_", i))
      p %>% clearGroup(paste0("volumes_", i))
      for (vol in input$pltvis_vol) {
        p_vol <- volumes()[Variant == "V1"][Volume_Name %in% vol, c("Longitude","Latitude")]
        p %>% addPolygons(data = Polygon(p_vol), group = paste0("volumes_", i))
      }
    }
  }, ignoreNULL = F)
  
  observeEvent(input$pltvis_vol_1, {
    p <- leafletProxy("pltvis_map_1")
    p %>% clearGroup("volumes_1")
    for (vol in input$pltvis_vol_1) {
      p_vol <- volumes()[Volume_Name %in% vol, c("Longitude","Latitude")]
      p %>% addPolygons(data = Polygon(p_vol), group = "volumes_1")
    }
  }, ignoreNULL = F)
  
  observeEvent(input$pltvis_vol_2, {
    p <- leafletProxy("pltvis_map_2")
    p %>% clearGroup("volumes_2")
    for (vol in input$pltvis_vol_2) {
      p_vol <- volumes()[Volume_Name %in% vol, c("Longitude","Latitude")]
      p %>% addPolygons(data = Polygon(p_vol), group = "volumes_2")
    }
  }, ignoreNULL = F)
  
  observeEvent(input$pltvis_vol_3, {
    p <- leafletProxy("pltvis_map_3")
    p %>% clearGroup("volumes_3")
    for (vol in input$pltvis_vol_3) {
      p_vol <- volumes()[Volume_Name %in% vol, c("Longitude","Latitude")]
      p %>% addPolygons(data = Polygon(p_vol), group = "volumes_3")
    }
  }, ignoreNULL = F)
  
  # Get tracks from table
  plt_tracks <- eventReactive(input$pltvis_flights_view, {
    fpid <- plt_dat_4()$`Flight Plan ID`[input$plt_detailed_table_rows_selected]
    
    tracks <- as.data.table(dbGetQuery(dbi_con, sprintf(
      "SELECT * FROM vw_Radar_Track_Point_Derived WHERE Flight_Plan_ID IN ('%s')",
      paste0(fpid, collapse = "','")
    )))
    
    return(tracks)
  })
  
  # Dynamically update map with new flights selected
  observeEvent(plt_tracks(), {
    
    p1 <- leafletProxy("pltvis_map_1")
    p2 <- leafletProxy("pltvis_map_2")
    p3 <- leafletProxy("pltvis_map_3")
    
    # Set map bounds
    
    p1 %>% fitBounds(
      lng1 = min(plt_tracks()$Lon, na.rm = T),
      lng2 = max(plt_tracks()$Lon, na.rm = T),
      lat1 = min(plt_tracks()$Lat, na.rm = T),
      lat2 = max(plt_tracks()$Lat, na.rm = T)
    )
    p2 %>% fitBounds(
      lng1 = min(plt_tracks()$Lon, na.rm = T),
      lng2 = max(plt_tracks()$Lon, na.rm = T),
      lat1 = min(plt_tracks()$Lat, na.rm = T),
      lat2 = max(plt_tracks()$Lat, na.rm = T)
    )
    p3 %>% fitBounds(
      lng1 = min(plt_tracks()$Lon, na.rm = T),
      lng2 = max(plt_tracks()$Lon, na.rm = T),
      lat1 = min(plt_tracks()$Lat, na.rm = T),
      lat2 = max(plt_tracks()$Lat, na.rm = T)
    )

    # Drawing tracks
    
    labs <- simple_map_labels(plt_tracks())
    
    legs <- as.data.table(
      dbGetQuery(
        dbi_con, "
          SELECT DISTINCT Path_Leg_Name FROM (
            SELECT DISTINCT Path_Leg_Name FROM tbl_Path_Leg
            UNION
            SELECT DISTINCT Path_Leg_Name FROM tbl_Path_Leg_2
            UNION
            SELECT DISTINCT Path_Leg_Name FROM tbl_Path_Leg_3
          ) AS leggy_boys
          ORDER BY Path_Leg_Name 
        "
      )
    )
    
    pal <- colorFactor(brewer.pal(11, "Spectral"), domain = legs$Path_Leg_Name)
    
    p1 %>% clearGroup("Tracks") %>%
      addCircleMarkers(
        data = plt_tracks(),
        lng = ~Lon,
        lat = ~Lat,
        color = ~pal(Path_Leg),
        label = labs,
        labelOptions = labelOptions(textsize = "13px", direction = "auto"),
        weight = 5,
        radius = 5,
        group = "Tracks",
        options = markerOptions(zIndexOffset = 1000001)
      )
    
    p2 %>% clearGroup("Tracks") %>%
      addCircleMarkers(
        data = plt_tracks(),
        lng = ~Lon,
        lat = ~Lat,
        color = ~pal(Path_Leg_2),
        label = labs,
        labelOptions = labelOptions(textsize = "13px", direction = "auto"),
        weight = 5,
        radius = 5,
        group = "Tracks",
        options = markerOptions(zIndexOffset = 1000001)
      )
    
    p3 %>% clearGroup("Tracks") %>%
      addCircleMarkers(
        data = plt_tracks(),
        lng = ~Lon,
        lat = ~Lat,
        color = ~pal(Path_Leg_3),
        label = labs,
        labelOptions = labelOptions(textsize = "13px", direction = "auto"),
        weight = 5,
        radius = 5,
        group = "Tracks",
        options = markerOptions(zIndexOffset = 1000001)
      )
    
    # Drawing indicators
    
    fpid <- unique(plt_tracks()$Flight_Plan_ID)
    
    d_v1 <- as.data.table(
      dbGetQuery(
        dbi_con, sprintf("
          SELECT
            t.Flight_Plan_ID,
            Indicator_Activation_Time_V1,
            Base_Leg_Altidude_V1,
            Non_Standard_Transition_V1,
            Base_Leg_Lateral_Distance_V1,
            Lat,
            Lon,
            Mode_C,
            Corrected_Mode_C
          FROM (
            SELECT * FROM vw_PLT_Detailed_Analysis
            WHERE Flight_Plan_ID IN ('%s')
          ) AS t
          LEFT JOIN vw_Radar_Track_Point_Derived AS s ON (
            t.Flight_Plan_ID = s.Flight_Plan_ID AND Indicator_Activation_Time_V1 = Track_Time
          )
          ", paste0(fpid, collapse = "','")
        )
      )
    )
    
    p1 %>% clearGroup("Indicators") %>%
      addMarkers(
        data = d_v1,
        lng = ~Lon,
        lat = ~Lat,
        label = simple_map_labels(d_v1),
        labelOptions = labelOptions(textsize = "13px", direction = "auto"),
        group = "Indicators"
      )
    
    d_v2 <- as.data.table(
      dbGetQuery(
        dbi_con, sprintf("
          SELECT
            t.Flight_Plan_ID,
            Indicator_Activation_Time_V2,
            Base_Leg_Altidude_V2,
            Non_Standard_Transition_V2,
            Base_Leg_Lateral_Distance_V2,
            Lat,
            Lon,
            Mode_C,
            Corrected_Mode_C
          FROM (
            SELECT * FROM vw_PLT_Detailed_Analysis
            WHERE Flight_Plan_ID IN ('%s')
          ) AS t
          LEFT JOIN vw_Radar_Track_Point_Derived AS s ON (
            t.Flight_Plan_ID = s.Flight_Plan_ID AND Indicator_Activation_Time_V2 = Track_Time
          )
          ", paste0(fpid, collapse = "','")
        )
      )
    )
    
    p2 %>% clearGroup("Indicators") %>%
      addMarkers(
        data = d_v2,
        lng = ~Lon,
        lat = ~Lat,
        label = simple_map_labels(d_v2),
        labelOptions = labelOptions(textsize = "13px", direction = "auto"),
        group = "Indicators"
      )
    
    d_v3 <- as.data.table(
      dbGetQuery(
        dbi_con, sprintf("
          SELECT
            t.Flight_Plan_ID,
            Indicator_Activation_Time_V3,
            Base_Leg_Altidude_V3,
            Non_Standard_Transition_V3,
            Base_Leg_Lateral_Distance_V3,
            Lat,
            Lon,
            Mode_C,
            Corrected_Mode_C
          FROM (
            SELECT * FROM vw_PLT_Detailed_Analysis
            WHERE Flight_Plan_ID IN ('%s')
          ) AS t
          LEFT JOIN vw_Radar_Track_Point_Derived AS s ON (
            t.Flight_Plan_ID = s.Flight_Plan_ID AND Indicator_Activation_Time_V3 = Track_Time
          )
          ", paste0(fpid, collapse = "','")
        )
      )
    )

    p3 %>% clearGroup("Indicators") %>%
      addMarkers(
        data = d_v3,
        lng = ~Lon,
        lat = ~Lat,
        label = simple_map_labels(d_v3),
        labelOptions = labelOptions(textsize = "13px", direction = "auto"),
        group = "Indicators"
      )
    
  }, ignoreNULL = F)
  
  
  # PLT Analysis Tools | Detailed Analysis Plots ----------------------------
  
  plt_plotdat <- eventReactive(input$pltvis_flights_view, {
    fpid <- plt_dat_4()$`Flight Plan ID`[input$plt_detailed_table_rows_selected]
    return(plt_dat_4()[`Flight Plan ID` %in% fpid])
  })
  
  output$pltvis_g1_ui <- renderUI({
    choices_v1 <- unique(plt_plotdat()$`Non Standard Transition V1`)
    choices_v2 <- unique(plt_plotdat()$`Non Standard Transition V2`)
    choices_v3 <- unique(plt_plotdat()$`Non Standard Transition V3`)
    choices_all <- unique(c(
      plt_plotdat()$`Non Standard Transition V1`,
      plt_plotdat()$`Non Standard Transition V2`,
      plt_plotdat()$`Non Standard Transition V3`
    ))
    if (input$pltvis_g1_sep) {
      div(
        pickerInput_customised(ns("pltvis_g1_trans1"), "Filter Non-Standard Transitions V1", choices=choices_v1, selected=choices_v1),
        pickerInput_customised(ns("pltvis_g1_trans2"), "Filter Non-Standard Transitions V2", choices=choices_v2, selected=choices_v2),
        pickerInput_customised(ns("pltvis_g1_trans3"), "Filter Non-Standard Transitions V3", choices=choices_v3, selected=choices_v3)
      )
    } else {
      pickerInput_customised(ns("pltvis_g1_trans"), "Filter Non-Standard Transitions", choices=choices_all, selected=choices_all)
    }
  })
  
  output$pltvis_g3_ui <- renderUI({
    choices_v1 <- unique(plt_plotdat()$`Non Standard Transition V1`)
    choices_v2 <- unique(plt_plotdat()$`Non Standard Transition V2`)
    choices_v3 <- unique(plt_plotdat()$`Non Standard Transition V3`)
    choices_all <- unique(c(
      plt_plotdat()$`Non Standard Transition V1`,
      plt_plotdat()$`Non Standard Transition V2`,
      plt_plotdat()$`Non Standard Transition V3`
    ))
    if (input$pltvis_g3_sep) {
      div(
        pickerInput_customised(ns("pltvis_g3_trans1"), "Filter Non-Standard Transitions V1", choices=choices_v1, selected=choices_v1),
        pickerInput_customised(ns("pltvis_g3_trans2"), "Filter Non-Standard Transitions V2", choices=choices_v2, selected=choices_v2),
        pickerInput_customised(ns("pltvis_g3_trans3"), "Filter Non-Standard Transitions V3", choices=choices_v3, selected=choices_v3)
      )
    } else {
      pickerInput_customised(ns("pltvis_g3_trans"), "Filter Non-Standard Transitions", choices=choices_all, selected=choices_all)
    }
  })
  
  output$pltvis_g4_ui <- renderUI({
    choices_v1 <- unique(plt_plotdat()$`Non Standard Transition V1`)
    choices_v2 <- unique(plt_plotdat()$`Non Standard Transition V2`)
    choices_v3 <- unique(plt_plotdat()$`Non Standard Transition V3`)
    choices_all <- unique(c(
      plt_plotdat()$`Non Standard Transition V1`,
      plt_plotdat()$`Non Standard Transition V2`,
      plt_plotdat()$`Non Standard Transition V3`
    ))
    if (input$pltvis_g4_sep) {
      div(
        pickerInput_customised(ns("pltvis_g4_trans1"), "Filter Non-Standard Transitions V1", choices=choices_v1, selected=choices_v1),
        pickerInput_customised(ns("pltvis_g4_trans2"), "Filter Non-Standard Transitions V2", choices=choices_v2, selected=choices_v2),
        pickerInput_customised(ns("pltvis_g4_trans3"), "Filter Non-Standard Transitions V3", choices=choices_v3, selected=choices_v3)
      )
    } else {
      pickerInput_customised(ns("pltvis_g4_trans"), "Filter Non-Standard Transitions", choices=choices_all, selected=choices_all)
    }
  })
  
  output$pltvis_g1 <- renderPlotly({
    
    d1 <- if (input$pltvis_g1_zeros) {
      plt_plotdat()[`Activation Time Difference V1 V2` != 0]
    } else {
      plt_plotdat()
    }
    
    d2 <- if (input$pltvis_g1_zeros) {
      plt_plotdat()[`Activation Time Difference V1 V3` != 0]
    } else {
      plt_plotdat()
    }
    
    if (input$pltvis_g1_sep) {
      d1 <- d1[`Non Standard Transition V1` %in% input$pltvis_g1_trans1 &
                 `Non Standard Transition V2` %in% input$pltvis_g1_trans2]
      
      d2 <- d2[`Non Standard Transition V1` %in% input$pltvis_g1_trans1 &
                 `Non Standard Transition V3` %in% input$pltvis_g1_trans3]
    } else {
      d1 <- d1[`Non Standard Transition V1` %in% input$pltvis_g1_trans &
                 `Non Standard Transition V2` %in% input$pltvis_g1_trans]
      
      d2 <- d2[`Non Standard Transition V1` %in% input$pltvis_g1_trans &
                 `Non Standard Transition V3` %in% input$pltvis_g1_trans]
    }
    
    
    plot_ly(alpha = 0.6) %>%
      add_histogram(data = d1, x = ~`Activation Time Difference V1 V2`, name = "V1 vs V2") %>%
      add_histogram(data = d2, x = ~`Activation Time Difference V1 V3`, name = "V1 vs V3") %>%
      layout(
        legend = list(x = 100, y = 0.5),
        barmode = "overlay",
        title = "Indicator Activation Time Difference",
        xaxis = list(title = "Indicator Activation Time Difference"),
        yaxis = list(title = "Frequency")
      )
  })
  
  output$pltvis_g2 <- renderPlotly({
    plot_ly(data = plt_plotdat(), alpha = 0.6) %>%
      add_markers(x=~`Activation Time Difference V1 V2`, y=~`Base Leg Altidude V2`, name="V1 vs V2") %>%
      add_markers(x=~`Activation Time Difference V1 V3`, y=~`Base Leg Altidude V3`, name="V1 vs V3") %>%
      layout(
        legend = list(x = 100, y = 0.5),
        title = "Activation Time Difference vs QNH",
        xaxis = list(title = "Activation Time Difference", tickformat="%H:%M:%S"),
        yaxis = list(title = "Base Leg Altidude")
      )
  })
  
  output$pltvis_g3 <- renderPlotly({
    if (input$pltvis_g3_sep) {
      d1 <- plt_plotdat()[`Non Standard Transition V1` %in% input$pltvis_g3_trans1]
      d2 <- plt_plotdat()[`Non Standard Transition V2` %in% input$pltvis_g3_trans2]
      d3 <- plt_plotdat()[`Non Standard Transition V3` %in% input$pltvis_g3_trans3]
    } else {
      d1 <- plt_plotdat()[`Non Standard Transition V1` %in% input$pltvis_g3_trans]
      d2 <- plt_plotdat()[`Non Standard Transition V2` %in% input$pltvis_g3_trans]
      d3 <- plt_plotdat()[`Non Standard Transition V3` %in% input$pltvis_g3_trans]
    }
    
    plot_ly(alpha = 0.6) %>%
      add_histogram(data = d1, x = ~`Base Leg Lateral Distance V1`, name = "V1") %>%
      add_histogram(data = d2, x = ~`Base Leg Lateral Distance V2`, name = "V2") %>%
      add_histogram(data = d3, x = ~`Base Leg Lateral Distance V3`, name = "V3") %>%
      layout(
        legend = list(x = 100, y = 0.5),
        barmode = "overlay",
        title = "Lateral Distance from Localiser when First Tracked",
        xaxis = list(title = "Lateral Distance From Localiser"),
        yaxis = list(title = "Frequency")
      )
  })
  
  output$pltvis_g4 <- renderPlotly({
    
    if (input$pltvis_g4_sep) {
      d1 <- plt_plotdat()[`Non Standard Transition V1` %in% input$pltvis_g4_trans1]
      d2 <- plt_plotdat()[`Non Standard Transition V2` %in% input$pltvis_g4_trans2]
      d3 <- plt_plotdat()[`Non Standard Transition V3` %in% input$pltvis_g4_trans3]
    } else {
      d1 <- plt_plotdat()[`Non Standard Transition V1` %in% input$pltvis_g4_trans]
      d2 <- plt_plotdat()[`Non Standard Transition V2` %in% input$pltvis_g4_trans]
      d3 <- plt_plotdat()[`Non Standard Transition V3` %in% input$pltvis_g4_trans]
    }
    
    plot_ly(alpha = 0.6) %>%
      add_histogram(data = d1, x = ~`Base Leg Altidude V1`, name = "V1") %>%
      add_histogram(data = d2, x = ~`Base Leg Altidude V2`, name = "V2") %>%
      add_histogram(data = d3, x = ~`Base Leg Altidude V3`, name = "V3") %>%
      layout(
        legend = list(x = 100, y = 0.5),
        barmode = "overlay",
        title = "Altitude when First Tracked",
        xaxis = list(title = "Altitude when Indicator First Activated"),
        yaxis = list(title = "Frequency")
      )
  })
  
}
