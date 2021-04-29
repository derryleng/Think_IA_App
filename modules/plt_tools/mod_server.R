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

map_template <- function() {
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
    d[is.na(Non_Standard_Transition_V1) | Non_Standard_Transition_V1 == ""]$Non_Standard_Transition_V1 <- "None"
    d[is.na(Non_Standard_Transition_V2) | Non_Standard_Transition_V2 == ""]$Non_Standard_Transition_V2 <- "None"
    d[is.na(Non_Standard_Transition_V3) | Non_Standard_Transition_V3 == ""]$Non_Standard_Transition_V3 <- "None"
    return(d)
  })
  
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

  # Load UI elements --------------------------------------------------------   

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
  
  # Create New Adaptation ---------------------------------------------------
  
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
  
  # Load Existing Adaptation ------------------------------------------------
  
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
  
  # Export ------------------------------------------------------------------
  
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
  
  # Show/hide additional variant tables -------------------------------------
  
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
  
  # Hots --------------------------------------------------------------------

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
  
  # Update map toggles based on hots ----------------------------------------
  
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
  
  # Hot volume preview map --------------------------------------------------
  
  output$map <- renderLeaflet({
    map_template()
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
        group = "Volumes_3"
      )
    }
  })
  
  # PLT Analysis Tools ------------------------------------------------------
  
  output$analysis_view <- renderUI({
    detailed_filter_3_range1 <- c(
      min(vw_PLT_Detailed_Analysis()$Activation_Time_Difference_V1_V2, na.rm = T),
      max(vw_PLT_Detailed_Analysis()$Activation_Time_Difference_V1_V2, na.rm = T)
    )
    
    detailed_filter_3_range2 <- c(
      min(vw_PLT_Detailed_Analysis()$Activation_Time_Difference_V1_V3, na.rm = T),
      max(vw_PLT_Detailed_Analysis()$Activation_Time_Difference_V1_V3, na.rm = T)
    )
    
    div(
      h3("PLT Running"),
      
      checkboxGroupInput(
        ns("plt_analysis_variants"), label = "Variant Selection", 
        choices = list("Variant 1" = 1, "Variant 2" = 2, "Variant 3" = 3),
        selected = 1
      ),
      
      actionButton(ns("plt_analysis_run"), "Run PLT Analysis"),
      
      hr(),
      
      h3("Summary Stats"),
      
      actionButton(ns("summary_show"), "Output summary stats"),
      
      uiOutput(ns("summary_stats_ui")),
      
      hr(),
      
      h3("Detailed Analysis"),
      
      radioButtons(
        ns("detailed_filter_1"),
        "Options",
        c("All Flights" = 1, "Changed Non-Standard Transitions" = 2)
      ),
      
      pickerInput_customised(
        ns("pltvis_g0_trans1"),
        "Filter Non-Standard Transitions V1",
        choices = unique(vw_PLT_Detailed_Analysis()$Non_Standard_Transition_V1),
        selected = unique(vw_PLT_Detailed_Analysis()$Non_Standard_Transition_V1)
      ),
      pickerInput_customised(
        ns("pltvis_g0_trans2"),
        "Filter Non-Standard Transitions V2",
        choices = unique(vw_PLT_Detailed_Analysis()$Non_Standard_Transition_V2),
        selected = unique(vw_PLT_Detailed_Analysis()$Non_Standard_Transition_V2)
      ),
      pickerInput_customised(
        ns("pltvis_g0_trans3"),
        "Filter Non-Standard Transitions V3",
        choices = unique(vw_PLT_Detailed_Analysis()$Non_Standard_Transition_V3),
        selected = unique(vw_PLT_Detailed_Analysis()$Non_Standard_Transition_V3)
      ),

      sliderInput(
        ns("detailed_filter_3"),
        "Filter Activation Time Difference (V1 vs V2)",
        value = detailed_filter_3_range1,
        min = detailed_filter_3_range1[1],
        max = detailed_filter_3_range1[2]
      ),
      
      sliderInput(
        ns("detailed_filter_4"),
        "Filter Activation Time Difference (V1 vs V3)",
        value = detailed_filter_3_range2,
        min = detailed_filter_3_range2[1],
        max = detailed_filter_3_range2[2]
      ),
      
      actionButton(ns("detailed_show"), "Update table"),
      
      uiOutput(ns("detailed_analysis_ui"))
      
    )
  })
  
  # Run PLT Analysis
  observeEvent(input$plt_analysis_run, {
    
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
      for (k in variants) {
        message("Executing UTMA_PLT_Validation_Run_Variant_", k, ".sql")
        dbSendQuery(dbi_con, read_SQL_File(paste0("modules/plt_tools/UTMA_PLT_Validation_Run_Variant_", k, ".sql")))
      }
    }

  })
  
  observeEvent(input$summary_show, {
    output$summary_stats_ui <- renderUI({
      div(
        div(style = "height: 15px;"),
        
        DT::dataTableOutput(outputId = ns("plt_summary_table"))
      )
    })
    output$plt_summary_table <- DT::renderDataTable({
      datatable_customised_2(Output_Summary_Stats(dbi_con))
    }, server = T)
  })
  
  vw_PLT_Detailed_Analysis_Filtered <- eventReactive(input$detailed_show, {
    d <- vw_PLT_Detailed_Analysis()
    if (input$detailed_filter_1 == 2) {
      d1 <- if (all(d$Non_Standard_Transition_V2 == "None")) {
        d[0]
      } else {
        d[Non_Standard_Transition_V1 != Non_Standard_Transition_V2]
      }
      d2 <- if (all(d$Non_Standard_Transition_V3 == "None")) {
        d[0]
      } else {
        d[Non_Standard_Transition_V1 != Non_Standard_Transition_V3]
      }
      d3 <- if (all(d$Non_Standard_Transition_V2 == "None") | all(d$Non_Standard_Transition_V3 == "None")) {
        d[0]
      } else {
        d[Non_Standard_Transition_V2 != Non_Standard_Transition_V3]
      }
      d <- rbind(d1, d2, d3)
    }
    d <- d[Non_Standard_Transition_V1 %in% input$pltvis_g0_trans1 &
             Non_Standard_Transition_V2 %in% input$pltvis_g0_trans2 &
             Non_Standard_Transition_V3 %in% input$pltvis_g0_trans3 &
             Activation_Time_Difference_V1_V2 >= input$detailed_filter_3[1] &
             Activation_Time_Difference_V1_V2 >= input$detailed_filter_3[2] &
             Activation_Time_Difference_V1_V3 >= input$detailed_filter_4[1] &
             Activation_Time_Difference_V1_V3 >= input$detailed_filter_4[2]]
    return(d)
  })
  
  observeEvent(input$detailed_show, {
    output$detailed_analysis_ui <- renderUI({
      div(
        div(style = "height: 15px;"),
        
        DT::dataTableOutput(outputId = ns("plt_detailed_table")),
        
        actionButton(ns("pltvis_flights_view"), "View selected flights"),
        
        uiOutput(ns("pltvis_flights_ui"))
      )
    })
    
    output$plt_detailed_table <- DT::renderDataTable({
      datatable_customised_2(vw_PLT_Detailed_Analysis_Filtered(), selection = "multiple")
    }, server = T)
  })
  
  observeEvent(input$pltvis_flights_view, {
    fpid <- vw_PLT_Detailed_Analysis_Filtered()$Flight_Plan_ID[input$plt_detailed_table_rows_selected]
    
    d <- vw_PLT_Detailed_Analysis_Filtered()[Flight_Plan_ID %in% fpid]
    
    output$pltvis_flights_ui <- renderUI({
      div(
        hr(),
        checkboxGroupInput(
          ns("pltvis_options"), label = "Display options", 
          choices = list("Tracks" = 1, "Indicator activation point" = 2)
        ),
        div(
          style = "display: flex",
          leafletOutput(ns("pltvis_map_1"), height = "563px"),
          leafletOutput(ns("pltvis_map_2"), height = "563px"),
          leafletOutput(ns("pltvis_map_3"), height = "563px")
        ),
        
        hr(),
        checkboxInput(ns("pltvis_g1_zeros"), "Remove zero values"),
        pickerInput_customised(ns("pltvis_g1_trans1"), "Filter Non-Standard Transitions V1"),
        pickerInput_customised(ns("pltvis_g1_trans2"), "Filter Non-Standard Transitions V2"),
        pickerInput_customised(ns("pltvis_g1_trans3"), "Filter Non-Standard Transitions V3"),
        plotlyOutput(ns("pltvis_g1")),
        
        hr(),
        plotlyOutput(ns("pltvis_g2")),
        
        hr(),
        pickerInput_customised(ns("pltvis_g3_trans1"), "Filter Non-Standard Transitions V1"),
        pickerInput_customised(ns("pltvis_g3_trans2"), "Filter Non-Standard Transitions V2"),
        pickerInput_customised(ns("pltvis_g3_trans3"), "Filter Non-Standard Transitions V3"),
        plotlyOutput(ns("pltvis_g3")),
        
        hr(),
        pickerInput_customised(ns("pltvis_g4_trans1"), "Filter Non-Standard Transitions V1"),
        pickerInput_customised(ns("pltvis_g4_trans2"), "Filter Non-Standard Transitions V2"),
        pickerInput_customised(ns("pltvis_g4_trans3"), "Filter Non-Standard Transitions V3"),
        plotlyOutput(ns("pltvis_g4"))
      )
    })
    
    output$pltvis_map_1 <- renderLeaflet(map_template())
    output$pltvis_map_2 <- renderLeaflet(map_template())
    output$pltvis_map_3 <- renderLeaflet(map_template())
    
    observeEvent(input$pltvis_options, {
      
      query <- sprintf(
        "SELECT * FROM vw_Radar_Track_Point_Derived WHERE Flight_Plan_ID IN ('%s')",
        paste0(fpid, collapse = "','")
      )
      dat <- as.data.table(dbGetQuery(dbi_con, query))
      
      p1 <- leafletProxy("pltvis_map_1") %>%
        fitBounds(
          lng1 = min(dat$Lon, na.rm = T),
          lng2 = max(dat$Lon, na.rm = T),
          lat1 = min(dat$Lat, na.rm = T),
          lat2 = max(dat$Lat, na.rm = T)
        )
      p2 <- leafletProxy("pltvis_map_2") %>%
        fitBounds(
          lng1 = min(dat$Lon, na.rm = T),
          lng2 = max(dat$Lon, na.rm = T),
          lat1 = min(dat$Lat, na.rm = T),
          lat2 = max(dat$Lat, na.rm = T)
        ) 
      p3 <- leafletProxy("pltvis_map_3") %>%
        fitBounds(
          lng1 = min(dat$Lon, na.rm = T),
          lng2 = max(dat$Lon, na.rm = T),
          lat1 = min(dat$Lat, na.rm = T),
          lat2 = max(dat$Lat, na.rm = T)
        ) 
      
      if (1 %in% input$pltvis_options) {
        labs <- simple_map_labels(dat)
        legs <- as.data.table(dbGetQuery(dbi_con, "
          SELECT DISTINCT Path_Leg_Name FROM (
            SELECT DISTINCT Path_Leg_Name FROM tbl_Path_Leg
            UNION
            SELECT DISTINCT Path_Leg_Name FROM tbl_Path_Leg_2
            UNION
            SELECT DISTINCT Path_Leg_Name FROM tbl_Path_Leg_3
          ) AS leggy_boys
          ORDER BY Path_Leg_Name 
        "))
        pal <- colorFactor(brewer.pal(11, "Spectral"), domain=legs$Path_Leg_Name)
        
        p1 %>% clearGroup("markers") %>%
          addCircleMarkers(
            data = dat,
            lng = ~Lon,
            lat = ~Lat,
            color = ~pal(Path_Leg),
            label = labs,
            labelOptions=labelOptions(textsize="13px", direction="auto"),
            weight=5,
            radius=5,
            group="markers"
          )
        p2 %>% clearGroup("markers") %>%
          addCircleMarkers(
            data = dat,
            lng = ~Lon,
            lat = ~Lat,
            color = ~pal(Path_Leg_2),
            label = labs,
            labelOptions=labelOptions(textsize="13px", direction="auto"),
            weight=5,
            radius=5,
            group="markers"
          )
        p3 %>% clearGroup("markers") %>%
          addCircleMarkers(
            data = dat,
            lng = ~Lon,
            lat = ~Lat,
            color = ~pal(Path_Leg_3),
            label = labs,
            labelOptions=labelOptions(textsize="13px", direction="auto"),
            weight=5,
            radius=5,
            group="markers"
          )
      } else {
        p1 %>% clearGroup("markers")
        p2 %>% clearGroup("markers")
        p3 %>% clearGroup("markers")
      }
      
      if (2 %in% input$pltvis_options) {
        d_v1 <- as.data.table(dbGetQuery(dbi_con, sprintf("
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
        )))
        p1 %>% clearGroup("ind") %>%
          addMarkers(
            data = d_v1,
            lng = ~Lon,
            lat = ~Lat,
            label = simple_map_labels(d_v1),
            labelOptions=labelOptions(textsize="13px", direction="auto"),
            group="ind"
          )
        
        d_v2 <- as.data.table(dbGetQuery(dbi_con, sprintf("
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
        )))
        p2 %>% clearGroup("ind") %>%
          addMarkers(
            data = d_v2,
            lng = ~Lon,
            lat = ~Lat,
            label = simple_map_labels(d_v2),
            labelOptions=labelOptions(textsize="13px", direction="auto"),
            group="ind"
          )
        
        d_v3 <- as.data.table(dbGetQuery(dbi_con, sprintf("
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
        )))
        p3 %>% clearGroup("ind") %>%
          addMarkers(
            data = d_v3,
            lng = ~Lon,
            lat = ~Lat,
            label = simple_map_labels(d_v3),
            labelOptions=labelOptions(textsize="13px", direction="auto"),
            group="ind"
          )
      } else {
        p1 %>% clearGroup("ind")
        p2 %>% clearGroup("ind")
        p3 %>% clearGroup("ind")
      }
      
    })
    
    for (i in c(
      "pltvis_g1_trans1", "pltvis_g1_trans2", "pltvis_g1_trans3",
      "pltvis_g3_trans1", "pltvis_g3_trans2", "pltvis_g3_trans3",
      "pltvis_g4_trans1", "pltvis_g4_trans2", "pltvis_g4_trans3"
    )) {
      updatePickerInput(
        session, i,
        choices = unique(c(
          d$Non_Standard_Transition_V1,
          d$Non_Standard_Transition_V2,
          d$Non_Standard_Transition_V3
        )),
        selected = "None"
      )
    }
    
    output$pltvis_g1 <- renderPlotly({
      d1 <- if (input$pltvis_g1_zeros) {
        d[Activation_Time_Difference_V1_V2 != 0]
      } else {
        d
      }
      d1 <- d1[Non_Standard_Transition_V1 %in% input$pltvis_g1_trans1 &
                 Non_Standard_Transition_V2 %in% input$pltvis_g1_trans2]
      
      d2 <- if (input$pltvis_g1_zeros) {
        d[Activation_Time_Difference_V1_V3 != 0]
      } else {
        d
      }
      d2 <- d2[Non_Standard_Transition_V1 %in% input$pltvis_g1_trans1 &
                 Non_Standard_Transition_V3 %in% input$pltvis_g1_trans3]
      
      plot_ly(alpha = 0.6) %>%
        add_histogram(data = d1, x = ~Activation_Time_Difference_V1_V2, name = "V1 vs V2") %>%
        add_histogram(data = d2, x = ~Activation_Time_Difference_V1_V3, name = "V1 vs V3") %>%
        layout(
          legend = list(x = 100, y = 0.5),
          barmode = "overlay",
          title = "Indicator Activation Time Difference",
          xaxis = list(title = "Indicator Activation Time Difference"),
          yaxis = list(title = "Frequency")
        )
    })
    
    output$pltvis_g2 <- renderPlotly({
      plot_ly(data = d, alpha = 0.6) %>%
        add_markers(x=~Activation_Time_Difference_V1_V2, y=~Base_Leg_Altidude_V2, name="V1 vs V2") %>%
        add_markers(x=~Activation_Time_Difference_V1_V3, y=~Base_Leg_Altidude_V3, name="V1 vs V3") %>%
        layout(
          legend = list(x = 100, y = 0.5),
          title = "Activation Time Difference vs QNH",
          xaxis = list(title = "Activation Time Difference", tickformat="%H:%M:%S"),
          yaxis = list(title = "Base Leg Altidude")
        )
    })
    
    output$pltvis_g3 <- renderPlotly({
      d1 <- d[Non_Standard_Transition_V1 %in% input$pltvis_g3_trans1]
      d2 <- d[Non_Standard_Transition_V2 %in% input$pltvis_g3_trans2]
      d3 <- d[Non_Standard_Transition_V3 %in% input$pltvis_g3_trans3]
      plot_ly(alpha = 0.6) %>%
        add_histogram(data = d1, x = ~Base_Leg_Lateral_Distance_V1, name = "V1") %>%
        add_histogram(data = d2, x = ~Base_Leg_Lateral_Distance_V2, name = "V2") %>%
        add_histogram(data = d3, x = ~Base_Leg_Lateral_Distance_V3, name = "V3") %>%
        layout(
          legend = list(x = 100, y = 0.5),
          barmode = "overlay",
          title = "Lateral Distance from Localiser when First Tracked",
          xaxis = list(title = "Lateral Distance From Localiser"),
          yaxis = list(title = "Frequency")
        )
    })
    
    output$pltvis_g4 <- renderPlotly({
      dummy_date <- as.POSIXct(paste0(Sys.Date(), " 00:00:00"), tz="UTC")
      d1 <- d[Non_Standard_Transition_V1 %in% input$pltvis_g4_trans1]
      d2 <- d[Non_Standard_Transition_V2 %in% input$pltvis_g4_trans2]
      d3 <- d[Non_Standard_Transition_V3 %in% input$pltvis_g4_trans3]
      plot_ly(alpha = 0.6) %>%
        add_histogram(data = d1, x = ~Indicator_Activation_Time_V1+dummy_date, name = "V1") %>%
        add_histogram(data = d2, x = ~Indicator_Activation_Time_V2+dummy_date, name = "V2") %>%
        add_histogram(data = d3, x = ~Indicator_Activation_Time_V3+dummy_date, name = "V3") %>%
        layout(
          legend = list(x = 100, y = 0.5),
          barmode = "overlay",
          title = "Altitude when First Tracked",
          xaxis = list(title = "Altitude when Indicator First Activated"),
          yaxis = list(title = "Frequency")
        )
    })
    
  })
  
}
