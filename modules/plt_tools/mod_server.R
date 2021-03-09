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

tbl_template <- list(
  tbl_Runway = data.table(
    Runway_Name = NA,
    Airfield_Name = NA,
    Heading = NA,
    Runway_Group = NA,
    Approach_Direction = NA
  ),
  tbl_Volumes = data.table(
    Volume_Name = NA,
    Min_Altitude = NA,
    Max_Altitude = NA,
    Start_Dist_From_Threshold = NA,
    End_Dist_From_Threshold = NA,
    Lateral_Dist_Left = NA,
    Lateral_Dist_Right = NA
  ),
  tbl_Path_Leg = data.table(
    Path_Leg_Name = NA,
    Landing_Runway = NA,
    Is_Intercept_Leg = NA,
    Is_ILS_Leg = NA,
    Is_Landing_Leg = NA,
    Path_Leg_Type = NA
  ),
  tbl_Path_Leg_Transition = data.table(
    Current_Path_Leg = NA,
    New_Path_Leg = NA,
    Min_Heading = NA,
    Max_Heading = NA,
    Volume_Name = NA,
    Min_Sustained_RoCD = NA,
    Runway_Name = NA,
    Associated_Runway = NA
  )
)

plt_tools_server <- function(input, output, session, con, dbi_con) {
  
  ns <- session$ns
  
  tbl_Adaptation_Data <- reactive({
    as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Adaptation_Data"))
  })
  
  tbl_Runway <- reactive({
    as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Runway"))
  })
  
  # Blank templates
  
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
  
  # Load UI elements
  
  observe({
    
    req(tbl$loaded)
    
    output$editor_view <- renderUI({
      div(
        style = "display: flex; flex-direction: column; flex-basis: 100%; gap: 30px",
        div(
          style = "flex: 1; overflow-x: auto",
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
          style = "flex: 1; height: calc(100vh - 265px);",
          div(
            style = "display: flex; background: #D51067; border-radius: 6px 6px 0 0; margin-top: -40px",
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
          leafletOutput(ns("map"), height = "100%")
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
  
  # Create New Adaptation
  
  observeEvent(input$editor_template, {
    tbl$loaded <- T
    tbl$tbl_Runway <- tbl$tbl_Runway[0]
    tbl$tbl_Volumes <- tbl$tbl_Volumes[0]
    tbl$tbl_Volumes_2 <- tbl$tbl_Volumes_2[0]
    tbl$tbl_Volumes_3 <- tbl$tbl_Volumes_3[0]
    tbl$tbl_Path_Leg <- tbl$tbl_Path_Leg[0]
    tbl$tbl_Path_Leg_2 <- tbl$tbl_Path_Leg_2[0]
    tbl$tbl_Path_Leg_3 <- tbl$tbl_Path_Leg_3[0]
    tbl$tbl_Path_Leg_Transition <- tbl$tbl_Path_Leg_Transition[0]
    tbl$tbl_Path_Leg_Transition_2 <- tbl$tbl_Path_Leg_Transition_2[0]
    tbl$tbl_Path_Leg_Transition_3 <- tbl$tbl_Path_Leg_Transition_3[0]
  })
  
  # Load Existing Adaptation
  
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
        size = "s",
        footer = div(class = "centered", modalButton("Dismiss")),
        easyClose = F
      ))
    }
    
  })
  
  # Export
  
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
  
  # Show/hide additional variant tables
  
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
  
  # Hots

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
  
  # Update map toggles based on hots
  
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
  
  # Preview map
  
  output$map <- renderLeaflet({
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
  
  observeEvent(input$toggle_tbl_Volumes, {
    p <- leafletProxy("map") %>% clearGroup("Volumes")
    for (i in input$toggle_tbl_Volumes) {
      v_i <- hot_tbl_Volumes()[Volume_Name %in% i][1]
      v2_i <- usp_GI_Latlong_From_XY(
        c(v_i$Start_Dist_From_Threshold, v_i$Start_Dist_From_Threshold, v_i$End_Dist_From_Threshold, v_i$End_Dist_From_Threshold, v_i$Start_Dist_From_Threshold),
        c(v_i$Lateral_Dist_Left, v_i$Lateral_Dist_Right, v_i$Lateral_Dist_Right, v_i$Lateral_Dist_Left, v_i$Lateral_Dist_Left),
        tbl_Adaptation_Data()
      )
      # v2_i <- data.table(
      #   PositionLongitude=c(v_i$Start_Dist_From_Threshold, v_i$Start_Dist_From_Threshold, v_i$End_Dist_From_Threshold, v_i$End_Dist_From_Threshold, v_i$Start_Dist_From_Threshold),
      #   PositionLatitude=c(v_i$Lateral_Dist_Left, v_i$Lateral_Dist_Right, v_i$Lateral_Dist_Right, v_i$Lateral_Dist_Left, v_i$Lateral_Dist_Left)
      # )
      # leaflet() %>% addTiles() %>% addPolygons(data = p_i)
      p_i <- Polygon(
        data.table(
          Longitude = v2_i$PositionLongitude*180/pi,
          Latitude = v2_i$PositionLatitude*180/pi
        )
      )
      p %>% addPolygons(
        data = p_i,
        group = "Volumes"
      )
    }
  })
  
  observeEvent(input$toggle_tbl_Volumes_2, {
    
  })
  
  observeEvent(input$toggle_tbl_Volumes_3, {
    
  })
  
}
