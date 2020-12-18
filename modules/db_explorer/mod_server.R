db_explorer_server <- function(input, output, session, con, dbi_con) {
  
  # Get custom query
  query <- eventReactive(input$db_execute, {
    input$db_query %>% sqlQuery(con,.) %>% as.data.table()
  })
  
  # Render custom query table
  output$db_output <- DT::renderDataTable({
    datatable_customised_1(query())
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
    " %>% sqlQuery(con,.) %>% as.data.table()
  })
  output$db_fp_table <- DT::renderDataTable({
    datatable_customised_1(flightplan())
  }, server = T)
  
  # tbl_Landing_Pair
  landing_pairs <- reactive({
    req(con)
    lp <- "SELECT * FROM tbl_Landing_Pair" %>% sqlQuery(con,.) %>% as.data.table()
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
    datatable_customised_1(landing_pairs())
  }, server = T)
  
  # tbl_Polygon
  volumes <- reactive({
    " SELECT * FROM tbl_Polygon
      LEFT JOIN (
        SELECT Volume_Name AS V2, Min_Altitude, Max_Altitude FROM tbl_Volume
      ) AS t ON Volume_Name = V2
    " %>% sqlQuery(con,.) %>% as.data.table() %>% .[,!c("V2")]
  })
  output$db_volumes <- DT::renderDataTable({
    datatable_customised_1(volumes())
  }, server = T)
  
  # tbl_Path_Leg
  legs <- reactive({
    " SELECT * FROM tbl_Path_Leg
    " %>% sqlQuery(con,.) %>% as.data.table()
  })
  output$db_legs <- DT::renderDataTable({
    datatable_customised_1(legs())
  }, server = T)
  
  # Flight Plan Stats
  
  db_fp_stats <- reactive({
    fp_id <- " SELECT Flight_Plan_ID FROM tbl_Flight_Plan
    " %>% sqlQuery(con,.) %>% unlist() %>% as.vector()
    fpd_id <- " SELECT Flight_Plan_ID FROM tbl_Flight_Plan_Derived
    " %>% sqlQuery(con,.) %>% unlist() %>% as.vector()
    both_id <- intersect(fp_id, fpd_id)
    track_fpid <-  " SELECT DISTINCT Flight_Plan_ID FROM tbl_Radar_Track_Point
    " %>% sqlQuery(con,.) %>% unlist() %>% as.vector()
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
    datatable_customised_2(db_fp_stats()[["fp_gen"]])
  }, server = F)
  output$db_fp_type_table <- DT::renderDataTable({
    datatable_customised_2(db_fp_stats()[["fp_ac"]])
  }, server = F)
  output$db_fp_wake_table <- DT::renderDataTable({
    datatable_customised_2(db_fp_stats()[["fp_wake"]])
  }, server = F)
  output$db_fp_lrwy_table <- DT::renderDataTable({
    datatable_customised_2(db_fp_stats()[["fp_lrwy"]])
  }, server = F)
  output$db_fp_lrwyt_table <- DT::renderDataTable({
    datatable_customised_2(db_fp_stats()[["fp_lrwyt"]])
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
    datatable_customised_2(db_lp_stats()[["lp_wake"]])
  }, server = F)
  output$db_lp_lrwy_table <- DT::renderDataTable({
    datatable_customised_2(db_lp_stats()[["lp_rwy"]])
  }, server = F)
  output$db_lp_wakerwy_table <- DT::renderDataTable({
    datatable_customised_2(db_lp_stats()[["lp_wakerwy"]])
  }, server = F)
  
  # Adaptation Data Views
  
  db_aircraft_adaptation <- reactive({
    " EXEC usp_GI_Get_ORD_Aircraft_Adaptation_Data
    " %>% sqlQuery(con,.) %>% as.data.table()
  })
  output$db_aircraft_adaptation_table <- DT::renderDataTable({
    datatable_customised_2(db_aircraft_adaptation())
  }, server = F)
  
  db_dbs_adaptation <- reactive({
    " EXEC usp_GI_Get_ORD_DBS_Adaptation_Data
    " %>% sqlQuery(con,.) %>% as.data.table()
  })
  output$db_dbs_adaptation_table <- DT::renderDataTable({
    datatable_customised_2(db_dbs_adaptation())
  }, server = F)
  
  db_runway_adaptation <- reactive({
    " EXEC usp_GI_Get_ORD_Runway_Adaptation_Data 'CYYZ'
    " %>% sqlQuery(con,.) %>% as.data.table()
  })
  output$db_runway_adaptation_table <- DT::renderDataTable({
    datatable_customised_2(db_runway_adaptation())
  }, server = F)
  
  db_wake_adaptation <- reactive({
    " EXEC usp_GI_Get_ORD_Wake_Adaptation_Data
    " %>% sqlQuery(con,.) %>% as.data.table()
  })
  output$db_wake_adaptation_table <- DT::renderDataTable({
    datatable_customised_2(db_wake_adaptation())
  }, server = F)
  
}
