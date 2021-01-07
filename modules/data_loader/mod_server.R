# Notes:
# Clear database runs usp_DL_Clear_Main_Tables
# Clear database by date runs usp_DL_Clear_Day_By_Date
# Selective Purge uses usp_DL_Clear_Days_By_Aircraft_Type
# Pre-ORD runs UTMA_ORD_Validation_Pre_Processing.sql
# Run ORD runs UTMA_ORD_Validation_Run.sql
# Pre-PLT runs UTMA_PLT_Validation_Pre_Processing.sql
# Run PLT runs UTMA_PLT_Validation_Run.sql
# Load Config runs usp_DL_Clear_Config_Tables first

# Current limitations:
# Cannot update existing flight plan rows with new information found. (Can only append new flight plan rows)

source("modules/data_loader/resources.R", local = T)

source("modules/data_loader/TBS_Log_Loader.R", local = T)

source("modules/data_loader/Asterix_Log_Loader.R", local = T)

source("modules/data_loader/NavCan_Log_Loader.R", local = T)

source("modules/data_loader/LVNL_Log_Loader.R", local = T)

source("modules/data_loader/Config_Management.R", local = T)

read_logs <- function(LogFilePaths, LogFileType, dbi_con) {
  
  tbl_Adaptation_Data <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Adaptation_Data"))
  
  tbl_Runway <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Runway"))
  
  Airfield_Name <- as.vector(unlist(dbGetQuery(dbi_con, "SELECT * FROM tbl_Airfield")$Airfield_Name))
  
  t0 <- Sys.time()
  for (i in 1:length(LogFilePaths)) {
    message("[",Sys.time(),"] ", "Begin loading file: ", basename(LogFilePaths[i]), " (", i, " of ", length(LogFilePaths), ")")
    t1 <- Sys.time()
    if (LogFileType == "eTBS system logs (NATS)") {
      process_eTBS_logs(LogFilePaths[i], tbl_Adaptation_Data, tbl_Runway, Airfield_Name, dbi_con)
    } else if (LogFileType == "Cat48 radar (NATS)") {
      process_Asterix_Cat48(LogFilePaths[i], tbl_Adaptation_Data, tbl_Runway, dbi_con)
    } else if (LogFileType == "Cat62 radar (NATS)") {
      process_Asterix_Cat62(LogFilePaths[i], tbl_Adaptation_Data, tbl_Runway, dbi_con)
    } else if (LogFileType == "Cat20 radar (NATS)") {
      process_Asterix_Cat20(LogFilePaths[i], tbl_Adaptation_Data, tbl_Runway, dbi_con)
    } else if (LogFileType == "Non-Mode_S radar (NAVCAN)") {
      process_NavCan_RadarNonModeS(LogFilePaths[i], tbl_Adaptation_Data, tbl_Runway, dbi_con)
    } else if (LogFileType == "Flight Plan logs (NAVCAN)") {
      process_NavCan_FP(LogFilePaths[i], dbi_con)
    } else if (LogFileType == "Alt Flight Plan logs (NAVCAN)") {
      process_NavCan_FPAlt(LogFilePaths[i], dbi_con)
    } else if (LogFileType == "Ground radar (NAVCAN)") {
      process_NavCan_GR(LogFilePaths[i], tbl_Runway, dbi_con)
    } else if (LogFileType == "Surface wind and QNH (NAVCAN)") {
      process_NavCan_SurfaceWindQNH(LogFilePaths[i], Airfield_Name, dbi_con)
    } else if (LogFileType == "Surveillance radar (LVNL)") {
      process_LVNL_Surv(LogFilePaths[i], tbl_Adaptation_Data, tbl_Runway, dbi_con)
    } else if (LogFileType == "Flight Plan logs (LVNL)") {
      process_LVNL_FP(LogFilePaths[i], dbi_con)
    } else if (LogFileType == "QNH logs (LVNL)") {
      process_LVNL_QNH(LogFilePaths[i], Airfield_Name, dbi_con)
    } else if (LogFileType == "Surface Wind logs (LVNL)") {
      process_LVNL_SurfaceWind(LogFilePaths[i], Airfield_Name, dbi_con)
    }
    t2 <- Sys.time()
    message("[",Sys.time(),"] ", "Finished loading file: ", basename(LogFilePaths[i]), " (time elapsed: ", dhms(as.numeric(difftime(t2, t1, units = "secs"))), ")")
  }
  t4 <- Sys.time()
  message("[",Sys.time(),"] ", "Finished loading ", length(LogFilePaths), " file(s) - total time elapsed: ", dhms(as.numeric(difftime(t4, t0, units = "secs"))))
  
}

data_loader_server <- function(input, output, session, con, dbi_con) {
  
  # observeEvent(input$clear_db, {
  #   output$clear_db_confirmed_wrapper <- renderUI({
  #     actionButton(ns("clear_db_confirmed"), "Confirm Clear Database")
  #   })
  # })
  
  observeEvent(input$clear_db, {
    # dbSendQuery(dbi_con, "EXEC usp_DL_Clear_Main_Tables @Dummy_Parameter = ''")
    dbSendQuery(dbi_con, "
      -- Note that order is important, ie reverse order of hierarchies.
    	DELETE FROM tbl_Mode_S_Wind_Seg_Forecast
    	IF IDENT_CURRENT('tbl_Mode_S_Wind_Seg_Forecast') >= 1
    		DBCC CHECKIDENT (tbl_Mode_S_Wind_Seg_Forecast, RESEED, 0)
    
    	DELETE FROM tbl_Mode_S_Wind_Point_Filter_Stats
    	IF IDENT_CURRENT('tbl_Mode_S_Wind_Point_Filter_Stats') >= 1
    		DBCC CHECKIDENT (tbl_Mode_S_Wind_Point_Filter_Stats, RESEED, 0)
    
    	DELETE FROM tbl_Mode_S_Wind_Forecast
    	IF IDENT_CURRENT('tbl_Mode_S_Wind_Forecast') >= 1
    		DBCC CHECKIDENT (tbl_Mode_S_Wind_Forecast, RESEED, 0)
    
    	DELETE FROM tbl_Mode_S_Wind_Observation
    	IF IDENT_CURRENT('tbl_Mode_S_Wind_Observation') >= 1
    		DBCC CHECKIDENT (tbl_Mode_S_Wind_Observation, RESEED, 0)
    
    	DELETE FROM tbl_Mode_S_Wind_Seg_Flags
    	IF IDENT_CURRENT('tbl_Mode_S_Wind_Seg_Flags') >= 1
    		DBCC CHECKIDENT (tbl_Mode_S_Wind_Seg_Flags, RESEED, 0)
    
    	DELETE FROM tbl_Mode_S_Wind_Seg
    	IF IDENT_CURRENT('tbl_Mode_S_Wind_Seg') >= 1
    		DBCC CHECKIDENT (tbl_Mode_S_Wind_Seg, RESEED, 0)
    
    	DELETE FROM tbl_PLT_Abnormal_Transition
    	IF IDENT_CURRENT('tbl_PLT_Abnormal_Transition') >= 1
    		DBCC CHECKIDENT (tbl_PLT_Abnormal_Transition, RESEED, 0)
    
    	DELETE FROM tbl_Previous_Radar_Track_Point_Derived
    	IF IDENT_CURRENT('tbl_Previous_Radar_Track_Point_Derived') >= 1
    		DBCC CHECKIDENT (tbl_Previous_Radar_Track_Point_Derived, RESEED, 0)
    
    	DELETE FROM tbl_Radar_Track_Point_Derived
    	IF IDENT_CURRENT('tbl_Radar_Track_Point_Derived') >= 1
    		DBCC CHECKIDENT (tbl_Radar_Track_Point_Derived, RESEED, 0)
    
    	DELETE FROM tbl_Radar_Track_Point
    	IF IDENT_CURRENT('tbl_Radar_Track_Point') >= 1
    		DBCC CHECKIDENT (tbl_Radar_Track_Point, RESEED, 0)
    
    	DELETE FROM tbl_PLT_Analysis_Report
    	IF IDENT_CURRENT('tbl_PLT_Analysis_Report') >= 1
    		DBCC CHECKIDENT (tbl_PLT_Analysis_Report, RESEED, 0)
    
    	DELETE FROM tbl_Previous_PLT_Analysis_Report
    	IF IDENT_CURRENT('tbl_Previous_PLT_Analysis_Report') >= 1
    		DBCC CHECKIDENT (tbl_Previous_PLT_Analysis_Report, RESEED, 0)
    
    	DELETE FROM tbl_Flight_Plan_Derived
    	IF IDENT_CURRENT('tbl_Flight_Plan_Derived') >= 1
    		DBCC CHECKIDENT (tbl_Flight_Plan_Derived, RESEED, 0)
    
    	DELETE FROM tbl_Flight_Plan
    	IF IDENT_CURRENT('tbl_Flight_Plan') >= 1
    		DBCC CHECKIDENT (tbl_Flight_Plan, RESEED, 0)
    
    	DELETE FROM tbl_ORD_Aircraft_Profile
    	IF IDENT_CURRENT('tbl_ORD_Aircraft_Profile') >= 1
    		DBCC CHECKIDENT (tbl_ORD_Aircraft_Profile, RESEED, 0)
    
    	DELETE FROM tbl_ORD_IAS_Profile
    	IF IDENT_CURRENT('tbl_ORD_IAS_Profile') >= 1
    		DBCC CHECKIDENT (tbl_ORD_IAS_Profile, RESEED, 0)
    
    	DELETE FROM tbl_ORD_GS_Profile
    	IF IDENT_CURRENT('tbl_ORD_GS_Profile') >= 1
    		DBCC CHECKIDENT (tbl_ORD_GS_Profile, RESEED, 0)
    
    	DELETE FROM tbl_ORD_Prediction
    	IF IDENT_CURRENT('tbl_ORD_Prediction') >= 1
    		DBCC CHECKIDENT (tbl_ORD_Prediction, RESEED, 0)
    
    	DELETE FROM tbl_ORD_Observation
    	IF IDENT_CURRENT('tbl_ORD_Observation') >= 1
    		DBCC CHECKIDENT (tbl_ORD_Observation, RESEED, 0)
    
    	DELETE FROM tbl_eTBS_Performance_Model
    	IF IDENT_CURRENT('tbl_eTBS_Performance_Model') >= 1
    		DBCC CHECKIDENT (tbl_eTBS_Performance_Model, RESEED, 0)
    
    	DELETE FROM tbl_All_Pair_Reference_Data
    	IF IDENT_CURRENT('tbl_All_Pair_Reference_Data') >= 1
    		DBCC CHECKIDENT (tbl_All_Pair_Reference_Data, RESEED, 0)
    
    	DELETE FROM tbl_All_Pair_Radar_Track_Point
    	IF IDENT_CURRENT('tbl_All_Pair_Radar_Track_Point') >= 1
    		DBCC CHECKIDENT (tbl_All_Pair_Radar_Track_Point, RESEED, 0)
    
    	DELETE FROM tbl_Landing_Pair
    	IF IDENT_CURRENT('tbl_Landing_Pair') >= 1
    		DBCC CHECKIDENT (tbl_Landing_Pair, RESEED, 0)
    
    	DELETE FROM tbl_Departure_Pair_Separation_Evolution
    	IF IDENT_CURRENT('tbl_Departure_Pair_Separation_Evolution') >= 1
    		DBCC CHECKIDENT (tbl_Departure_Pair_Separation_Evolution, RESEED, 0)
    
    	DELETE FROM tbl_Departure_Pair_Separation
    	IF IDENT_CURRENT('tbl_Departure_Pair_Separation') >= 1
    		DBCC CHECKIDENT (tbl_Departure_Pair_Separation, RESEED, 0)
    
    	DELETE FROM tbl_Departure_Pair
    	IF IDENT_CURRENT('tbl_Departure_Pair') >= 1
    		DBCC CHECKIDENT (tbl_Departure_Pair, RESEED, 0)
    
    	DELETE FROM tbl_Baro
    	IF IDENT_CURRENT('tbl_Baro') >= 1
    		DBCC CHECKIDENT (tbl_Baro, RESEED, 0)
    
    	DELETE FROM tbl_Anemometer
    	IF IDENT_CURRENT('tbl_Anemometer') >= 1
    		DBCC CHECKIDENT (tbl_Anemometer, RESEED, 0)          
    ")
  })
  
  shinyFileChoose(input, "logs_select", roots=getVolumes())
  
  log_files <- reactive({
    as.vector(parseFilePaths(roots=getVolumes(), input$logs_select)$datapath)
  })
  
  # Confirmation dialogue is tricky, how to define new UI in ns within server fn for Shiny modules?
  # observeEvent(input$logs_load, {
  #   showModal(modalDialog(
  #     div(
  #       class = "centered",
  #       h3("WARNING: Confirm Log Loading Operation")
  #     ),
  #     div(style = "height: 10px"),
  #     h4("Are you sure you wish to load the following file(s)?"),
  #     HTML("<li>", paste0(log_files(), collapse = "</li><li>"), "</li>"),
  #     div(style = "height: 10px"),
  #     h4("Log Type/Format"),
  #     input$logs_type,
  #     div(style = "height: 10px"),
  #     h4("Database"),
  #     as.character(dbGetQuery(dbi_con, "SELECT DB_NAME()")),
  #     size = "m",
  #     footer = div(
  #       class = "centered",
  #       modalButton("Cancel"),
  #       div(style = "width: 15px"),
  #       actionButton("logs_load_confirm", "Confirm")
  #     ),
  #     easyClose = F
  #   ))
  # })
  
  observeEvent(input$logs_load, {
    withCallingHandlers({
      shinyjs::html("console_output", "")
      read_logs(log_files(), input$logs_type, dbi_con)
    },
    message = function(m) {
      shinyjs::html(id = "console_output", html = paste0(m$message, "<br>"), add = TRUE)
    })
  })
  
}
