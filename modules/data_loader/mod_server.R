# Notes:
# Clear database by date runs usp_DL_Clear_Day_By_Date
# Selective Purge uses usp_DL_Clear_Days_By_Aircraft_Type
# Pre-ORD runs UTMA_ORD_Validation_Pre_Processing.sql
# Run ORD runs UTMA_ORD_Validation_Run.sql
# Pre-PLT runs UTMA_PLT_Validation_Pre_Processing.sql
# Run PLT runs UTMA_PLT_Validation_Run.sql

# Current limitations:
# Cannot update existing flight plan rows with new information found. (Can only append new flight plan rows)

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
    message("[",Sys.time(),"] ", "Finished loading file: ", basename(LogFilePaths[i]), " (time elapsed: ", Time_String_From_Seconds(as.numeric(difftime(t2, t1, units = "secs"))), ")")
  }
  t4 <- Sys.time()
  message("[",Sys.time(),"] ", "Finished loading ", length(LogFilePaths), " file(s) - total time elapsed: ", Time_String_From_Seconds(as.numeric(difftime(t4, t0, units = "secs"))))
  
}

data_loader_server <- function(input, output, session, con, dbi_con) {

  ns <- session$ns
  
  observeEvent(input$clear_db, {
    output$clear_db_confirmed_wrapper <- renderUI({
      actionButton(ns("clear_db_confirmed"), "Confirm Clear Database")
    })
  })
  
  observeEvent(input$clear_db_confirmed, {
    withCallingHandlers({
      shinyjs::html("console_output", "")
      message("[",Sys.time(),"] ", "Clearing database...")
      dbGetQuery(dbi_con, read_SQL_File("modules/data_loader/usp_DL_Clear_Main_Tables.sql"))
    },
    message = function(m) {
      shinyjs::html(id = "console_output", html = paste0(m$message, "<br>"), add = TRUE)
    })
  })
  
  # Load Raw Data
  
  observeEvent(input$logs_load, {
    showModal(modalDialog(
      div(
        class = "centered",
        h3("WARNING: Confirm Log Loading Operation")
      ),
      div(style = "height: 10px"),
      h4("Are you sure you wish to load the following file(s)?"),
      HTML("<li>", paste0(log_files(), collapse = "</li><li>"), "</li>"),
      div(style = "height: 10px"),
      h4("Log Type/Format"),
      input$logs_type,
      div(style = "height: 10px"),
      h4("Database"),
      as.character(dbGetQuery(dbi_con, "SELECT DB_NAME()")),
      size = "m",
      footer = div(
        class = "centered",
        modalButton("Cancel"),
        div(style = "width: 15px"),
        actionButton(ns("logs_load_confirm"), "Confirm")
      ),
      easyClose = F
    ))
  })
  
  shinyFileChoose(input, "logs_select", roots=getVolumes())
  
  log_files <- reactive({
    as.vector(parseFilePaths(roots=getVolumes(), input$logs_select)$datapath)
  })
  
  observeEvent(input$logs_load_confirm, {
    removeModal()
    withCallingHandlers({
      shinyjs::html("console_output", "")
      read_logs(log_files(), input$logs_type, dbi_con)
    },
    message = function(m) {
      shinyjs::html(id = "console_output", html = paste0(m$message, "<br>"), add = TRUE)
    })
  })
  
  # Load Config
  
  shinyDirChoose(input, "load_config_folder", roots=getVolumes()(), session=session, restrictions = system.file(package = 'base'))
  
  config_files <- reactive({
    return(parseDirPath(roots=getVolumes()(), input$load_config_folder))
  })
  
  observeEvent(input$load_config, {
    withCallingHandlers({
      shinyjs::html("console_output_config", "")
      message("[",Sys.time(),"] ", "Clearing existing configuration data...")
      dbSendQuery(dbi_con, read_SQL_File("modules/data_loader/usp_DL_Clear_Config_Tables.sql"))
      Sys.sleep(2)
      import_configs(config_files(), dbi_con)
    },
    message = function(m) {
      shinyjs::html(id = "console_output_config", html = paste0(m$message, "<br>"), add = TRUE)
    })
  })
  
}
