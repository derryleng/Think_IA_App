source("modules/data_loader/TBS_Log_Loader.R", local = T)

source("modules/data_loader/Asterix_Log_Loader.R", local = T)

source("modules/data_loader/NavCan_Log_Loader.R", local = T)

source("modules/data_loader/LVNL_Log_Loader.R", local = T)

source("modules/data_loader/Config_Management.R", local = T)

read_logs <- function(LogFilePaths, input, dbi_con) {
  
  tbl_list <- c("tbl_Adaptation_Data", "tbl_Runway", "tbl_Airfield")
  tbl <- lapply(tbl_list, function(i) as.data.table(dbGetQuery(dbi_con, paste("SELECT * FROM", i))))
  names(tbl) <- gsub("^tbl_(.*)$", "\\1", tbl_list)
  
  t0 <- Sys.time()
  for (i in 1:length(LogFilePaths)) {
    message("[",Sys.time(),"] ", "Begin loading file: ", basename(LogFilePaths[i]), " (", i, " of ", length(LogFilePaths), ")")
    t1 <- Sys.time()
    if (input$logs_type == "eTBS system logs (NATS)") {
      process_eTBS_logs(LogFilePaths[i], tbl$Adaptation_Data, tbl$Runway, tbl$Airfield$Airfield_Name, dbi_con)
    } else if (input$logs_type == "Cat48 radar (NATS)") {
      process_Asterix_Cat48(LogFilePaths[i], tbl$Adaptation_Data, tbl$Runway, dbi_con)
    } else if (input$logs_type == "Cat62 radar (NATS)") {
      process_Asterix_Cat62(LogFilePaths[i], tbl$Adaptation_Data, tbl$Runway, dbi_con)
    } else if (input$logs_type == "Cat20 radar (NATS)") {
      process_Asterix_Cat20(LogFilePaths[i], tbl$Adaptation_Data, tbl$Runway, dbi_con)
    } else if (input$logs_type == "Non-Mode_S radar (NAVCAN)") {
      process_NavCan_RadarNonModeS(LogFilePaths[i], tbl$Adaptation_Data, tbl$Runway, dbi_con)
    } else if (input$logs_type == "Flight Plan logs (NAVCAN)") {
      process_NavCan_FP(LogFilePaths[i], dbi_con)
    } else if (input$logs_type == "Alt Flight Plan logs (NAVCAN)") {
      process_NavCan_FPAlt(LogFilePaths[i], dbi_con)
    } else if (input$logs_type == "Ground radar (NAVCAN)") {
      process_NavCan_GR(LogFilePaths[i], tbl$Runway, dbi_con)
    } else if (input$logs_type == "Surface wind and QNH (NAVCAN)") {
      process_NavCan_SurfaceWindQNH(LogFilePaths[i], tbl$Airfield$Airfield_Name, dbi_con)
    } else if (input$logs_type == "Surveillance radar (LVNL)") {
      process_LVNL_Surv(LogFilePaths[i], tbl$Adaptation_Data, Runway, dbi_con)
    } else if (input$logs_type == "Flight Plan logs (LVNL)") {
      process_LVNL_FP(LogFilePaths[i], dbi_con)
    } else if (input$logs_type == "QNH logs (LVNL)") {
      process_LVNL_QNH(LogFilePaths[i], tbl$Airfield$Airfield_Name, dbi_con)
    } else if (input$logs_type == "Surface Wind logs (LVNL)") {
      process_LVNL_SurfaceWind(LogFilePaths[i], tbl$Airfield$Airfield_Name, dbi_con)
    }
    t2 <- Sys.time()
    message("[",Sys.time(),"] ", "Finished loading file: ", basename(LogFilePaths[i]), " (time elapsed: ", Time_String_From_Seconds(as.numeric(difftime(t2, t1, units = "secs"))), ")")
  }
  t4 <- Sys.time()
  message("[",Sys.time(),"] ", "Finished loading ", length(LogFilePaths), " file(s) - total time elapsed: ", Time_String_From_Seconds(as.numeric(difftime(t4, t0, units = "secs"))))
  
}

generic_confirm_dialogue <- function(SQLFileName, actionButtonName, dbi_con) {
  modalDialog(
    div(
      style = "text-align: center",
      h3("MODIFY DATABASE WARNING")
    ),
    hr(),
    div(
      style = "text-align: center",
      tags$b("Do you wish to proceed?")
    ),
    h5("SQL Script"),
    SQLFileName,
    h5("Database"),
    as.character(dbGetQuery(isolate(dbi_con), "SELECT DB_NAME()")),
    size = "s",
    footer = div(
      class = "centered",
      modalButton("Cancel"),
      div(style = "width: 15px"),
      actionButton(actionButtonName, "Confirm")
    ),
    easyClose = F
  )
}

additional_processing <- function(dbi_con) {
  
  Airfield_Name <- as.vector(unlist(dbGetQuery(dbi_con, "SELECT * FROM tbl_Airfield")$Airfield_Name))
  
  if (Airfield_Name == "EHAM") {
    dbSendQuery(dbi_con, read_SQL_File("modules/data_loader/Schiphol_UTMA_Validation_DB_Scripts/Schiphol_Generate_Surface_Wind_Entries.sql"))
  } else if (Airfield_Name == "CYYZ") {
    dbSendQuery(dbi_con, read_SQL_File("modules/data_loader/Toronto_UTMA_Validation_DB_Scripts/Remove_Stationary_Track_Updates.sql"))
    Sys.sleep(5)
    dbSendQuery(dbi_con, read_SQL_File("modules/data_loader/Toronto_UTMA_Validation_DB_Scripts/Remove_Stationary_Track_Updates.sql"))
    Sys.sleep(5)
    dbSendQuery(dbi_con, read_SQL_File("modules/data_loader/Toronto_UTMA_Validation_DB_Scripts/Toronto_Generate_Surface_Wind_Entries.sql"))
  }
  
}

data_loader_server <- function(input, output, session, con, dbi_con) {

  ns <- session$ns
  
  # Load Config
  
  shinyDirChoose(input, "load_config_folder", roots=getVolumes()(), session=session, restrictions = system.file(package = 'base'))
  
  config_files <- reactive({
    return(parseDirPath(roots=getVolumes()(), input$load_config_folder))
  })
  
  observeEvent(input$load_config, {
    withCallingHandlers({
      shinyjs::html("console_output_config", "")
      message("[",Sys.time(),"] ", "Clearing existing configuration data...")
      dbSendQuery(isolate(dbi_con), read_SQL_File("modules/data_loader/usp_DL_Clear_Config_Tables.sql"))
      Sys.sleep(2)
      import_Config(isolate(config_files()), isolate(dbi_con), isolate(input$load_legacy_wake), isolate(input$load_DW_volumes))
    },
    message = function(m) {
      shinyjs::html(id = "console_output_config", html = paste0(m$message, "<br>"), add = TRUE)
    })
  })
  
  # Export Config
  
  shinyDirChoose(input, "export_config_folder", roots=getVolumes()(), session=session, restrictions = system.file(package = 'base'))
  
  export_dir <- reactive({
    return(parseDirPath(roots=getVolumes()(), input$export_config_folder))
  })
  
  observeEvent(input$export_xml, {
    withCallingHandlers({
      shinyjs::html("console_output_config", "")
      export_Config(isolate(export_dir()), isolate(dbi_con), isolate(input$export_xml_version))
    },
    message = function(m) {
      shinyjs::html(id = "console_output_config", html = paste0(m$message, "<br>"), add = TRUE)
    })
  })
  
  # Clear Database
  
  observeEvent(input$clear_db, {
    showModal(generic_confirm_dialogue("usp_DL_Clear_Main_Tables.sql", ns("clear_db_confirmed"), dbi_con))
  })
  
  observeEvent(input$clear_db_confirmed, {
    removeModal()
    withCallingHandlers({
      shinyjs::html("console_output", "")
      message("[",Sys.time(),"] ", "Clearing database...")
      dbGetQuery(isolate(dbi_con), isolate(read_SQL_File("modules/data_loader/usp_DL_Clear_Main_Tables.sql")))
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
        h3("MODIFY DATABASE WARNING")
      ),
      div(style = "height: 10px"),
      h4("Are you sure you wish to load the following file(s)?"),
      HTML("<li>", paste0(log_files(), collapse = "</li><li>"), "</li>"),
      div(style = "height: 10px"),
      h4("Log Type/Format"),
      input$logs_type,
      div(style = "height: 10px"),
      h4("Database"),
      as.character(dbGetQuery(isolate(dbi_con), "SELECT DB_NAME()")),
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
      read_logs(log_files(), input, isolate(dbi_con))
    },
    message = function(m) {
      shinyjs::html(id = "console_output", html = paste0(m$message, "<br>"), add = TRUE)
    })
  })
  
  # IA Processing
  
  observeEvent(input$ord_pre_proc, {
    showModal(generic_confirm_dialogue("UTMA_ORD_Validation_Pre_Processing.sql", ns("ord_pre_proc_confirm"), dbi_con))
  })
  
  observeEvent(input$ord_pre_proc_confirm, {
    removeModal()
    ord_pre_proc_date <- input$ord_pre_proc_date
    ord_pre_proc <- read_SQL_File("modules/data_loader/UTMA_Validation_Data_Loader_Scripts/UTMA_ORD_Validation_Pre_Processing.sql")
    if (grepl("^[0-9]{2}/[0-9]{2}/[0-9]{2}$", ord_pre_proc_date)) {
      ord_pre_proc <- gsub("\\$\\$PARAMETER\\$\\$", ord_pre_proc_date, ord_pre_proc)
    }
    dbGetQuery(dbi_con, ord_pre_proc)
  })
  
  observeEvent(input$ord_proc, {
    showModal(generic_confirm_dialogue("UTMA_ORD_Validation_Run.sql", ns("ord_proc_confirm"), dbi_con))
  })
  
  observeEvent(input$ord_proc_confirm, {
    removeModal()
    ord_proc_date <- input$ord_proc_date
    ord_proc <- read_SQL_File("modules/data_loader/UTMA_Validation_Data_Loader_Scripts/UTMA_ORD_Validation_Run.sql")
    if (grepl("^[0-9]{2}/[0-9]{2}/[0-9]{2}$", ord_proc_date)) {
      ord_proc <- gsub("\\$\\$PARAMETER\\$\\$", ord_proc_date, ord_proc)
    }
    dbGetQuery(dbi_con, ord_proc)
  })
  
  observeEvent(input$plt_pre_proc, {
    showModal(generic_confirm_dialogue("UTMA_PLT_Validation_Pre_Processing.sql", ns("plt_pre_proc_confirm"), dbi_con))
  })
  
  observeEvent(input$plt_pre_proc_confirm, {
    removeModal()
    plt_pre_proc_date <- input$plt_pre_proc_date
    plt_pre_proc <- read_SQL_File("modules/data_loader/UTMA_Validation_Data_Loader_Scripts/UTMA_PLT_Validation_Pre_Processing.sql")
    if (grepl("^[0-9]{2}/[0-9]{2}/[0-9]{2}$", plt_pre_proc_date)) {
      plt_pre_proc <- gsub("\\$\\$PARAMETER\\$\\$", plt_pre_proc_date, plt_pre_proc)
    }
    dbGetQuery(dbi_con, plt_pre_proc)
  })
  
  observeEvent(input$plt_proc, {
    showModal(generic_confirm_dialogue("UTMA_PLT_Validation_Run.sql", ns("plt_proc_confirm"), dbi_con))
  })
  
  observeEvent(input$plt_proc_confirm, {
    removeModal()
    plt_proc_date <- input$plt_proc_date
    plt_proc <- read_SQL_File("modules/data_loader/UTMA_Validation_Data_Loader_Scripts/UTMA_PLT_Validation_Run.sql")
    if (grepl("^[0-9]{2}/[0-9]{2}/[0-9]{2}$", plt_proc_date)) {
      plt_proc <- gsub("\\$\\$PARAMETER\\$\\$", plt_proc_date, plt_proc)
    }
    dbGetQuery(dbi_con, plt_proc)
  })
  
  observeEvent(input$selective_purge, {
    showModal(generic_confirm_dialogue("usp_DL_Clear_Days_By_Aircraft_Type.sql", ns("selective_purge_confirm"), dbi_con))
  })
  
  observeEvent(input$selective_purge_confirm, {
    removeModal()
    selective_purge_actype <- input$selective_purge_actype
    selective_purge <- read_SQL_File("modules/data_loader/usp_DL_Clear_Days_By_Aircraft_Type.sql")
    sqlInterpolate(ANSI(), selective_purge, actype = selective_purge_actype)
    dbGetQuery(dbi_con, selective_purge)
  })
  
}
