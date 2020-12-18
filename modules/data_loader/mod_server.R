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

read_logs <- function(LogFilePaths, LogFileType, tbl_Adaptation_Data, tbl_Runway, Airfield_Name, dbi_con) {
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
  
  tbl_Adaptation_Data <- reactive({
    as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Adaptation_Data"))
  })
  
  tbl_Runway <- reactive({
    as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Runway"))
  })
  
  Airfield_Name <- reactive({
    as.vector(unlist(dbGetQuery(dbi_con, "SELECT * FROM tbl_Airfield")$Airfield_Name))
  })
  
  
  
}
