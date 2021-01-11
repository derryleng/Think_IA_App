process_LVNL_Surv <- function(LogFilePath, tbl_Adaptation_Data, tbl_Runway, dbi_con) {
  
  message("[",Sys.time(),"] ", "Begin processing LVNL surveillance log file...")
  logs <- fread(LogFilePath, header = F, skip = 1, na.strings = c("NA", "N/A", "NULL", ""))
  message("[",Sys.time(),"] ", "Read ", nrow(logs), " lines.")
  
  names(logs)[1:17] <- c(
    "date",        # date (yyyymmdd)
    "time",        # time In unix timestamps
    "CALLSIGN",    # callsign 
    "SSR",         # ssr mode a code
    "modec",       # mode-c from the aircraft
    "x",           # x In Nm center coordinate Is SPL-Tower (coordinates are In a file In the sharepoint directory)
    "y",           # y In Nm idem
    "tspeed",      # track speed
    "theading",    # track heading
    "mdsgs",       # mode-s ground speed
    "mdsias",      # mode-s indicated airspeed
    "mdstas",      # mode-s True airspeed
    "mdsmah",      # mode-s magnetic heading
    "mdstta",      # mode-s True track angle
    "mdsttar",     # mode-s True track angle rate (Is Not filled In the dataset)
    "mdsra",       # mode-s roll angle
    "adres"        # non-null Object mode-s address
  )
  
  x <- logs[!is.na(SSR) & !is.na(CALLSIGN) & !is.na(modec)]
  
  x <- x[!grepl("^7777$", SSR) & !grepl("^7000$", SSR) & as.numeric(modec) > -10 & as.numeric(modec) <= 100]
  
  if (nrow(x) > 0) {
    
    x$x <- as.numeric(x$x) * NmToMetres
    x$y <- as.numeric(x$y) * NmToMetres
    
    x <- x[
      x >= mean(tbl_Runway$Threshold_X_Pos) - tbl_Adaptation_Data$Load_X_Range &
        x <= mean(tbl_Runway$Threshold_X_Pos) + tbl_Adaptation_Data$Load_X_Range &
        y >= mean(tbl_Runway$Threshold_Y_Pos) - tbl_Adaptation_Data$Load_Y_Range &
        y <= mean(tbl_Runway$Threshold_Y_Pos) + tbl_Adaptation_Data$Load_Y_Range
    ]
    
    x <- cbind(x, Latlong_From_XY(x$x, x$y, tbl_Adaptation_Data))
    
  }
  
  message("[",Sys.time(),"] ", "Retrieved ", nrow(x), " valid rows.")
  
  out <- data.table(
    Flight_Plan_ID = NA,
    Track_Date = format(as.Date(as.character(x$date), "%Y%m%d"), "%d/%m/%Y"),
    Track_Time = Time_String_To_Seconds(format(as.POSIXct("1970-01-01 00:00:00", tz = "UTC") + as.numeric(x$time), "%H:%M:%S")),
    Callsign = x$CALLSIGN,
    SSR_Code = x$SSR,
    X_Pos = x$x,
    Y_Pos = x$y,
    Lat = x$PositionLatitude,
    Lon = x$PositionLongitude,
    Mode_C = as.numeric(x$modec) * 100 * FeetToMetres, 
    Track_SPD = as.numeric(x$tspeed) * KnotsToMetresPerSecond,
    Track_HDG = as.numeric(x$theading) * DegreesToRadians,
    Track_Number = NA,
    Mode_S_Address = x$adres,
    Mode_S_GSPD = as.numeric(x$mdsgs) * KnotsToMetresPerSecond,
    Mode_S_IAS = as.numeric(x$mdsias) * KnotsToMetresPerSecond,
    Mode_S_HDG = as.numeric(x$mdsmah) * DegreesToRadians,
    Mode_S_TAS = as.numeric(x$mdstas) * KnotsToMetresPerSecond,
    Mode_S_Track_HDG = as.numeric(x$mdstta) * DegreesToRadians,
    Mode_S_Track_HDG_Rate = NA,
    Mode_S_Roll_Angle = as.numeric(x$mdsra) * DegreesToRadians,
    Mode_S_BPS = NA
  )
  
  message("[",Sys.time(),"] ", "Generating Flight_Plan_ID (this may take a while)...")
  
  fp <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Flight_Plan"))
  for (j in unique(fp[FP_Date %in% unique(out$Track_Date)]$Flight_Plan_ID)) {
    out[
      Track_Date == fp[Flight_Plan_ID == j]$FP_Date & 
        abs(Track_Time - fp[Flight_Plan_ID == j]$FP_Time) < 7200 &
        Callsign == fp[Flight_Plan_ID == j]$Callsign &
        grepl(paste0("^[0]?", fp[Flight_Plan_ID == j]$SSR_Code, "$"), SSR_Code)
    ]$Flight_Plan_ID <- j
  }
  
  message("[",Sys.time(),"] ", "Appending ", nrow(out), " rows to tbl_Radar_Track_Point...")
  dbWriteTable(dbi_con, "tbl_Radar_Track_Point", out, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(out), " rows to tbl_Radar_Track_Point")
  
}

process_LVNL_FP <- function(LogFilePath, dbi_con) {
  
  message("[",Sys.time(),"] ", "Begin processing LVNL flight plan file...")
  x <- fread(LogFilePath, header = F, skip = 1, na.strings = c("NA", "N/A", "NULL", ""))
  message("[",Sys.time(),"] ", "Read ", nrow(x), " lines.")
  
  names(x)[1:9] <- c(
    "DATE_ATA",
    "TIME_ATA",
    "CALLSIGN",
    "SSR",
    "ICAO_ACTYPE",
    "WTC",
    "ORIGIN",
    "DESTINATION",
    "RUNWAY"
  )
  
  out <- data.table(
    FP_Date = format(as.Date(x$DATE_ATA, "%d-%m-%Y"), "%d/%m/%Y"),
    FP_Time = as.numeric(Time_String_To_Seconds(x$TIME_ATA)),
    Callsign = x$CALLSIGN,
    Aircraft_Type = x$ICAO_ACTYPE,
    SSR_Code = x$SSR,
    Wake_Vortex = x$WTC,
    Destination = x$DESTINATION,
    Landing_Runway = ifelse(grepl("^[0-9]{1}$", x$RUNWAY), paste0("R0", x$RUNWAY), ifelse(grepl("^[0-9]{2}$", x$RUNWAY), paste0("R", x$RUNWAY), NA)),
    STAR = NA,
    Origin = x$ORIGIN,
    Departure_Runway = NA,
    SID = NA
  )
  
  out <- out[!is.na(FP_Date) & !is.na(FP_Time) & !is.na(Callsign) & !is.na(SSR_Code)]
  
  message("[",Sys.time(),"] ", "Checking for duplicates within loaded data...")
  
  out_undup <- out[!duplicated(out[,.(FP_Date, Callsign)])]
  out_dup <- out[duplicated(out[,.(FP_Date, Callsign)])]
  
  # Remove some duplicate flight plans
  out_dup_proc <- rbindlist(lapply(unique(paste(out_dup$FP_Date, out_dup$Callsign)), function(j) {
    out_j <- out_dup[paste(FP_Date, Callsign) == j]
    return(out_j[FP_Time == max(out_j$FP_Time, na.rm = T)])
  }))
  
  out_pass_1 <- rbind(out_undup, out_dup_proc)
  
  # Flight plan check for more duplicates
  fp <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Flight_Plan"))
  
  if (nrow(fp) > 0) {
    message("[",Sys.time(),"] ", "Checking for duplicates with existing data...")
    out_pass_2 <- rbindlist(lapply(1:nrow(out_pass_1), function(j) {
      fp_j <- fp[
        FP_Date == out_pass_1$FP_Date[j] &
          Callsign == out_pass_1$Callsign[j] &
          Destination == out_pass_1$Destination[j]
      ]
      if (nrow(fp_j) == 0) {
        return(out_pass_1[j])
      }
    }), fill = T, use.names = T)
    rm(fp)
  } else {
    out_pass_2 <- out_pass_1
  }
  rm(out_pass_1)
  
  message("[",Sys.time(),"] ", "Appending ", nrow(out_pass_2), " rows to tbl_Flight_Plan...")
  
  dbWriteTable(dbi_con, "tbl_Flight_Plan", out_pass_2, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(out_pass_2), " rows to tbl_Flight_Plan")
  
}

process_LVNL_QNH <- function(LogFilePath, Airfield_Name, dbi_con) {
  
  message("[",Sys.time(),"] ", "Begin processing LVNL QNH log file...")
  x <- fread(LogFilePath, header = F, skip = 1, na.strings = c("NA", "N/A", "NULL", ""))
  message("[",Sys.time(),"] ", "Read ", nrow(x), " lines.")
  
  names(x)[1:3] <- c(
    "DATE_QNH",
    "TIME_QNH",
    "QNH"
  )
  
  out <- data.table(
    Airfield = Airfield_Name,
    Baro_Date = format(as.Date(x$DATE_QNH, "%d-%m-%Y"), "%d/%m/%Y"),
    Baro_Time = as.numeric(Time_String_To_Seconds(x$TIME_QNH)),
    Baro_Pressure = as.numeric(x$QNH) * MbarToPa
  )
  
  out <- out[!is.na(Airfield) & !is.na(Baro_Date) & !is.na(Baro_Time) & !is.na(Baro_Pressure)]
  
  message("[",Sys.time(),"] ", "Appending ", nrow(out), " rows to tbl_Baro...")
  dbWriteTable(dbi_con, "tbl_Baro", out, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(out), " rows to tbl_Baro")
  
}

process_LVNL_SurfaceWind <- function(LogFilePath, Airfield_Name, dbi_con) {
  
  message("[",Sys.time(),"] ", "Begin processing LVNL Surface Wind log file...")
  x <- fread(LogFilePath, header = F, skip = 1, na.strings = c("NA", "N/A", "NULL", ""))
  message("[",Sys.time(),"] ", "Read ", nrow(x), " lines.")
  
  names(x)[1:5] <- c(
    "DATE_WIND",
    "TIME_WIND",
    "RUNWAY",
    "WIND_SPEED",
    "WIND_DIR"
  )
  
  out <- data.table(
    Airfield = Airfield_Name,
    Landing_Runway = ifelse(grepl("^[0-9]{1}$", x$RUNWAY), paste0("R0", x$RUNWAY), ifelse(grepl("^[0-9]{2}$", x$RUNWAY), paste0("R", x$RUNWAY), NA)),
    Anemo_Date = format(as.Date(x$DATE_WIND, "%d-%m-%Y"), "%d/%m/%Y"),
    Anemo_Time = as.numeric(Time_String_To_Seconds(x$TIME_WIND)),
    Anemo_SPD = as.numeric(x$WIND_SPEED) * KnotsToMetresPerSecond,
    Anemo_HDG = as.numeric(x$WIND_DIR) * DegreesToRadians,
    Anemo_HW = NA,
    Anemo_CW = NA
  )
  
  out <- out[
    !is.na(Airfield) &
      !is.na(Landing_Runway) &
      !is.na(Anemo_Date) &
      !is.na(Anemo_Time) &
      !is.na(Anemo_SPD) &
      !is.na(Anemo_HDG)
  ]
  
  message("[",Sys.time(),"] ", "Appending ", nrow(out), " rows to tbl_Anemometer...")
  dbWriteTable(dbi_con, "tbl_Anemometer", out, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(out), " rows to tbl_Anemometer")
  
}
