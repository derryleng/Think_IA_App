Asterix_Filename_To_Date <- function(Log_Filename) {
  # This is a very specific function for filename strings containing yyyymmdd
  # If there are other blocks of >=8 numbers in filename this may get confused!
  return(format(as.Date(gsub("^.*([2]{1}[0]{1}[0-9]{2}[0-1]{1}[0-9]{1}[0-3]{1}[0-9]{1}).*$", "\\1", basename(Log_Filename)), format = "%Y%m%d"), "%d/%m/%Y"))
}

process_Asterix_Cat48 <- function(LogFilePath, tbl_Adaptation_Data, tbl_Runway, dbi_con) {
  
  Date_String <- Asterix_Filename_To_Date(basename(LogFilePath))
  message("[",Sys.time(),"] ", "Found date from filename: ", Date_String)
  
  message("[",Sys.time(),"] ", "Begin processing CAT48 log file...")
  logs <- fread(LogFilePath, header = F, skip = 1, na.strings = c("NA", "N/A", "NULL", ""))
  message("[",Sys.time(),"] ", "Read ", nrow(logs), " lines.")
  
  names(logs)[1:27] <- c(
    "SAC",
    "SIC",
    "Derived_X_nm",
    "Derived_Y_nm",
    "Time_hms",
    "Mode_A",
    "Mode_C_ft",
    "Mode_C_Not_Validated",
    "Mode_C_Garbled",
    "Mode_S_Address",
    "Aircraft_ID",
    "Track_Number",
    "Calculated_X_Position",
    "Calculated_Y_Position",
    "Calc_Groundspeed_kt",
    "Calc_Heading_deg",
    "Tentative_Track",
    "Type_of_Sensor_s_Maintaining_Track",
    "Low_Plot_To_Track_Association_Confidence",
    "Roll_Angle_deg",
    "True_Track_Angle_deg",
    "Ground_Speed_kt",
    "Track_Angle_Rate_deg/s",
    "True_Airspeed_kt",
    "Magnetic_Heading_deg",
    "Indicated_Airspeed_kt",
    "Barometric_Pressure_Setting_mbar"
  )
  
  x <- logs[!is.na(Mode_C_ft) & paste0(SIC, SAC) == "5249" & !is.na(Mode_A) & !is.na(Aircraft_ID)]
  
  x <- x[as.numeric(Mode_C_ft) <= 10000 & !grepl("^7777$", Mode_A) & !grepl("^7000$", Mode_A)]
  
  x$Derived_X_nm <- as.numeric(x$Derived_X_nm) * NmToMetres
  x$Derived_Y_nm <- as.numeric(x$Derived_Y_nm) * NmToMetres
  
  x <- x[
    Derived_X_nm >= mean(tbl_Runway$Threshold_X_Pos) - tbl_Adaptation_Data$Load_X_Range &
      Derived_X_nm <= mean(tbl_Runway$Threshold_X_Pos) + tbl_Adaptation_Data$Load_X_Range &
      Derived_Y_nm >= mean(tbl_Runway$Threshold_Y_Pos) - tbl_Adaptation_Data$Load_Y_Range &
      Derived_Y_nm <= mean(tbl_Runway$Threshold_Y_Pos) + tbl_Adaptation_Data$Load_Y_Range
  ]
  
  if (nrow(x) > 0) {
    
    x <- cbind(x, Latlong_From_XY(as.numeric(x$Derived_X_nm) * NmToMetres, as.numeric(x$Derived_Y_nm) * NmToMetres, tbl_Adaptation_Data))
    
    x[Magnetic_Heading_deg < 0]$Magnetic_Heading_deg <- as.numeric(x[Magnetic_Heading_deg < 0]$Magnetic_Heading_deg) + 360
    x[True_Track_Angle_deg < 0]$True_Track_Angle_deg <- as.numeric(x[True_Track_Angle_deg < 0]$True_Track_Angle_deg) + 360
    
  }
  
  out <- data.table(
    Flight_Plan_ID = integer(),
    Track_Date = Date_String,
    Track_Time = as.numeric(Time_String_To_Seconds(x$Time_hms)),
    Callsign = x$Aircraft_ID,
    SSR_Code = x$Mode_A,
    X_Pos = x$Derived_X_nm,
    Y_Pos = x$Derived_Y_nm,
    Lat = x$PositionLatitude,
    Lon = x$PositionLongitude,
    Mode_C = as.numeric(x$Mode_C_ft) * FeetToMetres,
    Track_SPD = as.numeric(x$Calc_Groundspeed_kt) * KnotsToMetresPerSecond,
    Track_HDG = as.numeric(x$Calc_Heading_deg) * DegreesToRadians,
    Track_Number = as.integer(x$Track_Number),
    Mode_S_Address = x$Mode_S_Address,
    Mode_S_GSPD = as.numeric(x$Ground_Speed_kt) * KnotsToMetresPerSecond,
    Mode_S_IAS = as.numeric(x$Indicated_Airspeed_kt) * KnotsToMetresPerSecond,
    Mode_S_HDG = as.numeric(x$Magnetic_Heading_deg) * DegreesToRadians,
    Mode_S_TAS = as.numeric(x$True_Airspeed_kt) * KnotsToMetresPerSecond,
    Mode_S_Track_HDG = as.numeric(x$True_Track_Angle_deg) * DegreesToRadians,
    Mode_S_Track_HDG_Rate = as.numeric(x$Track_Angle_Rate_degps) * DegreesToRadians,
    Mode_S_Roll_Angle = as.numeric(x$Roll_Angle_deg) * DegreesToRadians,
    Mode_S_BPS = as.numeric(x$Barometric_Pressure_Setting_mbar) * MbarToPa
  )
  
  fp <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Flight_Plan"))
  for (j in unique(fp$Flight_Plan_ID)) {
    out[
      Track_Date == fp[Flight_Plan_ID == j]$FP_Date & 
        abs(Track_Time - fp[Flight_Plan_ID == j]$FP_Time) < 7200 &
        Callsign == fp[Flight_Plan_ID == j]$Callsign &
        grepl(paste0("^[0]?", fp[Flight_Plan_ID == j]$SSR_Code, "$"), SSR_Code)
    ]$Flight_Plan_ID <- j
  }
  
  dbWriteTable(dbi_con, "tbl_Radar_Track_Point", out, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(out), " rows to tbl_Radar_Track_Point")
  
}

process_Asterix_Cat62 <- function(LogFilePath, tbl_Adaptation_Data, tbl_Runway, dbi_con) {
  
  Date_String <- Asterix_Filename_To_Date(basename(LogFilePath))
  message("[",Sys.time(),"] ", "Found date from filename: ", Date_String)
  
  message("[",Sys.time(),"] ", "Begin processing CAT62 log file...")
  logs <- fread(LogFilePath, header = F, skip = 2, na.strings = c("NA", "N/A", "NULL", ""))
  message("[",Sys.time(),"] ", "Read ", nrow(logs), " lines.")
  
  names(logs)[1:37] <- c(
    "062",
    "I062/010/Sac",
    "I062/010/Sic",
    "I062/040/TrackNum",
    "I062/060/Mode3A",
    "I062/070/Time",
    "I062/100/X",
    "I062/100/Y",
    "I062/105/Lat",
    "I062/105/Lon",
    "I062/135/QNH",
    "I062/135/CTL",
    "I062/185/VX",
    "I062/185/VY",
    "I062/295/MDA/MDA",
    "I062/295/MHG/MHG",
    "I062/295/TAS/TAS",
    "I062/295/BVR/BVR",
    "I062/295/RAN/RAN",
    "I062/295/TAR/TAR",
    "I062/295/TAN/TAN",
    "I062/295/GSP/GSP",
    "I062/295/IAR/IAR",
    "I062/295/BPS/BPS",
    "I062/380/ADR/ADR",
    "I062/380/ID/ID",
    "I062/380/MHG/MHG",
    "I062/380/TAS/TAS",
    "I062/380/BVR/BVR",
    "I062/380/RAN/RAN",
    "I062/380/TAR/RateOfTurn",
    "I062/380/TAN/TAN",
    "I062/380/GSP/GSP",
    "I062/380/IAR/IAR",
    "I062/380/BPS/BPS",
    "I062/390/CSN/CSN",
    "I062/SP/CCR/RC"
  )
  
  x <- logs[`062` == "62" & !is.na(`I062/135/CTL`) & paste0(`I062/010/Sac`, `I062/010/Sic`) == "52254" & !is.na(`I062/390/CSN/CSN`)]
  
  x <- x[as.numeric(`I062/135/CTL`) < 100 & !grepl("^7777$", `I062/060/Mode3A`) & !grepl("^7000$", `I062/060/Mode3A`)]
  
  x <- x[
    `I062/100/X` >= mean(tbl_Runway$Threshold_X_Pos) - tbl_Adaptation_Data$Load_X_Range &
      `I062/100/X` <= mean(tbl_Runway$Threshold_X_Pos) + tbl_Adaptation_Data$Load_X_Range &
      `I062/100/Y` >= mean(tbl_Runway$Threshold_Y_Pos) - tbl_Adaptation_Data$Load_Y_Range &
      `I062/100/Y` <= mean(tbl_Runway$Threshold_Y_Pos) + tbl_Adaptation_Data$Load_Y_Range
  ]
  
  if (nrow(x) > 0) {
    
    if (tbl_Adaptation_Data$Use_Local_Coords) {
      x <- cbind(x, Latlong_To_XY(as.numeric(x$`I062/105/Lat`) * DegreesToRadians, as.numeric(x$`I062/105/Lon`) * DegreesToRadians, tbl_Adaptation_Data))
    } else {
      x <- cbind(x, Latlong_From_XY(as.numeric(x$`I062/100/X`), as.numeric(x$`I062/100/Y`), tbl_Adaptation_Data))
    }
    
    x$`I062/380/GSP/GSP` <- ifelse(as.numeric(x$`I062/295/GSP/GSP`) > tbl_Adaptation_Data$Max_Mode_S_Data_Age, NA, x$`I062/380/GSP/GSP`)
    x$`I062/380/IAR/IAR` <- ifelse(as.numeric(x$`I062/295/IAR/IAR`) > tbl_Adaptation_Data$Max_Mode_S_Data_Age, NA, x$`I062/380/IAR/IAR`)
    x$`I062/380/MHG/MHG` <- ifelse(as.numeric(x$`I062/295/MHG/MHG`) > tbl_Adaptation_Data$Max_Mode_S_Data_Age, NA, x$`I062/380/MHG/MHG`)
    x$`I062/380/TAS/TAS` <- ifelse(as.numeric(x$`I062/295/TAS/TAS`) > tbl_Adaptation_Data$Max_Mode_S_Data_Age, NA, x$`I062/380/TAS/TAS`)
    x$`I062/380/TAN/TAN` <- ifelse(as.numeric(x$`I062/295/TAN/TAN`) > tbl_Adaptation_Data$Max_Mode_S_Data_Age, NA, x$`I062/380/TAN/TAN`)
    x$`I062/380/TAR/RateOfTurn` <- ifelse(as.numeric(x$`I062/295/TAR/TAR`) > tbl_Adaptation_Data$Max_Mode_S_Data_Age, NA, x$`I062/380/TAR/RateOfTurn`)
    x$`I062/380/RAN/RAN` <- ifelse(as.numeric(x$`I062/295/RAN/RAN`) > tbl_Adaptation_Data$Max_Mode_S_Data_Age, NA, x$`I062/380/RAN/RAN`)
    x$`I062/380/BPS/BPS` <- ifelse(as.numeric(x$`I062/295/BPS/BPS`) > tbl_Adaptation_Data$Max_Mode_S_Data_Age, NA, x$`I062/380/BPS/BPS`)
    
  }
  
  out <- data.table(
    Flight_Plan_ID = NA,
    Track_Date = Date_String,
    Track_Time = as.numeric(x$`I062/070/Time`),
    Callsign = x$`I062/390/CSN/CSN`,
    SSR_Code = x$`I062/060/Mode3A`,
    X_Pos = ifelse(tbl_Adaptation_Data$Use_Local_Coords, x$Position_X, x$`I062/100/X`),
    Y_Pos = ifelse(tbl_Adaptation_Data$Use_Local_Coords, x$Position_Y, x$`I062/100/Y`),
    Lat = ifelse(tbl_Adaptation_Data$Use_Local_Coords, x$`I062/105/Lat` * DegreesToRadians, x$PositionLatitude),
    Lon = ifelse(tbl_Adaptation_Data$Use_Local_Coords, x$`I062/105/Lon` * DegreesToRadians, x$PositionLongitude),
    Mode_C = as.numeric(x$`I062/135/CTL`) * 100 * FeetToMetres,
    Track_SPD = sqrt(as.numeric(x$`I062/185/VX`)^2 + as.numeric(x$`I062/185/VY`)^2),
    Track_HDG = To_Vector_Angle(as.numeric(x$`I062/185/VX`), as.numeric(x$`I062/185/VY`)),
    Track_Number = as.integer(x$`I062/040/TrackNum`),
    Mode_S_Address = x$`I062/380/ADR/ADR`,
    Mode_S_GSPD = as.numeric(x$`I062/380/GSP/GSP`) * KnotsToMetresPerSecond,
    Mode_S_IAS = as.numeric(x$`I062/380/IAR/IAR`) * KnotsToMetresPerSecond,
    Mode_S_HDG = as.numeric(x$`I062/380/MHG/MHG`) * DegreesToRadians,
    Mode_S_TAS = as.numeric(x$`I062/380/TAS/TAS`) * KnotsToMetresPerSecond,
    Mode_S_Track_HDG = as.numeric(x$`I062/380/TAN/TAN`) * DegreesToRadians,
    Mode_S_Track_HDG_Rate = as.numeric(x$`I062/380/TAR/RateOfTurn`) * DegreesToRadians,
    Mode_S_Roll_Angle = as.numeric(x$`I062/380/RAN/RAN`) * DegreesToRadians,
    Mode_S_BPS = (as.numeric(x$`I062/380/BPS/BPS`) + 800) * MbarToPa
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
  
  dbWriteTable(dbi_con, "tbl_Radar_Track_Point", out, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(out), " rows to tbl_Radar_Track_Point")
  
}

process_Asterix_Cat20 <- function(LogFilePath, tbl_Adaptation_Data, tbl_Runway, dbi_con) {
  
  Date_String <- Asterix_Filename_To_Date(basename(LogFilePath))
  message("[",Sys.time(),"] ", "Found date from filename: ", Date_String)
  
  message("[",Sys.time(),"] ", "Begin processing CAT20 log file...")
  logs <- fread(LogFilePath, header = F, skip = 6, na.strings = c("NA", "N/A", "NULL", ""))
  message("[",Sys.time(),"] ", "Read ", nrow(logs), " lines.")
  
  names(logs)[1:50] <- c(
    "I020/010/Sac",
    "I020/010/Sic",
    "I020/020/Data",
    "I020/041/Lat",
    "I020/041/Lon",
    "I020/042/X",
    "I020/042/Y",
    "I020/070/L",
    "I020/070/G",
    "I020/070/V",
    "I020/070/Mode3A",
    "I020/090/MFL",
    "I020/110/CartHgt",
    "I020/140/Time",
    "I020/161/TrkNum",
    "I020/170/Data",
    "I020/202/VX",
    "I020/202/VY",
    "I020/210/AX",
    "I020/210/AY",
    "I020/220/ADR",
    "I020/230/Data",
    "I020/245/ID",
    "I020/245/STI",
    "I020/250/Bds4_0/Bps",
    "I020/250/Bds4_0/BpsStatus",
    "I020/250/Bds4_0/FcuAlt",
    "I020/250/Bds4_0/FcuAltStatus",
    "I020/250/Bds4_0/FmsAlt",
    "I020/250/Bds4_0/FmsAltStatus",
    "I020/250/Bds5_0/Gsp",
    "I020/250/Bds5_0/GspStatus",
    "I020/250/Bds5_0/Ran",
    "I020/250/Bds5_0/RanStatus",
    "I020/250/Bds5_0/Tan",
    "I020/250/Bds5_0/TanRate",
    "I020/250/Bds5_0/TanRateStatus",
    "I020/250/Bds5_0/TanStatus",
    "I020/250/Bds5_0/Tas",
    "I020/250/Bds5_0/TasStatus",
    "I020/250/Bds6_0/BaroRate",
    "I020/250/Bds6_0/BaroRateStatus",
    "I020/250/Bds6_0/Ias",
    "I020/250/Bds6_0/IasStatus",
    "I020/250/Bds6_0/Ivv",
    "I020/250/Bds6_0/IvvStatus",
    "I020/250/Bds6_0/Mach",
    "I020/250/Bds6_0/MachStatus",
    "I020/250/Bds6_0/Mgh",
    "I020/250/Bds6_0/MghStatus"
  )
  
  x <- logs[`I020/010/Sac` == "52" & !is.na(`I020/090/MFL`) & paste0(`I020/010/Sac`, `I020/010/Sic`) == "52137" & !is.na(`I020/245/ID`)]
  
  x <- x[as.numeric(`I020/090/MFL`) < 100 & !grepl("^7777$", `I020/070/Mode3A`) & !grepl("^7000$", `I020/070/Mode3A`)]
  
  x <- x[
    `I020/042/X` >= mean(tbl_Runway$Threshold_X_Pos) - tbl_Adaptation_Data$Load_X_Range &
      `I020/042/X` <= mean(tbl_Runway$Threshold_X_Pos) + tbl_Adaptation_Data$Load_X_Range &
      `I020/042/Y` >= mean(tbl_Runway$Threshold_Y_Pos) - tbl_Adaptation_Data$Load_Y_Range &
      `I020/042/Y` <= mean(tbl_Runway$Threshold_Y_Pos) + tbl_Adaptation_Data$Load_Y_Range
  ]
  
  if (nrow(x) > 0) {
    
    if (tbl_Adaptation_Data$Use_Local_Coords) {
      x <- cbind(x, Latlong_To_XY(as.numeric(x$`I020/041/Lat`) * DegreesToRadians, as.numeric(x$`I020/041/Lon`) * DegreesToRadians, tbl_Adaptation_Data))
    } else {
      x <- cbind(x, Latlong_From_XY(as.numeric(x$`I020/042/X`), as.numeric(x$`I020/042/Y`), tbl_Adaptation_Data))
    }
    
    x[`I020/250/Bds6_0/Mgh` < 0]$`I020/250/Bds6_0/Mgh` <- as.numeric(x[`I020/250/Bds6_0/Mgh` < 0]$`I020/250/Bds6_0/Mgh`) + 360
    x[`I020/250/Bds5_0/Tan` < 0]$`I020/250/Bds5_0/Tan` <- as.numeric(x[`I020/250/Bds5_0/Tan` < 0]$`I020/250/Bds5_0/Tan`) + 360
    x$`I020/250/Bds5_0/Gsp` <- ifelse(as.numeric(x$`I020/250/Bds5_0/GspStatus`) != 1, NA, x$`I020/250/Bds5_0/Gsp`)
    x$`I020/250/Bds6_0/Ias` <- ifelse(as.numeric(x$`I020/250/Bds6_0/IasStatus`) != 1, NA, x$`I020/250/Bds6_0/Ias`)
    x$`I020/250/Bds6_0/Mgh` <- ifelse(as.numeric(x$`I020/250/Bds6_0/MghStatus`) != 1, NA, x$`I020/250/Bds6_0/Mgh`)
    x$`I020/250/Bds5_0/Tas` <- ifelse(as.numeric(x$`I020/250/Bds5_0/TasStatus`) != 1, NA, x$`I020/250/Bds5_0/Tas`)
    x$`I020/250/Bds5_0/Tan` <- ifelse(as.numeric(x$`I020/250/Bds5_0/TanStatus`) != 1, NA, x$`I020/250/Bds5_0/Tan`)
    x$`I020/250/Bds5_0/TanRate` <- ifelse(as.numeric(x$`I020/250/Bds5_0/TanRateStatus`) != 1, NA, x$`I020/250/Bds5_0/TanRate`)
    x$`I020/250/Bds5_0/Ran` <- ifelse(as.numeric(x$`I020/250/Bds5_0/RanStatus`) != 1, NA, x$`I020/250/Bds5_0/Ran`)
    x$`I020/250/Bds4_0/Bps` <- ifelse(as.numeric(x$`I020/250/Bds4_0/BpsStatus`) != 1, NA, x$`I020/250/Bds4_0/Bps`)
    
  }
  
  out <- data.table(
    Flight_Plan_ID = integer(),
    Track_Date = Date_String,
    Track_Time = as.numeric(x$`I020/140/Time`),
    Callsign = x$`I020/245/ID`,
    SSR_Code = x$`I020/070/Mode3A`,
    X_Pos = felse(tbl_Adaptation_Data$Use_Local_Coords, x$Position_X, x$`I020/042/X`),
    Y_Pos = ifelse(tbl_Adaptation_Data$Use_Local_Coords, x$Position_Y, x$`I020/042/Y`),
    Lat = ifelse(tbl_Adaptation_Data$Use_Local_Coords, x$`I020/041/Lat` * DegreesToRadians, x$PositionLatitude),
    Lon = ifelse(tbl_Adaptation_Data$Use_Local_Coords, x$`I020/041/Lon` * DegreesToRadians, x$PositionLongitude),
    Mode_C = as.numeric(x$`I020/090/MFL`) * 100 * FeetToMetres,
    Track_SPD = sqrt(as.numeric(x$`I020/202/VX`)^2 + as.numeric(x$`I020/202/VY`)^2),
    Track_HDG = To_Vector_Angle(as.numeric(x$`I020/202/VX`), as.numeric(x$`I020/202/VY`)),
    Track_Number = as.integer(x$`I020/161/TrkNum`),
    Mode_S_Address = x$`I020/220/ADR`,
    Mode_S_GSPD = as.numeric(x$`I020/250/Bds5_0/Gsp`) * KnotsToMetresPerSecond,
    Mode_S_IAS = as.numeric(x$`I020/250/Bds6_0/Ias`) * KnotsToMetresPerSecond,
    Mode_S_HDG = as.numeric(x$`I020/250/Bds6_0/Mgh`) * DegreesToRadians,
    Mode_S_TAS = as.numeric(x$`I020/250/Bds5_0/Tas`) * KnotsToMetresPerSecond,
    Mode_S_Track_HDG = as.numeric(x$`I020/250/Bds5_0/Tan`) * DegreesToRadians,
    Mode_S_Track_HDG_Rate = as.numeric(x$`I020/250/Bds5_0/TanRate`) * DegreesToRadians,
    Mode_S_Roll_Angle = as.numeric(x$`I020/250/Bds5_0/Ran`) * DegreesToRadians,
    Mode_S_BPS = (as.numeric(x$`I020/250/Bds4_0/Bps`) + 800) * MbarToPa
  )
  
  fp <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Flight_Plan"))
  for (j in unique(fp$Flight_Plan_ID)) {
    out[
      Track_Date == fp[Flight_Plan_ID == j]$FP_Date & 
        abs(Track_Time - fp[Flight_Plan_ID == j]$FP_Time) < 7200 &
        Callsign == fp[Flight_Plan_ID == j]$Callsign &
        grepl(paste0("^[0]?", fp[Flight_Plan_ID == j]$SSR_Code, "$"), SSR_Code)
    ]$Flight_Plan_ID <- j
  }
  
  dbWriteTable(dbi_con, "tbl_Radar_Track_Point", out, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(out), " rows to tbl_Radar_Track_Point")
  
}
