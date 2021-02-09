process_eTBS_logs_9002 <- function(LogFile) {
  
  logs <- rbindlist(lapply(grep("^9002, .*$", LogFile), function(i) {
    
    Log_Date <- as.character(as.Date(gsub("^([0-9]{2}-[0-9]{2}-[0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3})  Message: [0-9]{4}$", "\\1", LogFile[i-1]), "%y-%m-%d"))
    Log_Time <- Time_String_To_Seconds(gsub("^([0-9]{2}-[0-9]{2}-[0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3})  Message: [0-9]{4}$", "\\2", LogFile[i-1]))
    
    LogContents <- data.table(t(c(Log_Date, Log_Time, unlist(strsplit(LogFile[i], split = "\\s{0,},\\s{0,}")))))
    
    # Ref Leidos TBS20170004-TBS logging - v1.2
    names(LogContents) <- c(
      "Date",
      "Time",
      "Message_Type",
      "Message_Length",
      "SSR_Code",
      "AC_Wake_Cat",
      "Arr_APT",
      "Seq_Num",
      "Aircraft_Type",
      "Callsign",
      "Arrival_Runway",
      "Min_Spacing_Before",
      "Min_Spacing_After",
      "RNAV_Approach",
      "Desequenced_Flight",
      "Deseq_Code",
      "TGT_Landing_Time"
    )[1:length(names(LogContents))]
    
    return(LogContents)
    
  }), use.names = T)
  
  x <- logs[!grepl("^0$", SSR_Code)]
  
  # Should make sure no duplicates here!
  
  x$AC_Wake_Cat <- ifelse(
    x$AC_Wake_Cat == "0", "J", ifelse(
      x$AC_Wake_Cat == "1", "H", ifelse(
        x$AC_Wake_Cat == "2", "UM", ifelse(
          x$AC_Wake_Cat == "3", "LM", ifelse(
            x$AC_Wake_Cat == "4", "S", ifelse(
              x$AC_Wake_Cat == "5", "L", NA
            )
          )
        )
      )
    )
  )
  
  return(data.table(
    FP_Date = format(as.Date(x$Date), "%d/%m/%y"),
    FP_Time = as.numeric(x$Time),
    Callsign = x$Callsign,
    Aircraft_Type = x$Aircraft_Type,
    SSR_Code = x$SSR_Code,
    Wake_Vortex = x$AC_Wake_Cat,
    Destination = x$Arr_APT,
    Landing_Runway = paste0("R", x$Arrival_Runway)
  ))
  
}

process_eTBS_logs_9005 <- function(LogFile, tbl_Adaptation_Data, tbl_Runway) {
  
  logs <- rbindlist(lapply(grep("^9005, .*$", LogFile), function(i) {
    
    Log_Date <- as.character(as.Date(gsub("^([0-9]{2}-[0-9]{2}-[0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3})  Message: [0-9]{4}$", "\\1", LogFile[i-1]), "%y-%m-%d"))
    Log_Time <- Time_String_To_Seconds(gsub("^([0-9]{2}-[0-9]{2}-[0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3})  Message: [0-9]{4}$", "\\2", LogFile[i-1]))
    
    LogContents <- data.table(t(c(Log_Date, Log_Time, unlist(strsplit(LogFile[i], split = "\\s{0,},\\s{0,}")))))
    
    # Ref Leidos Time Based Separation Logging Definition QeTBSD-TE-4, Version 1.6
    names(LogContents) <- c(
      "Date",
      "Time",
      "Message_Type",
      "Message_Length",
      "Barometric_Pressure",
      "Ground_Speed",
      "Latitude",
      "Longitude",
      "Magnetic_Heading",
      "Rept_Sys_X",
      "Rept_Sys_Y",
      "Track_Angle",
      "Turn_Rate",
      "XDot",
      "YDot",
      "ZDot",
      "Alt",
      "Asterix_Latitude",
      "Asterix_Longitude",
      "Target_Address",
      "Track_Time",
      "System_Time",
      "Asterix_Mode3A",
      "Asterix_CTPA",
      "SSR_Code",
      "Track_ID",
      "Indicated_Air_Speed",
      "Asterix_SAC",
      "Asterix_SIC",
      "Asterix_SID",
      "Air_ID",
      "Callsign",
      "Intention",
      "Runway",
      "Roll_Angle",
      "True_Air_Speed",
      "Roll_Angle_Age",
      "Track_Angle_Age",
      "Ground_Speed_Age",
      "Magnetic_HDG_Age",
      "Indicated_ASP_Age",
      "Barometric_Age",
      "True_ASPD_Age"
    )[1:length(names(LogContents))]
    
    return(LogContents)
    
  }), use.names = T)
  
  x <- logs[!is.na(Alt) & !grepl("^-32767.*$", Alt)]
  
  x <- x[
    as.numeric(Alt) >= -1000 &
      as.numeric(Alt) <= 8000 &
      as.numeric(XDot) >= -8192 &
      as.numeric(XDot) <= 8192 &
      as.numeric(YDot) >= -8192 &
      as.numeric(YDot) <= 8192
  ]
  
  x <- x[
    Rept_Sys_X >= mean(tbl_Runway$Threshold_X_Pos) - tbl_Adaptation_Data$Load_X_Range &
      Rept_Sys_X <= mean(tbl_Runway$Threshold_X_Pos) + tbl_Adaptation_Data$Load_X_Range &
      Rept_Sys_Y >= mean(tbl_Runway$Threshold_Y_Pos) - tbl_Adaptation_Data$Load_Y_Range &
      Rept_Sys_Y <= mean(tbl_Runway$Threshold_Y_Pos) + tbl_Adaptation_Data$Load_Y_Range
  ]
  
  if (nrow(x) > 0) {
    
    x <- cbind(x, usp_GI_Latlong_From_XY(as.numeric(x$Rept_Sys_X), as.numeric(x$Rept_Sys_Y), tbl_Adaptation_Data))
    
    x$Ground_Speed <- ifelse(grepl("^-32767.*$", x$Ground_Speed) | as.numeric(x$Ground_Speed_Age) > tbl_Adaptation_Data$Max_Mode_S_Data_Age[1], NA, x$Ground_Speed)
    x$Indicated_Air_Speed <- ifelse(is.na(x$Indicated_Air_Speed) | grepl("^65535.*$", x$Indicated_Air_Speed) | as.numeric(x$Indicated_ASP_Age) > tbl_Adaptation_Data$Max_Mode_S_Data_Age[1], NA, x$Indicated_Air_Speed)
    x$Magnetic_Heading <- ifelse(is.na(x$Magnetic_Heading) | grepl("^-32767.*$", x$Magnetic_Heading) | as.numeric(x$Magnetic_HDG_Age) > tbl_Adaptation_Data$Max_Mode_S_Data_Age[1], NA, x$Magnetic_Heading)
    x$True_Air_Speed <- ifelse(is.na(x$True_Air_Speed) | grepl("^65535.*$", x$True_Air_Speed) | as.numeric(x$True_ASPD_Age) > tbl_Adaptation_Data$Max_Mode_S_Data_Age[1], NA, x$True_Air_Speed)
    x$Track_Angle <- ifelse(is.na(x$Track_Angle) | grepl("^-32767.*$", x$Track_Angle) | as.numeric(x$Track_Angle_Age) > tbl_Adaptation_Data$Max_Mode_S_Data_Age[1], NA, x$Track_Angle)
    x$Roll_Angle <- ifelse(is.na(x$Roll_Angle) | grepl("^-32767.*$", x$Roll_Angle) | as.numeric(x$Roll_Angle_Age) > tbl_Adaptation_Data$Max_Mode_S_Data_Age[1], NA, x$Roll_Angle)
    x$Barometric_Pressure <- ifelse(is.na(x$Barometric_Pressure) | grepl("^-32767.*$", x$Barometric_Pressure) | as.numeric(x$Barometric_Age) > tbl_Adaptation_Data$Max_Mode_S_Data_Age[1], NA, x$Barometric_Pressure)

  }
  
  out <- data.table(
    Flight_Plan_ID = integer(),
    Track_Date = as.character(format(as.Date(x$Date), "%d/%m/%y")),
    Track_Time = as.numeric(x$Time),
    Callsign = as.character(x$Callsign),
    SSR_Code = as.character(x$SSR_Code),
    X_Pos = as.numeric(x$Rept_Sys_X),
    Y_Pos = as.numeric(x$Rept_Sys_Y),
    Lat = as.numeric(x$PositionLatitude),
    Lon = as.numeric(x$PositionLongitude),
    Mode_C = as.numeric(x$Alt) * fnc_GI_Ft_To_M(),
    Track_SPD = sqrt(as.numeric(x$XDot)^2 + as.numeric(x$YDot)^2),
    Track_HDG = fnc_GI_To_Vector_Angle(as.numeric(x$XDot), as.numeric(x$YDot)),
    Track_Number = as.integer(x$Track_ID),
    Mode_S_Address = as.character(x$Target_Address),
    Mode_S_GSPD = as.numeric(x$Ground_Speed) * fnc_GI_Nm_To_M(),
    Mode_S_IAS = as.numeric(x$Indicated_Air_Speed) * fnc_GI_Kts_To_M_Per_Sec(),
    Mode_S_HDG = as.numeric(x$Magnetic_Heading) * fnc_GI_Degs_To_Rads(),
    Mode_S_TAS = as.numeric(x$True_Air_Speed) * fnc_GI_Kts_To_M_Per_Sec(),
    Mode_S_Track_HDG = as.numeric(x$Track_Angle) * fnc_GI_Degs_To_Rads(),
    Mode_S_Track_HDG_Rate = numeric(),
    Mode_S_Roll_Angle = as.numeric(x$Roll_Angle) * fnc_GI_Degs_To_Rads(),
    Mode_S_BPS = as.numeric(x$Barometric_Pressure) * fnc_GI_Mbar_To_Pa()
  )
  
  return(out)
  
}

process_eTBS_logs_9043 <- function(LogFile, Airfield_Name) {
  
  logs <- rbindlist(lapply(grep("^9043, .*$", LogFile), function(i) {
    
    Log_Date <- as.character(as.Date(gsub("^([0-9]{2}-[0-9]{2}-[0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3})  Message: [0-9]{4}$", "\\1", LogFile[i-1]), "%y-%m-%d"))
    
    LogContents <- data.table(t(c(Log_Date, unlist(strsplit(LogFile[i], split = "\\s{0,},\\s{0,}")))))
    
    # Ref Leidos TBS20170004-TBS logging - v1.2
    names(LogContents) <- c(
      "Date",
      "Message_Type",
      "Message_Length",
      "Runway_Group_Index",
      "State",
      "Num_Entries",
      "Derived_QNH",
      "Derived_QNH_Valid",
      "Derived_QNH_Track_ID",
      "Update_Time",
      "Derived_QNH_Time",
      "Forecast_Seg_Update",
      "Derived_QNH_Update",
      "Periodic_Update",
      paste0(c("Time",
               "Forecast_Time",
               "Wind_Effect_IAS",
               "Avg_Wind_SPD",
               "Avg_Wind_HDG",
               "Range_To_Threshold",
               "Min_Range_To_Threshold",
               "Max_Range_To_Threshold",
               "Extrapolation_Flag"), "_", rep(seq(1, 49, 1), each = 9))
    )[1:length(names(LogContents))]
    
    return(LogContents)
    
  }), use.names = T, fill = T)
  
  x <- logs[Derived_QNH_Update == 1]
  
  return(data.table(
    Airfield = Airfield_Name,
    Baro_Date = format(as.Date(x$Date), "%d/%m/%y"),
    Baro_Time = as.numeric(x$Derived_QNH_Time) / 1000,
    Baro_Pressure = as.numeric(x$Derived_QNH) * fnc_GI_Mbar_To_Pa()
  ))
  
}

process_eTBS_logs_9081 <- function(LogFile, Airfield_Name) {
  
  logs <- rbindlist(lapply(grep("^9081, .*$", LogFile), function(i) {
    
    Log_Date <- as.character(as.Date(gsub("^([0-9]{2}-[0-9]{2}-[0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3})  Message: [0-9]{4}$", "\\1", LogFile[i-1]), "%y-%m-%d"))
    Log_Time <- Time_String_To_Seconds(gsub("^([0-9]{2}-[0-9]{2}-[0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3})  Message: [0-9]{4}$", "\\2", LogFile[i-1]))
    
    LogContents <- data.table(t(c(Log_Date, Log_Time, unlist(strsplit(LogFile[i], split = "\\s{0,},\\s{0,}")))))
    
    # Ref Leidos Time Based Separation Logging Definition L5249 – eTBS Lite Phase 1, QeTBSD-TE-5, Ver 2.1
    
    names(LogContents)[1:18] <- c(
      "Date",
      "Time",
      "Message_Type",
      "Message_Length",
      "Lead_Track_ID",
      "Lead_Callsign",
      "Lead_SSR_Code",
      "Follower_Track_ID",
      "Follower_Callsign",
      "Follower_SSR_Code",
      "Runway",
      "ORD_Calc_Type",
      "Runway_Surface_Wind_Runway",
      "Runway_Surface_Wind_SPD",
      "Runway_Surface_Wind_HDG",
      "Runway_Surface_Headwind",
      "Runway_Surface_Wind_Source",
      "Num_Wind_Seg_Entries")
    
    names(LogContents)[19:(19 + 5 * as.numeric(LogContents$Num_Wind_Seg_Entries) - 1)] <- paste0(c("IDX",
                                                                                                   "Time",
                                                                                                   "Forecast_Time",
                                                                                                   "Wind_Effect_IAS",
                                                                                                   "Range_To_Threshold"), "_", rep(seq(1, as.numeric(LogContents$Num_Wind_Seg_Entries), 1), each = 5))
    
    names(LogContents)[(19 + 5 * as.numeric(LogContents$Num_Wind_Seg_Entries)):length(names(LogContents))] <- c(
      "Wind_Profile_Source",
      "TBS_Thresh_Sep",
      "Leader_Dist_To_Runway",
      "Leader_AC_Type",
      "Leader_AC_Type_Used",
      "Follower_Dist_To_Runway",
      "Follower_AC_Type",
      "Follower_AC_Type_Used",
      "Leader_WVC",
      "Follower_WVC",
      "Leader_VRef",
      "Leader_Velocity",
      "Leader_Velocity_Source",
      "Leader_Local_Stab_Thresh",
      "Leader_Landing_Stab_Speed_Type",
      "Follower_VRef",
      "Follower_Velocity",
      "Follower_Velocity_Source",
      "Follower_Local_Stab_Thresh",
      "Follower_Landing_Stab_Speed_Type",
      "Leader_Start_Initial_Deceleration",
      "Leader_Deceleration_Position",
      "Follower_Start_Initial_Deceleration",
      "Follower_Deceleration_Position",
      "ORD_Compression_Distance"
    )
    
    return(LogContents)
    
  }), use.names = T, fill = T)
  
  return(data.table(
    Airfield = Airfield_Name,
    Landing_Runway = paste0("R", logs$Runway),
    Anemo_Date = format(as.Date(logs$Date), "%d/%m/%y"),
    Anemo_Time = as.numeric(logs$Time),
    Anemo_SPD = as.numeric(logs$Runway_Surface_Wind_SPD) * fnc_GI_Kts_To_M_Per_Sec(),
    Anemo_HDG = as.numeric(logs$Runway_Surface_Wind_HDG) * fnc_GI_Degs_To_Rads(),
    Anemo_HW = as.numeric(logs$Runway_Surface_Headwind) * fnc_GI_Kts_To_M_Per_Sec(),
    Anemo_CW = NA
  ))
  
}

process_eTBS_logs <- function(LogFilePath, tbl_Adaptation_Data, tbl_Runway, Airfield_Name, dbi_con) {
  
  message("[",Sys.time(),"] ", "Begin processing TBS log file...")
  LogFile <- readLines(LogFilePath)
  message("[",Sys.time(),"] ", "Read ", length(LogFile), " lines.")
  
  ### Flight Plan
  message("[",Sys.time(),"] ", "Begin processing 9002 entries...")
  logs_9002 <- process_eTBS_logs_9002(LogFile)
  message("[",Sys.time(),"] ", "Finished processing 9002 entries (", nrow(logs_9002), " found), saving to tbl_Flight_Plan...")
  
  message("[",Sys.time(),"] ", "Checking for duplicates within loaded data...")
  logs_9002_pass_1 <- unique(logs_9002, by = c("FP_Date", "Callsign", "SSR_Code"))
  
  message("[",Sys.time(),"] ", "Checking for duplicates with existing data...")
  fp <- as.data.table(dbGetQuery(dbi_con, "SELECT DISTINCT FP_Date, Callsign, SSR_Code, Destination FROM tbl_Flight_Plan"))
  if (nrow(fp) > 0) {
    logs_9002_pass_2 <- logs_9002_pass_1[paste(FP_Date, Callsign, SSR_Code, Destination) %!in% paste(fp$FP_Date, fp$Callsign, fp$SSR_Code, fp$Destination)]
  } else {
    logs_9002_pass_2 <- logs_9002_pass_1
  }
  
  dbWriteTable(dbi_con, "tbl_Flight_Plan", logs_9002_pass_2, append = T)
  message("[",Sys.time(),"] ", "Successfully appended rows to tbl_Flight_Plan.")
  
  ### Radar tracks
  message("[",Sys.time(),"] ", "Begin processing 9005 entries...")
  logs_9005 <- process_eTBS_logs_9005(LogFile, tbl_Adaptation_Data, tbl_Runway)
  message("[",Sys.time(),"] ", "Finished processing 9005 entries (", nrow(logs_9005), " found), cross referencing FPIDs...")
  
  if (nrow(logs_9005) > 0) {
    # Flight plan ID cross reference
    message("[",Sys.time(),"] ", "Generating Flight_Plan_ID...")
    logs_9005 <- generateFPID(logs_9005, dbi_con)
    
    message("[",Sys.time(),"] ", "Finished cross referencing FPIDs, saving to tbl_Radar_Track_Point...")
    dbWriteTable(dbi_con, "tbl_Radar_Track_Point", logs_9005, append = T)
    message("[",Sys.time(),"] ", "Successfully appended rows to tbl_Radar_Track_Point.")
  }
  
  ### Barometer
  message("[",Sys.time(),"] ", "Begin processing 9043 entries...")
  logs_9043 <- process_eTBS_logs_9043(LogFile, Airfield_Name)
  message("[",Sys.time(),"] ", "Finished processing 9043 entries (", nrow(logs_9043), " found), saving to tbl_Baro...")
  dbWriteTable(dbi_con, "tbl_Baro", logs_9043, append = T)
  message("[",Sys.time(),"] ", "Successfully appended rows to tbl_Baro.")
  
  ### Anemometer
  message("[",Sys.time(),"] ", "Begin processing 9081 entries...")
  logs_9081 <- process_eTBS_logs_9081(LogFile, Airfield_Name)
  message("[",Sys.time(),"] ", "Finished processing 9081 entries (", nrow(logs_9081), " found), saving to tbl_Anemometer...")
  dbWriteTable(dbi_con, "tbl_Anemometer", logs_9081, append = T)
  message("[",Sys.time(),"] ", "Successfully appended rows to tbl_Anemometer.")
  
}
