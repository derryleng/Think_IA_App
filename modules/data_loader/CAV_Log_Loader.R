process_CAV_logs_9002 <- function(LogFile) {
  
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
      "Min_Space_Before",
      "Min_Space_After",
      "Target_Landing_Time",
      "SSR_Code",
      "Arrival_Airport",
      "Sequence_Number",
      "Callsign",
      "Arrival_Runway_Intent",
      "Desequenced_Code",
      "Desequenced_Flight",
      "Approach_Type"
    )[1:length(names(LogContents))]
    
    return(LogContents)
    
  }), use.names = T)
  
  x <- logs[!grepl("^0$", SSR_Code)]
  
  return(data.table(
    Log_Date = format(as.Date(x$Date), "%d/%m/%y"),
    Log_Time = as.numeric(x$Time),
    Callsign = x$Callsign,
    Approach_Type = ifelse(x$Approach_Type == "0", "ILS", ifelse(
      x$Approach_Type == "1", "MLS", ifelse(
        x$Approach_Type == "2", "RNAV", ifelse(
          x$Approach_Type == "3", "RNP", "Unknown"
        )
      )
    )),
    SSR_Code = x$SSR_Code,
    Destination = x$Arrival_Airport,
    Landing_Runway = paste0("R", x$Arrival_Runway_Intent)
  ))
  
}

process_CAV_logs_9005 <- function(LogFile, tbl_Adaptation_Data, tbl_Runway) {
  
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
      "True_ASPD_Age",
      "Is_Altitude_Corrected"
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
  
  # CAV
  if ("Is_Altitude_Corrected" %in% names(x)) {
    x$Is_Altitude_Corrected <- ifelse(x$Is_Altitude_Corrected == "1", T, ifelse(x$Is_Altitude_Corrected %in% c("", "255") | is.na(x$Is_Altitude_Corrected), NA, F))
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

process_CAV_logs_9043 <- function(LogFile) {
  
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
  
  x <- logs[Forecast_Seg_Update == 1 & Derived_QNH_Update == 1 & Periodic_Update == 1]
  
  x$Wind_State <- ifelse(
    x$Num_Entries == "0", "Normal", ifelse(
      x$Num_Entries == "1", "Stale", ifelse(
        x$Num_Entries == "2", "Failed", " "
      )
    )
  )
  
  x$Runway_Group <- ifelse(
    x$Runway_Group_Index == "0", "R04", ifelse(
      x$Runway_Group_Index == "1", "R06", ifelse(
        x$Runway_Group_Index == "2", "R09", ifelse(
          x$Runway_Group_Index == "3", "R22", ifelse(
            x$Runway_Group_Index == "4", "R24", ifelse(
              x$Runway_Group_Index == "5", "R27", ifelse(
                x$Runway_Group_Index == "6", "RNo", ifelse(
                  x$Runway_Group_Index == "7", "RSo", " "
                )
              )
            )
          )
        )
      )
    )
  )
  
  return(data.table(
    
  ))
  
}

process_CAV_logs_9087 <- function(LogFile) {
  
}

process_CAV_logs_9083 <- function(LogFile) {
  
}
