process_CAV_logs_9002 <- function(LogFile) {
  
  # Ref Leidos TBS20170004-TBS logging - v1.2
  x <- parse_log_lines(LogFile, "9002", c(
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
  ))
  
  return(data.table(
    Log_Date = format(as.Date(x$Date), "%d/%m/%y"),
    Log_Time = as.numeric(x$Time),
    Callsign = x$Callsign,
    SSR_Code = x$SSR_Code,
    Aircraft_Type = NA,
    Wake_Category = NA,
    Target_Landing_Time = as.numeric(x$Target_Landing_Time),
    Sequence_Number = ifelse(is.na(x$Sequence_Number) | !grepl("^[0-9]{1,2}$", x$Sequence_Number), NA, as.numeric(x$Sequence_Number)),
    Min_Spacing_Infront = as.numeric(x$Min_Space_Before) * fnc_GI_Nm_To_M(),
    Min_Spacing_Behind = as.numeric(x$Min_Space_After) * fnc_GI_Nm_To_M(),
    Arrival_Airport = x$Arrival_Airport,
    Arrival_Runway = x$Arrival_Runway_Intent,
    RNAV_Approach = ifelse(
      x$Approach_Type == "0", "ILS", ifelse(
        x$Approach_Type == "1", "MLS", ifelse(
          x$Approach_Type == "2", "RNAV", ifelse(
            x$Approach_Type == "3", "RNP", "Unknown"
          )
        )
      )
    )
  ))
  
}

process_CAV_logs_9005 <- function(LogFile) {
  
  # Ref Leidos Time Based Separation Logging Definition QeTBSD-TE-4, Version 1.6
  x <- parse_log_lines(LogFile, "9006", c(
    "Date",
    "Time",
    "Message_Type",
    "Message_Length",
    "Barometric_Pressure",
    "GSPD",
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
    "IAS",
    "Asterix_SAC",
    "Asterix_SIC",
    "Asterix_SID",
    "Air_ID",
    "Callsign",
    "Intention",
    "Runway",
    "Roll_Angle",
    "TAS",
    "Roll_Angle_Age",
    "Track_Angle_Age",
    "GSPD_Age",
    "Magnetic_HDG_Age",
    "IAS_Age",
    "Barometric_Age",
    "TAS_Age",
    "Is_Altitude_Corrected"
  ))
  
  x <- x[!is.na(Alt) & !grepl("^-32767$", Alt) & Alt != ""]
  
  x <- x[!is.na(XDot) & !is.na(YDot)]
  
  x <- x[Alt >= -1000 & Alt <= 8000 & XDot >= -8192 & XDot <= 8192 & YDot >= -8192 & YDot <= 8192]
  
  x <- x[!is.na(Is_Altitude_Corrected) & !grepl("^255$", Is_Altitude_Corrected) & Is_Altitude_Corrected != ""]
  
  out <- data.table(
    Log_Date = format(as.Date(x$Date), "%d/%m/%y"),
    Log_Time = as.numeric(x$Time),
    Callsign = x$Air_ID,
    Track_Number = as.numeric(x$Track_ID),
    SSR_Code = as.numeric(x$SSR_Code),
    Mode_S_Address = x$Target_Address,				
    Track_Time = as.numeric(x$System_Time) / 1000,
    Latitude = as.numeric(x$Latitude) * fnc_GI_Degs_To_Rads(),
    Longitude = as.numeric(x$Longitude) * fnc_GI_Degs_To_Rads(),
    X_Pos = as.numeric(x$Rept_Sys_X),
    Y_Pos = as.numeric(x$Rept_Sys_Y),
    Mode_C = as.numeric(x$Alt) * fnc_GI_Ft_To_M(),
    Intention = x$Intention,
    Runway = paste0("R", x$Runway),
    Track_SPD = sqrt(as.numeric(x$XDot)^2 + as.numeric(x$YDot)^2),
    Track_HDG = fnc_GI_To_Vector_Angle(as.numeric(x$XDot), as.numeric(x$YDot)),
    Mode_S_GSPD = ifelse(is.na(x$GSPD) | x$GSPD %in% c("", "-32767"), NA, as.numeric(x$GSPD)) * fnc_GI_Nm_To_M(),
    Mode_S_IAS = ifelse(is.na(x$IAS) | x$IAS %in% c("", "65535"), NA, as.numeric(x$IAS)) * fnc_GI_Kts_To_M_Per_Sec(),
    Mode_S_Mag_HDG = ifelse(is.na(x$Magnetic_Heading) | x$Magnetic_Heading %in% c("", "-32767"), NA, as.numeric(x$Magnetic_Heading)) * fnc_GI_Degs_To_Rads(),
    Mode_S_TAS = ifelse(is.na(x$TAS) | x$TAS %in% c("", "65535"), NA, as.numeric(x$TAS)) * fnc_GI_Kts_To_M_Per_Sec(),
    Mode_S_Track_HDG = ifelse(is.na(x$Track_Angle) | x$Track_Angle %in% c("", "-32767"), NA, as.numeric(x$Track_Angle)) * fnc_GI_Degs_To_Rads(),
    Mode_S_Roll_Angle = ifelse(is.na(x$Roll_Angle) | x$Roll_Angle %in% c("", "-32767"), NA, as.numeric(x$Roll_Angle)) * fnc_GI_Degs_To_Rads(),
    Mode_S_BPS = ifelse(is.na(x$Barometric_Pressure) | x$Barometric_Pressure %in% c("", "-32767"), NA, as.numeric(x$Barometric_Pressure)) * fnc_GI_Mbar_To_Pa(),
    Mode_S_GSPD_Age = ifelse(is.na(x$GSPD_Age) | x$GSPD_Age %in% c("", "255"), NA, as.numeric(x$GSPD_Age)),
    Mode_S_IAS_Age = ifelse(is.na(x$IAS_Age) | x$IAS_Age %in% c("", "255"), NA, as.numeric(x$IAS_Age)),
    Mode_S_Mag_HDG_Age = ifelse(is.na(x$Magnetic_HDG_Age) | x$Magnetic_HDG_Age %in% c("", "255"), NA, as.numeric(x$Magnetic_HDG_Age)),
    Mode_S_TAS_Age = ifelse(is.na(x$TAS_Age) | x$TAS_Age %in% c("", "255"), NA, as.numeric(x$TAS_Age)),
    Mode_S_Track_HDG_Age = ifelse(is.na(x$Track_Angle_Age) | x$Track_Angle_Age %in% c("", "255"), NA, as.numeric(x$Track_Angle_Age)),
    Mode_S_Roll_Angle_Age = ifelse(is.na(x$Roll_Angle_Age) | x$Roll_Angle_Age %in% c("", "255"), NA, as.numeric(x$Roll_Angle_Age)),
    Mode_S_BPS_Age = ifelse(is.na(x$Barometric_Age) | x$Barometric_Age %in% c("", "255"), NA, as.numeric(x$Barometric_Age)),
    Is_Altitude_Corrected = ifelse(x$Is_Altitude_Corrected == "1", T, F)
  )
  
  return(out)
  
}

process_CAV_logs_9041 <- function(LogFile) {
  
  # Ref Leidos Time Based Separation Logging Definition QeTBSD-TE-4, Version 1.6
  x <- parse_log_lines(LogFile, "9041", c(
    "Date",
    "Time",
    "Message_Type",
    "Message_Length",
    "System_Track",
    "RBC",
    "Dep_Airport",
    "Arr_Airport",
    "Flight_ID",
    "Callsign",
    "Aircraft_Type",
    "Is_Assosciated"
  ))
  
  x <- x[Is_Assosciated %in% c("1", "0")]
  
  return(data.table(
    Log_Date = format(as.Date(x$Date), "%d/%m/%y"),
    Log_Time = Time_String_To_Seconds(as.numeric(x$Time)),
    Track_Number = as.numeric(x$System_Track),
    SSR_Code = as.numeric(x$RBC),
    Callsign = x$Callsign,
    Aircraft_Type = x$Aircraft_Type,
    Destination = x$Arr_Airport,
    Is_Assosciated = ifelse(x$Is_Assosciated == "1", T, F)
  ))
  
}

process_CAV_logs_9043 <- function(LogFile) {
  
  # Ref Leidos TBS20170004-TBS logging - v1.2
  logs <- parse_log_lines(LogFile, "9043", c(
    "Date",
    "Time",
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
    paste0(c("Seg_Time",
             "Forecast_Time",
             "Wind_Effect_IAS",
             "Avg_Wind_SPD",
             "Avg_Wind_HDG",
             "Range_To_Threshold",
             "Min_Range_To_Threshold",
             "Max_Range_To_Threshold",
             "Extrapolation_Flag"), "_", rep(seq(1, 99, 1), each = 9))
  ))
  
  x1 <- logs[Derived_QNH_Update == "1" | Periodic_Update == "1"]
  
  out1 <- data.table(
    Log_Date = format(as.Date(x1$Date), "%d/%m/%y"),
    Log_Time = as.numeric(x1$Time),
    Update_Time = as.numeric(x1$Update_Time) / 1000,
    Derived_QNH_Time = as.numeric(x1$Derived_QNH_Time) / 1000,
    Track_Number = as.numeric(x1$Derived_QNH_Track_ID),
    Derived_QNH = as.numeric(x1$Derived_QNH) * fnc_GI_Mbar_To_Pa(),
    Derived_QNH_Valid = ifelse(x1$Derived_QNH_Valid == "1", T, F),
    Periodic_Update = ifelse(x1$Periodic_Update == "1", T, F)
  )
  
  x2 <- logs[Forecast_Seg_Update == "1" | Periodic_Update == "1"]
  
  out2 <- rbindlist(lapply(1:nrow(x2), function(i) {
    
    return(data.table(
      Log_Date = format(as.Date(x2$Date[i]), "%d/%m/%y"),
      Log_Time = as.numeric(x2$Time[i]),
      Runway_Group = ifelse(
        x2$Runway_Group_Index[i] == "0", "R04", ifelse(
          x2$Runway_Group_Index[i] == "1", "R06", ifelse(
            x2$Runway_Group_Index[i] == "2", "R09", ifelse(
              x2$Runway_Group_Index[i] == "3", "R22", ifelse(
                x2$Runway_Group_Index[i] == "4", "R24", ifelse(
                  x2$Runway_Group_Index[i] == "5", "R27", ifelse(
                    x2$Runway_Group_Index[i] == "6", "RNo", ifelse(
                      x2$Runway_Group_Index[i] == "7", "RSo", " "
                    )
                  )
                )
              )
            )
          )
        )
      ),
      Update_Time = as.numeric(x2$Update_Time[i]) / 1000,
      Seg_Time = as.numeric(x2[i][, grep("^Seg_Time_[0-9]+$", names(x2[i]), value = T), with = F]) / 1000,
      Seg_Range_To_Threshold = as.numeric(x2[i][, grep("^Range_To_Threshold_[0-9]+$", names(x2[i]), value = T), with = F]) * fnc_GI_Nm_To_M(),
      Wind_State = ifelse(
        x2$State[i] == "0", "Normal", ifelse(
          x2$State[i] == "1", "Stale", ifelse(
            x2$State[i] == "2", "Failed", ""
          )
        )
      ),
      Ave_Wind_Effect = as.numeric(x2[i][, grep("^Wind_Effect_IAS_[0-9]+$", names(x2[i]), value = T), with = F]) * fnc_GI_Kts_To_M_Per_Sec(),
      Extrapolation_Flag = ifelse(x2[i][, grep("^Extrapolation_Flag_[0-9]+$", names(x2[i]), value = T), with = F] == "1", T, F),
      Periodic_Update = ifelse(x2$Periodic_Update[i] == "1", T, F)
    ))
    
  }))
  
  return(list(tbl_GWCS_Derived_QNH = out1, tbl_GWCS_Forecast_Seg = out2))
  
}

process_CAV_logs_9087 <- function(LogFile) {
  
  # Ref Toronto_IA_Validation_Plan, Iss 1a, Jan 2020.
  x <- parse_log_lines(LogFile, "9087", c(
    "Date",
    "Time",
    "Message_Type",
    "Message_Length",
    "TBS_Threshold_Ref_Time",
    "TBS_FAF_Ref_Time",
    "Thresh_Mean_Wind_Effect",
    "FAF_Mean_Wind_Effect",
    "Threshold_Distance",
    "ORD_Comp_Dist",
    "FAF_Distance",
    "Leader_Dist_To_Thresh",
    "Leader_WVC",
    "Leader_Track_ID",
    "Leader_SSR_Code",
    "Follower_Dist_To_Thresh",
    "Follower_WVC",
    "Follower_Track_ID",
    "Follower_SSR_Code",
    "Leader_Runway",
    "Follower_Runway",
    "Leader_Callsign",
    "Follower_Callsign",
    "Leader_AC_Type",
    "Follower_AC_Type",
    "Threshold_Constraint_Type",
    "FAF_Constraint_Type",
    "TBS_Service_Level",
    "RNAV_Flag",
    "Low_Vis_Flag",
    "In_Trail_Spacing",
    "Runway_Dependency_Separation",
    "Num_Wind_Seg_Entries",
    paste0(c("IDX",
             "Time",
             "Forecast_Time",
             "Assumed_IAS",
             "Wind_Effect_IAS",
             "Range_To_Threshold"), "_", rep(seq(1, 99, 1), each = 6))
  ))
  
  out1 <- data.table(
    Log_Date = format(as.Date(x$Date), "%d/%m/%y"),
    Log_Time = as.numeric(x$Time),
    Leader_Track_Number = as.numeric(x$Leader_Track_ID),
    Leader_Callsign = x$Leader_Callsign,
    Leader_SSR_Code = as.numeric(x$Leader_SSR_Code),
    Follower_Track_Number = as.numeric(x$Follower_Track_ID),
    Follower_Callsign = x$Follower_Callsign,
    Follower_SSR_Code = as.numeric(x$Follower_SSR_Code),
    Leader_Runway = paste0("R", x$Leader_Runway),
    Follower_Runway = paste0("R", x$Follower_Runway),
    TBS_Service_Level = ifelse(
      x$TBS_Service_Level == "0", "TBS", ifelse(
        x$TBS_Service_Level == "1", "DBS", ifelse(
          x$TBS_Service_Level == "2", "No_Service", "Error in service level type"
        )
      )
    ),
    Threshold_Constraint_Type = ifelse(
      x$Threshold_Constraint_Type == "0", "NON_WAKE", ifelse(
        x$Threshold_Constraint_Type == "1", "WAKE", ifelse(
          x$Threshold_Constraint_Type == "2", "SPACING", ifelse(
            x$Threshold_Constraint_Type == "3", "RWY_DEP", ifelse(
              x$Threshold_Constraint_Type == "4", "ROT", "Error in threshold constraint type"
            )
          )
        )
      )
    ),
    FAF_Constraint_Type = ifelse(
      x$FAF_Constraint_Type == "0", "NON_WAKE", ifelse(
        x$FAF_Constraint_Type == "1", "WAKE", ifelse(
          x$FAF_Constraint_Type == "2", "SPACING", ifelse(
            x$FAF_Constraint_Type == "3", "RWY_DEP", ifelse(
              x$FAF_Constraint_Type == "4", "ROT", "Error in threshold constraint type"
            )
          )
        )
      )
    ),
    TBS_Thresh_Ref_Time = as.numeric(x$TBS_Threshold_Ref_Time),
    TBS_FAF_Ref_Time = as.numeric(x$TBS_FAF_Ref_Time),
    Leader_Aircraft_Type = x$Leader_AC_Type,
    Follower_Aircraft_Type = x$Follower_AC_Type,
    Leader_Wake_Cat = ifelse(
      x$Leader_WVC == "0", "A", ifelse(
        x$Leader_WVC == "1", "B", ifelse(
          x$Leader_WVC == "2", "C", ifelse(
            x$Leader_WVC == "3", "D", ifelse(
              x$Leader_WVC == "4", "E", ifelse(
                x$Leader_WVC == "5", "F", ifelse(
                  x$Leader_WVC == "6", "G", "Error in Leader Wake Cat"
                )
              )
            )
          )
        )
      )
    ),
    Follower_Wake_Cat = ifelse(
      x$Follower_WVC == "0", "A", ifelse(
        x$Follower_WVC == "1", "B", ifelse(
          x$Follower_WVC == "2", "C", ifelse(
            x$Follower_WVC == "3", "D", ifelse(
              x$Follower_WVC == "4", "E", ifelse(
                x$Follower_WVC == "5", "F", ifelse(
                  x$Follower_WVC == "6", "G", "Error in Follower Wake Cat"
                )
              )
            )
          )
        )
      )
    ),
    Thresh_Mean_Wind_Effect = as.numeric(x$Thresh_Mean_Wind_Effect) * fnc_GI_Kts_To_M_Per_Sec(),
    FAF_Mean_Wind_Effect = as.numeric(x$FAF_Mean_Wind_Effect) * fnc_GI_Kts_To_M_Per_Sec(),
    Thresh_Distance = as.numeric(x$Threshold_Distance) * fnc_GI_Nm_To_M(),
    ORD_Compression = as.numeric(x$ORD_Comp_Dist) * fnc_GI_Nm_To_M(),
    FAF_Distance = as.numeric(x$FAF_Distance) * fnc_GI_Nm_To_M(),
    Leader_Distance_To_Threshold = as.numeric(x$Leader_Dist_To_Thresh) * fnc_GI_Nm_To_M(),
    Follower_Distance_To_Threshold = as.numeric(x$Follower_Dist_To_Thresh) * fnc_GI_Nm_To_M(),
    RNAV_Flag = ifelse(x$RNAV_Flag == "1", T, F),
    Low_Vis_Flag = x$Low_Vis_Flag,
    In_Trail_Spacing = as.numeric(x$In_Trail_Spacing) * fnc_GI_Nm_To_M(),
    Runway_Dependency_Separation = as.numeric(x$Runway_Dependency_Separation) * fnc_GI_Nm_To_M()
  )
  
  out2 <- rbindlist(lapply(1:nrow(x), function(i) {
    
    return(data.table(
      TBS_Tool_Calculation_ID = i,
      Update_Time = as.numeric(x[i][, grep("^Time_[0-9]+$", names(x[i]), value = T), with = F]) / 1000,
      Seg_Time = as.numeric(x[i][, grep("^Forecast_Time_[0-9]+$", names(x[i]), value = T), with = F]) / 1000,
      Ave_Wind_Effect = as.numeric(x[i][, grep("^Wind_Effect_IAS_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Kts_To_M_Per_Sec(),
      Seg_Range_To_Threshold = as.numeric(x[i][, grep("^Range_To_Threshold_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Nm_To_M()
    ))
    
  }))
  
  return(tbl_TBS_Tool_Calculation = out1, tbl_TBS_Tool_Calculation_Wind_Seg = out2)
  
}

process_CAV_logs_9081 <- function(LogFile) {
  
  # Ref Leidos IA Build 12 Log Format
  x <- parse_log_lines(LogFile, "9081", c(
    "Date",
    "Time",
    "Message_Type",
    "Message_Length",
    "Runway_Group_Index",
    "Leader_Runway_Index",
    "Leader_Dist_To_Runway",
    "Leader_Track_ID",
    "Leader_SSR_Code",
    "Leader_Wake_Vortex_Category_Index",
    "Leader_Callsign",
    "Leader_Aircraft_Type",
    "Follower_Runway_Index",
    "Follower_Dist_To_Runway",
    "Follower_Track_ID",
    "Follower_SSR_Code",
    "Follower_Wake_Vortex_Category_Index",
    "Follower_Callsign",
    "Follower_Aircraft_Type",
    "TBS_Thresh_Sep",
    "ORD_Compression_Distance",
    "GWCS_Wind_Profile_Source",
    "Leader_Vref ",
    "Leader_Velocity",
    "Leader_Velocity_Source",
    "Leader_Deceleration_Position",
    "Leader_Start_Initial_Deceleration_Distance",
    "Leader_Local_Stabilisation_Threshold",
    "Leader_Compression_Commencement_Threshold",
    "Leader_Runway_Surface_Wind_SPD",
    "Leader_Runway_Surface_Wind_HDG",
    "Leader_Current_Runway_Surface_Wind_SPD",
    "Leader_Current_Runway_Surface_Wind_HDG",
    "Leader_Runway_Surface_Headwind",
    "Leader_Landing_Stabilisation_Speed_Type",
    "Leader_Aircraft_Type_Used",
    "Leader_Runway_Surface_Wind_Source",
    "Leader_Wind_Sensor_ID",
    "Follower_VRef",
    "Follower_Velocity",
    "Follower_Velocity_Source",
    "Follower_Deceleration_Position",
    "Follower_Start_Initial_Deceleration_Distance",
    "Follower_Local_Stabilisation_Threshold",
    "Follower_Compression_Commencement_Threshold",
    "Follower_Runway_Surface_Wind_SPD",
    "Follower_Runway_Surface_Wind_HDG",
    "Follower_Current_Runway_Surface_Wind_SPD",
    "Follower_Current_Runway_Surface_Wind_HDG",
    "Follower_Runway_Surface_Headwind",
    "Follower_Landing_Stabilisation_Speed_Type",
    "Follower_Aircraft_Type_Used",
    "Follower_Runway_Surface_Wind_Source",
    "Follower_Wind_Sensor_ID",
    "Num_Wind_Segment_Entries",
    paste0(c("Wind_Segment_Index",
             "Wind_Segment_Time",
             "Wind_Segment_Forecast_Time",
             "Segment_Wind_Effect_IAS",
             "Segment_Range_To_Threshold"), "_", rep(seq(1, 99, 1), each = 5))
  ))
  
  out1 <- data.table(
    Log_Date = format(as.Date(x$Date), "%d/%m/%y"),
    Log_Time = as.numeric(x$Time),
    Leader_Track_Number = as.numeric(x$Leader_Track_ID),
    Leader_Callsign = x$Leader_Callsign,
    Leader_SSR_Code = as.numeric(x$Leader_SSR_Code),
    Leader_Landing_Runway = ifelse(
      x$Leader_Runway_Index == "0", "R05", ifelse(
        x$Leader_Runway_Index == "1", "R06L", ifelse(
          x$Leader_Runway_Index == "2", "R06R", ifelse(
            x$Leader_Runway_Index == "3", "R15L", ifelse(
              x$Leader_Runway_Index == "4", "R15R", ifelse(
                x$Leader_Runway_Index == "5", "R23", ifelse(
                  x$Leader_Runway_Index == "6", "R24L", ifelse(
                    x$Leader_Runway_Index == "7", "R24R", ifelse(
                      x$Leader_Runway_Index == "8", "R33L", ifelse(
                        x$Leader_Runway_Index == "9", "R33R", "Error in Leader Landing Runway"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    Follower_Track_Number = as.numeric(x$Follower_Track_ID),
    Follower_Callsign = x$Follower_Callsign,
    Follower_SSR_Code = as.numeric(x$Follower_SSR_Code),
    Follower_Landing_Runway = ifelse(
      x$Follower_Landing_Runway_Index == "0", "R05", ifelse(
        x$Follower_Landing_Runway_Index == "1", "R06L", ifelse(
          x$Follower_Landing_Runway_Index == "2", "R06R", ifelse(
            x$Follower_Landing_Runway_Index == "3", "R15L", ifelse(
              x$Follower_Landing_Runway_Index == "4", "R15R", ifelse(
                x$Follower_Landing_Runway_Index == "5", "R23", ifelse(
                  x$Follower_Landing_Runway_Index == "6", "R24L", ifelse(
                    x$Follower_Landing_Runway_Index == "7", "R24R", ifelse(
                      x$Follower_Landing_Runway_Index == "8", "R33L", ifelse(
                        x$Follower_Landing_Runway_Index == "9", "R33R", "Error in Follower Landing Runway"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    ORD_Calc_Type = ifelse(
      as.numeric(x$Leader_Dist_To_Runway) >= as.numeric(x$Leader_Compression_Commencement_Threshold), "Lead_Outside_CCT", ifelse(
        as.numeric(x$Leader_Dist_To_Runway) < as.numeric(x$Leader_Compression_Commencement_Threshold), "Lead_Inside_CCT", "Error in calc type"
      )
    ),
    Leader_AGI_Surface_Wind_SPD = as.numeric(x$Leader_Runway_Surface_Wind_SPD) * fnc_GI_Kts_To_M_Per_Sec(),
    Leader_AGI_Surface_Wind_HDG = as.numeric(x$Leader_Runway_Surface_Wind_HDG) * fnc_GI_Degs_To_Rads(),
    Leader_AGI_Surface_Headwind = as.numeric(x$Leader_Runway_Surface_Headwind) * fnc_GI_Kts_To_M_Per_Sec(),
    Leader_Default_Surface_Wind_Flag = ifelse(x$Leader_Runway_Surface_Wind_Source == "1", T, F),
    Follower_AGI_Surface_Wind_SPD = as.numeric(x$Follower_Runway_Surface_Wind_SPD) * fnc_GI_Kts_To_M_Per_Sec(),
    Follower_AGI_Surface_Wind_HDG = as.numeric(x$Follower_Runway_Surface_Wind_HDG) * fnc_GI_Degs_To_Rads(),
    Follower_AGI_Surface_Headwind = as.numeric(x$Follower_Runway_Surface_Headwind) * fnc_GI_Kts_To_M_Per_Sec(),
    Follower_Default_Surface_Wind_Flag = ifelse(x$Follower_Runway_Surface_Wind_Source == "1", T, F),
    Default_GWCS_Profile_Flag = ifelse(x$Default_GWCS_Profile_Flag == "1", T, F),
    ORD_Separation_Distance = as.numeric(x$TBS_Thresh_Sep) * fnc_GI_Nm_To_M(),
    Leader_Distance_To_Threshold = as.numeric(x$Leader_Dist_To_Runway) * fnc_GI_Nm_To_M(),
    Leader_Aircraft_Type = x$Leader_Aircraft_Type,
    Leader_Aircraft_Type_Used_Flag = ifelse(x$Leader_Aircraft_Type_Used == "1", T, F),
    Follower_Aircraft_Type = x$Follower_Aircraft_Type,
    Follower_Aircraft_Type_Used_Flag = ifelse(x$Follower_Aircraft_Type_Used == "1", T, F),
    Leader_Wake_Cat = ifelse(
      x$Leader_Wake_Vortex_Category_Index == "0", "A", ifelse(
        x$Leader_Wake_Vortex_Category_Index == "1", "B", ifelse(
          x$Leader_Wake_Vortex_Category_Index == "2", "C", ifelse(
            x$Leader_Wake_Vortex_Category_Index == "3", "D", ifelse(
              x$Leader_Wake_Vortex_Category_Index == "4", "E", ifelse(
                x$Leader_Wake_Vortex_Category_Index == "5", "F", ifelse(
                  x$Leader_Wake_Vortex_Category_Index == "6", "G", "Error in Leader Wake Cat"
                )
              )
            )
          )
        )
      )
    ),
    Follower_Wake_Cat = ifelse(
      x$Follower_Wake_Vortex_Category_Index == "0", "A", ifelse(
        x$Follower_Wake_Vortex_Category_Index == "1", "B", ifelse(
          x$Follower_Wake_Vortex_Category_Index == "2", "C", ifelse(
            x$Follower_Wake_Vortex_Category_Index == "3", "D", ifelse(
              x$Follower_Wake_Vortex_Category_Index == "4", "E", ifelse(
                x$Follower_Wake_Vortex_Category_Index == "5", "F", ifelse(
                  x$Follower_Wake_Vortex_Category_Index == "6", "G", "Error in Follower Wake Cat"
                )
              )
            )
          )
        )
      )
    ),
    Leader_VRef = as.numeric(x$Leader_Vref) * fnc_GI_Kts_To_M_Per_Sec(),
    Leader_VApp = as.numeric(x$Leader_Velocity) * fnc_GI_Kts_To_M_Per_Sec(),
    Follower_VRef = as.numeric(x$Follower_Vref) * fnc_GI_Kts_To_M_Per_Sec(),
    Follower_VApp = as.numeric(x$Follower_Velocity) * fnc_GI_Kts_To_M_Per_Sec(),
    Leader_Start_Initial_Deceleration_Distance = as.numeric(x$Leader_Start_Initial_Deceleration_Distance) * fnc_GI_Nm_To_M(),
    Leader_Final_Deceleration_Distance = as.numeric(x$Leader_Deceleration_Position) * fnc_GI_Nm_To_M(),
    Follower_Start_Initial_Deceleration_Distance = as.numeric(x$Follower_Start_Initial_Deceleration_Distance) * fnc_GI_Nm_To_M(),
    Follower_Final_Deceleration_Distance = as.numeric(x$Follower_Deceleration_Position) * fnc_GI_Nm_To_M(),
    ORD_Compression = as.numeric(x$ORD_Compression_Distance) * fnc_GI_Nm_To_M(),
    TBS_Table_Distance = NA
  )
  
  out2 <- rbindlist(lapply(1:nrow(x), function(i) {
    
    return(data.table(
      ORD_Tool_Calculation_ID = i,
      Update_Time = as.numeric(x[i][, grep("^Wind_Segment_Forecast_Time_[0-9]+$", names(x[i]), value = T), with = F]) / 1000,
      Seg_Time = as.numeric(x[i][, grep("^Wind_Segment_Time_[0-9]+$", names(x[i]), value = T), with = F]) / 1000,
      Ave_Wind_Effect = as.numeric(x[i][, grep("^Segment_Wind_Effect_IAS_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Kts_To_M_Per_Sec(),
      Seg_Range_To_Threshold = as.numeric(x[i][, grep("^Segment_Range_To_Threshold_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Nm_To_M()
    ))
    
  }))
  
  return(tbl_ORD_Tool_Calculation = out1, tbl_ORD_Tool_Calculation_Wind_Seg = out2)
  
}

process_CAV_logs_9083 <- function(LogFile) {
  
  # Ref Leidos IA Build 12 Log Format
  logs <- rbindlist(lapply(grep(paste0("^9081, .*$"), LogFile), function(i) {
    
    Log_Date <- as.character(as.Date(gsub("^([0-9]{2}-[0-9]{2}-[0-9]{2})[T ]{1}([0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3})  Message: [0-9]{4}$", "\\1", LogFile[i-1]), "%y-%m-%d"))
    Log_Time <- Time_String_To_Seconds(gsub("^([0-9]{2}-[0-9]{2}-[0-9]{2})[T ]{1}([0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3})  Message: [0-9]{4}$", "\\2", LogFile[i-1]))
    
    LogContents <- data.table(t(c(Log_Date, Log_Time, unlist(strsplit(LogFile[i], split = "\\s{0,},\\s{0,}")))))
    
    names(LogContents) <- c(
      "Date",
      "Time",
      "Message_Type",
      "Message_Length",
      "Wind_Sensor_ID",
      "Runway_Surface_Wind_SPD",
      "Runway_Surface_Wind_HDG",
      "Runway_Surface_Headwind",
      "Runway_Surface_Wind_Source",
      "GWCS_Wind_Profile_Source",
      "Num_Wind_Segment_Entries",
      "Num_TBS_Table_Entries",
      "Runway_Index",
      paste0(c(
        "Wind_Segment_Index",
        "Wind_Segment_Time",
        "Wind_Segment_Forecast_Time",
        "Segment_Wind_Effect_IAS",
        "Segment_Range_To_Threshold"
      ), "_", rep(seq(1, as.numeric(LogContents$V11), 1), each = 5)),
      paste0(c(
        "TBS_Table_Entry_Index",
        "TBS_Thresh_Sep",
        "ORD_Compression_Distance",
        "DBS_Reference_Value",
        "Leader_VRef",
        "Leader_Velocity",
        "Leader_Velocity_Source",
        "Leader_Local_Stab_Thresh",
        "Leader_Compression_Commencement_Threshold",
        "Leader_Landing_Stab_Speed_Type",
        "Leader_Deceleration_Position",
        "Leader_Start_Initial_Deceleration",
        "Follower_VRef",
        "Follower_Velocity",
        "Follower_Velocity_Source",
        "Follower_Local_Stab_Thresh",
        "Follower_Compression_Commencement_Threshold",
        "Follower_Landing_Stab_Speed_Type",
        "Follower_Deceleration_Position",
        "Follower_Start_Initial_Deceleration",
      ), "_", rep(seq(1, as.numeric(LogContents$V12), 1), each = 20)),
    )
    
    return(LogContents)
    
  }), use.names = T, fill = T)
  
  out1 <- rbindlist(lapply(1:nrow(x), function(i) {
    
    Landing_Runway <- ifelse(
      x$Runway_Index[i] == "0", "R05", ifelse(
        x$Runway_Index[i] == "1", "R06L", ifelse(
          x$Runway_Index[i] == "2", "R06R", ifelse(
            x$Runway_Index[i] == "3", "R15L", ifelse(
              x$Runway_Index[i] == "4", "R15R", ifelse(
                x$Runway_Index[i] == "5", "R23", ifelse(
                  x$Runway_Index[i] == "6", "R24L", ifelse(
                    x$Runway_Index[i] == "7", "R24R", ifelse(
                      x$Runway_Index[i] == "8", "R33L", ifelse(
                        x$Runway_Index[i] == "9", "R33R", "Error in Runway"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    
    Runway_Surface_Wind_SPD <- as.numeric(x$Runway_Surface_Wind_SPD[i]) * fnc_GI_Kts_To_M_Per_Sec()
    Runway_Surface_Wind_HDG <- as.numeric(x$Runway_Surface_Wind_HDG[i]) * fnc_GI_Degs_To_Rads()
    Leader_AGI_Surface_Headwind <- as.numeric(x$Leader_AGI_Surface_Headwind[i]) * fnc_GI_Kts_To_M_Per_Sec()
    Runway_Surface_Wind_Source <- ifelse(x$Runway_Surface_Wind_Source[i] == "1", T, F)
    
    return(data.table(
      Log_Date = format(as.Date(x$Date[i]), "%d/%m/%y"),
      Log_Time = as.numeric(x$Time[i]),
      Leader_Track_Number = NA,
      Leader_Callsign = NA,
      Leader_SSR_Code = NA,
      Leader_Landing_Runway = Landing_Runway,
      Follower_Track_Number = NA,
      Follower_Callsign = NA,
      Follower_SSR_Code = NA,
      Follower_Landing_Runway = Landing_Runway,
      ORD_Calc_Type = "TBS_Table",
      Leader_AGI_Surface_Wind_SPD = Runway_Surface_Wind_SPD,
      Leader_AGI_Surface_Wind_HDG = Runway_Surface_Wind_HDG,
      Leader_AGI_Surface_Headwind = Leader_AGI_Surface_Headwind,
      Leader_Default_Surface_Wind_Flag = Runway_Surface_Wind_Source,
      Follower_AGI_Surface_Wind_SPD = Runway_Surface_Wind_SPD,
      Follower_AGI_Surface_Wind_HDG = Runway_Surface_Wind_HDG,
      Follower_AGI_Surface_Headwind = Leader_AGI_Surface_Headwind,
      Follower_Default_Surface_Wind_Flag = Runway_Surface_Wind_Source,
      Default_GWCS_Profile_Flag = ifelse(x$GWCS_Wind_Profile_Source[i] == "1", T, F),
      ORD_Separation_Distance = as.numeric(x[i][, grep("^TBS_Thresh_Sep_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Nm_To_M(),
      Leader_Distance_To_Threshold = NA,
      Leader_Aircraft_Type = NA,
      Leader_Aircraft_Type_Used_Flag = NA,
      Follower_Aircraft_Type = NA,
      Follower_Aircraft_Type_Used_Flag = NA,
      Leader_Wake_Cat = NA,
      Follower_Wake_Cat = NA,
      Leader_VRef = as.numeric(x[i][, grep("^Leader_VRef_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Kts_To_M_Per_Sec(),
      Leader_VApp = as.numeric(x[i][, grep("^Leader_Velocity_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Kts_To_M_Per_Sec(),
      Follower_VRef = as.numeric(x[i][, grep("^Follower_VRef_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Kts_To_M_Per_Sec(),
      Follower_VApp = as.numeric(x[i][, grep("^Follower_Velocity_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Kts_To_M_Per_Sec(),
      Leader_Start_Initial_Deceleration_Distance = as.numeric(x[i][, grep("^Leader_Start_Initial_Deceleration_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Nm_To_M(),
      Leader_Final_Deceleration_Distance = as.numeric(x[i][, grep("^Leader_Deceleration_Position_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Nm_To_M(),
      Follower_Start_Initial_Deceleration_Distance = as.numeric(x[i][, grep("^Follower_Start_Initial_Deceleration_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Nm_To_M(),
      Follower_Final_Deceleration_Distance = as.numeric(x[i][, grep("^Follower_Deceleration_Position_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Nm_To_M(),
      ORD_Compression = as.numeric(x[i][, grep("^ORD_Compression_Distance_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Nm_To_M(),
      TBS_Table_Distance = as.numeric(x[i][, grep("^DBS_Reference_Value_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Nm_To_M()
    ))
    
  }))
  
  out2 <- rbindlist(lapply(1:nrow(x), function(i) {
    
    return(data.table(
      ORD_Tool_Calculation_ID = i,
      Update_Time = as.numeric(x[i][, grep("^Wind_Segment_Forecast_Time_[0-9]+$", names(x[i]), value = T), with = F]) / 1000,
      Seg_Time = as.numeric(x[i][, grep("^Wind_Segment_Time_[0-9]+$", names(x[i]), value = T), with = F]) / 1000,
      Ave_Wind_Effect = as.numeric(x[i][, grep("^Segment_Wind_Effect_IAS_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Kts_To_M_Per_Sec(),
      Seg_Range_To_Threshold = as.numeric(x[i][, grep("^Segment_Range_To_Threshold_[0-9]+$", names(x[i]), value = T), with = F]) * fnc_GI_Nm_To_M()
    ))
    
  }))
  
  return(tbl_ORD_Tool_Calculation = out1, tbl_ORD_Tool_Calculation_Wind_Seg = out2)
  
}

process_CAV_logs <- function(LogFilePath, dbi_con = dbi_con) {
  
  message("[",Sys.time(),"] ", "Begin processing TBS log file...")
  LogFile <- readLines(LogFilePath)
  message("[",Sys.time(),"] ", "Read ", length(LogFile), " lines.")
  
  ### Flight Plan
  
  message("[",Sys.time(),"] ", "Begin processing 9002 entries...")
  logs_9002 <- process_CAV_logs_9002(LogFile)
  message("[",Sys.time(),"] ", "Finished processing 9002 entries (", nrow(logs_9002), " found).")

  # logs_9002 <- logs_9002[!is.na(Log_Date) & !is.na(Callsign)]
  
  message("[",Sys.time(),"] ", "Checking for duplicates within loaded data...")
  out_pass_1 <- unique(out, by = c("Log_Date", "Callsign"))
  
  message("[",Sys.time(),"] ", "Checking for duplicates with existing data...")
  fp <- as.data.table(dbGetQuery(dbi_con, "SELECT DISTINCT Log_Date, Callsign FROM tbl_Aman_Flight_Plan"))
  if (nrow(fp) > 0) {
    out_pass_2 <- out_pass_1[paste(Log_Date, Callsign) %!in% paste(fp$Log_Date, fp$Callsign)]
  } else {
    out_pass_2 <- out_pass_1
  }
  
  message("[",Sys.time(),"] ", "Appending ", nrow(out_pass_2), " rows to tbl_Aman_Flight_Plan...")
  dbWriteTable(dbi_con, "tbl_Aman_Flight_Plan", out_pass_2, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(out_pass_2), " rows to tbl_Aman_Flight_Plan")

  ### Radar tracks
  
  message("[",Sys.time(),"] ", "Begin processing 9005 entries...")
  logs_9005 <- process_CAV_logs_9005(LogFile)
  message("[",Sys.time(),"] ", "Finished processing 9005 entries (", nrow(logs_9005), " found), cross referencing FPIDs...")
  # Flight plan ID cross reference
  if (nrow(logs_9005) > 0) {
    message("[",Sys.time(),"] ", "Generating Flight_Plan_ID...")
    logs_9005 <- generateFPID(logs_9005, dbi_con)
    message("[",Sys.time(),"] ", "Finished cross referencing FPIDs, saving to tbl_Radar_Track_Point...")
    dbWriteTable(dbi_con, "tbl_Radar_Track_Point", logs_9005, append = T)
    message("[",Sys.time(),"] ", "Successfully appended rows to tbl_Radar_Track_Point.")
  }
  
  ### GWCS AMAN Sequencing
  
  message("[",Sys.time(),"] ", "Begin processing 9041 entries...")
  logs_9041 <- process_CAV_logs_9041(LogFile)
  message("[",Sys.time(),"] ", "Finished processing 9041 entries (", nrow(logs_9041), " found), saving to tbl_GWCS_AMAN_Sequencing...")
  dbWriteTable(dbi_con, "tbl_GWCS_AMAN_Sequencing", logs_9041, append = T)
  message("[",Sys.time(),"] ", "Successfully appended rows to tbl_GWCS_AMAN_Sequencing.")
  
  ### GWCS Derived QNH and Forecast Seg
  
  message("[",Sys.time(),"] ", "Begin processing 9043 entries...")
  logs_9043 <- process_CAV_logs_9043(LogFile)
  message("[",Sys.time(),"] ", "Finished processing 9043 entries (", nrow(logs_9043$tbl_GWCS_Derived_QNH), " Derived QNH and ", nrow(logs_9043$tbl_GWCS_Forecast_Seg), " Forecast Seg found).")
  dbWriteTable(dbi_con, "tbl_GWCS_Derived_QNH", logs_9043$tbl_GWCS_Derived_QNH, append = T)
  message("[",Sys.time(),"] ", "Successfully appended rows to tbl_GWCS_Derived_QNH.")
  dbWriteTable(dbi_con, "tbl_GWCS_Forecast_Seg", logs_9043$tbl_GWCS_Forecast_Seg, append = T)
  message("[",Sys.time(),"] ", "Successfully appended rows to tbl_GWCS_Forecast_Seg.")
  
  ### TBS Tool Calculation and Wind Seg
  
  message("[",Sys.time(),"] ", "Begin processing 9087 entries...")
  logs_9087 <- process_CAV_logs_9087(LogFile)
  message("[",Sys.time(),"] ", "Finished processing 9087 entries (", nrow(logs_9087$tbl_TBS_Tool_Calculation), " Derived QNH and ", nrow(logs_9087$tbl_TBS_Tool_Calculation_Wind_Seg), " Forecast Seg found).")
  
  last_ID <- as.vector(unlist(dbGetQuery(dbi_con, "SELECT TOP 1 TBS_Tool_Calculation_ID FROM tbl_TBS_Tool_Calculation ORDER BY TBS_Tool_Calculation_ID DESC")))
  logs_9087$tbl_TBS_Tool_Calculation_Wind_Seg$TBS_Tool_Calculation_ID <- logs_9087$tbl_TBS_Tool_Calculation_Wind_Seg$TBS_Tool_Calculation_ID + last_ID
  
  dbWriteTable(dbi_con, "tbl_TBS_Tool_Calculation", logs_9087$tbl_TBS_Tool_Calculation, append = T)
  message("[",Sys.time(),"] ", "Successfully appended rows to tbl_TBS_Tool_Calculation.")
  dbWriteTable(dbi_con, "tbl_TBS_Tool_Calculation_Wind_Seg", logs_9087$tbl_TBS_Tool_Calculation_Wind_Seg, append = T)
  message("[",Sys.time(),"] ", "Successfully appended rows to tbl_TBS_Tool_Calculation_Wind_Seg.")
  
  ### ORD Tool Calculation and Wind Seg (9081)
  
  message("[",Sys.time(),"] ", "Begin processing 9081 entries...")
  logs_9081 <- process_CAV_logs_9081(LogFile)
  message("[",Sys.time(),"] ", "Finished processing 9081 entries (", nrow(logs_9081$tbl_ORD_Tool_Calculation), " Derived QNH and ", nrow(logs_9081$tbl_ORD_Tool_Calculation_Wind_Seg), " Forecast Seg found).")
  
  last_ID <- as.vector(unlist(dbGetQuery(dbi_con, "SELECT TOP 1 ORD_Tool_Calculation_ID FROM tbl_ORD_Tool_Calculation ORDER BY ORD_Tool_Calculation_ID DESC")))
  logs_9081$tbl_ORD_Tool_Calculation_Wind_Seg$ORD_Tool_Calculation_ID <- logs_9081$tbl_ORD_Tool_Calculation_Wind_Seg$ORD_Tool_Calculation_ID + last_ID
  
  dbWriteTable(dbi_con, "tbl_ORD_Tool_Calculation", logs_9081$tbl_ORD_Tool_Calculation, append = T)
  message("[",Sys.time(),"] ", "Successfully appended rows to tbl_ORD_Tool_Calculation.")
  dbWriteTable(dbi_con, "tbl_ORD_Tool_Calculation_Wind_Seg", logs_9081$tbl_ORD_Tool_Calculation_Wind_Seg, append = T)
  message("[",Sys.time(),"] ", "Successfully appended rows to tbl_ORD_Tool_Calculation_Wind_Seg.")
  
  ### ORD Tool Calculation and Wind Seg (9083)
  
  message("[",Sys.time(),"] ", "Begin processing 9083 entries...")
  logs_9083 <- process_CAV_logs_9083(LogFile)
  message("[",Sys.time(),"] ", "Finished processing 9083 entries (", nrow(logs_9083$tbl_ORD_Tool_Calculation), " Derived QNH and ", nrow(logs_9083$tbl_ORD_Tool_Calculation_Wind_Seg), " Forecast Seg found).")
  
  last_ID <- as.vector(unlist(dbGetQuery(dbi_con, "SELECT TOP 1 ORD_Tool_Calculation_ID FROM tbl_ORD_Tool_Calculation ORDER BY ORD_Tool_Calculation_ID DESC")))
  logs_9083$tbl_ORD_Tool_Calculation_Wind_Seg$ORD_Tool_Calculation_ID <- logs_9083$tbl_ORD_Tool_Calculation_Wind_Seg$ORD_Tool_Calculation_ID + last_ID
  
  dbWriteTable(dbi_con, "tbl_ORD_Tool_Calculation", logs_9083$tbl_ORD_Tool_Calculation, append = T)
  message("[",Sys.time(),"] ", "Successfully appended rows to tbl_ORD_Tool_Calculation.")
  dbWriteTable(dbi_con, "tbl_ORD_Tool_Calculation_Wind_Seg", logs_9083$tbl_ORD_Tool_Calculation_Wind_Seg, append = T)
  message("[",Sys.time(),"] ", "Successfully appended rows to tbl_ORD_Tool_Calculation_Wind_Seg.")
  
}
