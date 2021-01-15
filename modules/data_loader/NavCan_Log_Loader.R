process_NavCan_RadarNonModeS <- function(LogFilePath, tbl_Adaptation_Data, tbl_Runway, dbi_con) {
  
  message("[",Sys.time(),"] ", "Begin processing NavCan Non-Mode S surveillance file...")
  logs <- fread(LogFilePath, header = F, skip = 1, na.strings = c("NA", "N/A", "NULL", ""))
  message("[",Sys.time(),"] ", "Read ", nrow(logs), " lines.")
  
  names(logs)[1:9] <- c(
    "FLIGHT_EVENT_DATE",
    "FLIGHT_EVENT_TIME_UTC",
    "ACID",
    "SSR_CODE_REPORTED",
    "FLIGHT_FIX_ALTITUDE_ESTAB_FT",
    "FLIGHT_FIX_LATITUDE_DEG",
    "FLIGHT_FIX_LONGITUDE_DEG",
    "FLIGHT_FIX_SPEED_KN",
    "FLIGHT_FIX_HEADING_DEG"
  )
  
  x <- logs[!is.na(SSR_CODE_REPORTED) & !is.na(ACID) & !is.na(FLIGHT_FIX_ALTITUDE_ESTAB_FT)]
  
  x <- x[as.numeric(FLIGHT_FIX_ALTITUDE_ESTAB_FT) <= 10000 & as.numeric(FLIGHT_FIX_ALTITUDE_ESTAB_FT) > -1000 & !grepl("^7777$", SSR_CODE_REPORTED) & !grepl("^7000$", SSR_CODE_REPORTED)]
  
  if (nrow(x) > 0) {
    
    x <- cbind(x, usp_GI_Latlong_To_XY(as.numeric(x$FLIGHT_FIX_LATITUDE_DEG) * fnc_GI_Degs_To_Rads(), as.numeric(x$FLIGHT_FIX_LONGITUDE_DEG) * fnc_GI_Degs_To_Rads(), tbl_Adaptation_Data))
    
    x <- x[
      Position_X >= mean(tbl_Runway$Threshold_X_Pos) - tbl_Adaptation_Data$Load_X_Range &
        Position_X <= mean(tbl_Runway$Threshold_X_Pos) + tbl_Adaptation_Data$Load_X_Range &
        Position_Y >= mean(tbl_Runway$Threshold_Y_Pos) - tbl_Adaptation_Data$Load_Y_Range &
        Position_Y <= mean(tbl_Runway$Threshold_Y_Pos) + tbl_Adaptation_Data$Load_Y_Range
    ]
    
  }
  
  out <- data.table(
    Flight_Plan_ID = NA,
    Track_Date = x$FLIGHT_EVENT_DATE,
    Track_Time = as.numeric(Time_String_To_Seconds(x$FLIGHT_EVENT_TIME_UTC)),
    Callsign = x$ACID,
    SSR_Code = x$SSR_CODE_REPORTED,
    X_Pos = x$Position_X,
    Y_Pos = x$Position_Y,
    Lat = as.numeric(x$FLIGHT_FIX_LATITUDE_DEG) * fnc_GI_Degs_To_Rads(),
    Lon = as.numeric(x$FLIGHT_FIX_LONGITUDE_DEG) * fnc_GI_Degs_To_Rads(),
    Mode_C = as.numeric(x$FLIGHT_FIX_ALTITUDE_ESTAB_FT) * fnc_GI_Ft_To_M(), 
    Track_SPD = as.numeric(x$FLIGHT_FIX_SPEED_KN) * fnc_GI_Kts_To_M_Per_Sec(),
    Track_HDG = as.numeric(x$FLIGHT_FIX_HEADING_DEG) * fnc_GI_Degs_To_Rads(),
    Track_Number = NA,
    Mode_S_Address = 0,
    Mode_S_GSPD = NA,
    Mode_S_IAS = NA,
    Mode_S_HDG = NA,
    Mode_S_TAS = NA,
    Mode_S_Track_HDG = NA,
    Mode_S_Track_HDG_Rate = NA,
    Mode_S_Roll_Angle = NA,
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
  
  dbWriteTable(dbi_con, "tbl_Radar_Track_Point", out, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(out), " rows to tbl_Radar_Track_Point")
  
}

process_NavCan_FP <- function(LogFilePath, dbi_con) {
  
  message("[",Sys.time(),"] ", "Begin processing NavCan flight plan file...")
  x <- fread(LogFilePath, header = F, skip = 1, na.strings = c("NA", "N/A", "NULL", ""))
  message("[",Sys.time(),"] ", "Read ", nrow(x), " lines.")
  
  names(x)[1:34] <- c(
    "Id",
    "Runway",
    "Carrier_Code",
    "EXCDS_Movement_Id",
    "ACID",
    "Movement_Type_Code",
    "Flight_Rule_Id",
    "AC_Type_Dsgntr_Code",
    "Itinerant_Ind",
    "Flight_Dep_Date_UTC",
    "EXCDS_Site_Code",
    "Arr_Aero_Code",
    "Dep_Aero_Code",
    "Dep_RWY_Dsgntr_Code",
    "Arr_RWY_Dsgntr_Code",
    "Flight_Arr_Date_UTC",
    "ICAO_AC_WTC_Code",
    "UTCDateTime",
    "UTCDate",
    "UTCTime",
    "Airport",
    "LocalDateTime",
    "LocalDate",
    "LocalTime",
    "Local_15_Min_Bin",
    "Local_30_Min_Bin",
    "Local_1_Hour_Bin",
    "Carrier_Name",
    "Movement",
    "Month",
    "Weekday",
    "Flightkey",
    "MovementCode",
    "Year"
  )
  
  out <- data.table(
    FP_Date = x$UTCDate,
    FP_Time = as.numeric(Time_String_To_Seconds(x$UTCTime)),
    Callsign = x$ACID,
    Aircraft_Type = x$AC_Type_Dsgntr_Code,
    SSR_Code = NA,
    Wake_Vortex = x$ICAO_AC_WTC_Code,
    Destination = x$Arr_Aero_Code,
    Landing_Runway = ifelse(grepl("^[0-9]{1}$", x$Arr_RWY_Dsgntr_Code), paste0("R0", x$Arr_RWY_Dsgntr_Code), ifelse(grepl("^[0-9]{2}$", x$Arr_RWY_Dsgntr_Code), paste0("R", x$Arr_RWY_Dsgntr_Code), NA)),
    STAR = NA,
    Origin = x$Dep_Aero_Code,
    Departure_Runway = ifelse(grepl("^[0-9]{1}$", x$Dep_RWY_Dsgntr_Code), paste0("R0", x$Dep_RWY_Dsgntr_Code), ifelse(grepl("^[0-9]{2}$", x$Dep_RWY_Dsgntr_Code), paste0("R", x$Dep_RWY_Dsgntr_Code), NA)),
    SID = NA
  )
  
  out <- out[!is.na(FP_Date) & !is.na(FP_Time) & !is.na(Callsign) & !is.na(SSR_Code)]
  
  message("[",Sys.time(),"] ", "Checking for duplicates within loaded data...")
  
  out_undup <- out[!duplicated(out[,.(FP_Date, Callsign)])]
  out_dup <- out[duplicated(out[,.(FP_Date, Callsign)])]
  
  # Remove some duplicate flight plans
  out_dup_proc <- rbindlist(lapply(unique(paste(out_dup$FP_Date, out_dup$Callsign)), function(j) {
    out_j <- out_dup[paste(FP_Date, Callsign) == j]
    if (!all(is.na(out_j$FP_Time))) {
      return(out_j[FP_Time == max(out_j$FP_Time, na.rm = T)])
    } else {
      out_j[1]
    }
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

process_NavCan_FPAlt <- function(LogFilePath, dbi_con) {
  
  message("[",Sys.time(),"] ", "Begin processing NavCan (alt) flight plan file...")
  x <- fread(LogFilePath, header = F, skip = 1, na.strings = c("NA", "N/A", "NULL", ""))
  message("[",Sys.time(),"] ", "Read ", nrow(x), " lines.")
  
  names(x)[1:11] <- c(
    "LUTUTC",
    "CALLSIGN",
    "SSRCODE",
    "TYPE",
    "WAKE",
    "ORIGIN",
    "DESTINATION",
    "ARRIVAL_RUNWAY",
    "ROUTE",
    "SID",
    "STAR"
  )
  
  x$Timestamp <- as.POSIXct(x$LUTUTC, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  
  out <- data.table(
    FP_Date = format(as.Date(x$Timestamp), "%d/%m/%Y"),
    FP_Time = Time_String_To_Seconds(format(x$Timestamp, "%H:%M:%S")),
    Callsign = x$CALLSIGN,
    Aircraft_Type = x$TYPE,
    SSR_Code = x$SSRCODE,
    Wake_Vortex = x$WAKE,
    Destination = x$DESTINATION,
    Landing_Runway = ifelse(grepl("^[0-9]{1}$", x$ARRIVAL_RUNWAY), paste0("R0", x$ARRIVAL_RUNWAY), ifelse(grepl("^[0-9]{2}$", x$ARRIVAL_RUNWAY), paste0("R", x$ARRIVAL_RUNWAY), NA)),
    STAR = x$STAR,
    Origin = x$ORIGIN,
    Departure_Runway = NA,
    SID = x$SID
  )
  
  out <- out[!is.na(FP_Date) & !is.na(FP_Time) & !is.na(Callsign) & !is.na(SSR_Code)]
  
  message("[",Sys.time(),"] ", "Checking for duplicates within loaded data...")
  
  out_undup <- out[!duplicated(out[,.(FP_Date, Callsign)])]
  out_dup <- out[duplicated(out[,.(FP_Date, Callsign)])]
  
  # Remove some duplicate flight plans
  out_dup_proc <- rbindlist(lapply(unique(paste(out_dup$FP_Date, out_dup$Callsign)), function(j) {
    out_j <- out_dup[paste(FP_Date, Callsign) == j]
    if (!all(is.na(out_j$FP_Time))) {
      return(out_j[FP_Time == max(out_j$FP_Time, na.rm = T)])
    } else {
      out_j[1]
    }
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
          SSR_Code == out_pass_1$SSR_Code[j] &
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

process_NavCan_GR <- function(LogFilePath, tbl_Adaptation_Data, tbl_Runway, dbi_con) {
  
  message("[",Sys.time(),"] ", "Begin processing NavCan ground radar log file...")
  logs <- fread(LogFilePath, header = F, skip = 1, na.strings = c("NA", "N/A", "NULL", ""))
  message("[",Sys.time(),"] ", "Read ", nrow(logs), " lines.")
  
  names(logs)[1:37] <- c(
    "aircraft_type",
    "bitmask",
    "callsign",
    "cart_coord_x",
    "cart_coord_y",
    "cart_velocity_x",
    "cart_velocity_y",
    "departure",
    "destination",
    "direction",
    "id",
    "latitude",
    "longitude",
    "mode_3a",
    "mode_3a_valid",
    "mode_c_altitude",
    "mode_c_valid",
    "mode_s_addr",
    "site_code",
    "speed",
    "stand",
    "the_geom",
    "tid",
    "time_of_day",
    "timestamp",
    "track_info_direction",
    "track_info_size",
    "track_info_type",
    "track_info_vmi",
    "track_num",
    "track_status_fusion",
    "track_status_quality",
    "track_status_rad",
    "turbulence_cat",
    "uuid",
    "valid",
    "vehicle_fleet_id"
  )
  
  x <- logs[destination == "CYYZ" & abs(cart_coord_x) <= 4000 & abs(cart_coord_y) <= 4000 & !is.na(mode_3a) & !is.na(callsign) & !is.na(mode_c_altitude)]
  
  x <- x[as.numeric(mode_c_altitude) <= 10000]
  
  if (nrow(x) > 0) {
    
    x <- cbind(x, usp_GI_Latlong_From_XY(as.numeric(x$cart_coord_x), as.numeric(x$cart_coord_y), tbl_Adaptation_Data))
    
    x <- x[
      as.numeric(x$cart_coord_x) >= mean(tbl_Runway$Threshold_X_Pos) - tbl_Adaptation_Data$Load_X_Range &
        as.numeric(x$cart_coord_x) <= mean(tbl_Runway$Threshold_X_Pos) + tbl_Adaptation_Data$Load_X_Range &
        as.numeric(x$cart_coord_y) >= mean(tbl_Runway$Threshold_Y_Pos) - tbl_Adaptation_Data$Load_Y_Range &
        as.numeric(x$cart_coord_y) <= mean(tbl_Runway$Threshold_Y_Pos) + tbl_Adaptation_Data$Load_Y_Range
    ]
    
    x$timestamp <- as.POSIXct(x$timestamp, "%Y-%m-%d %H:%M:%OS", tz = "UTC")
    
  }
  
  out <- data.table(
    Flight_Plan_ID = NA,
    Track_Date = format(as.Date(x$timestamp), "%d/%m/%Y"),
    Track_Time = Time_String_To_Seconds(format(x$timestamp, "%H:%M:%S")),
    Callsign = x$callsign,
    SSR_Code = x$mode_3a,
    X_Pos = as.numeric(x$cart_coord_x),
    Y_Pos = as.numeric(x$cart_coord_y),
    Lat = x$PositionLatitude,
    Lon = x$PositionLongitude,
    Mode_C = as.numeric(x$mode_c_altitude) * fnc_GI_Ft_To_M(), 
    Track_SPD = NA,
    Track_HDG = NA,
    Track_Number = NA,
    Mode_S_Address = 0,
    Mode_S_GSPD = NA,
    Mode_S_IAS = NA,
    Mode_S_HDG = NA,
    Mode_S_TAS = NA,
    Mode_S_Track_HDG = NA,
    Mode_S_Track_HDG_Rate = NA,
    Mode_S_Roll_Angle = NA,
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
  
  dbWriteTable(dbi_con, "tbl_Radar_Track_Point", out, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(out), " rows to tbl_Radar_Track_Point")
  
}

process_NavCan_SurfaceWindQNH <- function(LogFilePath, Airfield_Name, dbi_con) {
  
  message("[",Sys.time(),"] ", "Begin processing NavCan sensor readings log file...")
  x <- fread(LogFilePath, header = F, skip = 1, na.strings = c("NA", "N/A", "NULL", ""))
  message("[",Sys.time(),"] ", "Read ", nrow(x), " lines.")
  
  names(x)[1:11] <- c(
    "create_date",
    "pressure",
    "altimeter",
    "temperature",
    "rel_humidity",
    "dewpoint",
    "wind_speed",
    "wind_direction",
    "gust_speed",
    "wind_variation_start",
    "wind_variation_end"
  )
  
  x$timestamp <- as.POSIXct(x$create_date, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  out1 <- data.table(
    Airfield = Airfield_Name,
    Landing_Runway = "",
    Anemo_Date = format(as.Date(x$timestamp), "%d/%m/%Y"),
    Anemo_Time = Time_String_To_Seconds(format(x$timestamp, "%H:%M:%S")),
    Anemo_SPD = as.numeric(x$wind_speed) * fnc_GI_Kts_To_M_Per_Sec(),
    Anemo_HDG = as.numeric(x$wind_direction) * fnc_GI_Degs_To_Rads(),
    Anemo_HW = NA,
    Anemo_CW = NA
  )
  
  out1 <- out1[
    !is.na(Airfield) &
      !is.na(Landing_Runway) &
      !is.na(Anemo_Date) &
      !is.na(Anemo_Time) &
      !is.na(Anemo_SPD) &
      !is.na(Anemo_HDG)
  ]
  
  dbWriteTable(dbi_con, "tbl_Anemometer", out1, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(out1), " rows to tbl_Anemometer")
  
  out2 <- data.table(
    Airfield = Airfield_Name,
    Baro_Date = format(as.Date(x$timestamp), "%d/%m/%Y"),
    Baro_Time = Time_String_To_Seconds(format(x$timestamp, "%H:%M:%S")),
    Baro_Pressure = as.numeric(x$pressure) * fnc_GI_Mbar_To_Pa()
  )
  
  out2 <- out2[!is.na(Airfield) & !is.na(Baro_Date) & !is.na(Baro_Time) & !is.na(Baro_Pressure)]
  
  dbWriteTable(dbi_con, "tbl_Baro", out2, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(out2), " rows to tbl_Baro")

}
