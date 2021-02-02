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
  
  message("[",Sys.time(),"] ", "Retrieved ", nrow(x), " valid rows.")
  
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
    Mode_S_Address = NA,
    Mode_S_GSPD = NA,
    Mode_S_IAS = NA,
    Mode_S_HDG = NA,
    Mode_S_TAS = NA,
    Mode_S_Track_HDG = NA,
    Mode_S_Track_HDG_Rate = NA,
    Mode_S_Roll_Angle = NA,
    Mode_S_BPS = NA
  )
  
  if (nrow(out) > 0) {
    message("[",Sys.time(),"] ", "Generating Flight_Plan_ID...")
    out <- generateFPID(out, dbi_con)
    
    message("[",Sys.time(),"] ", "Appending ", nrow(out), " rows to tbl_Radar_Track_Point...")
    dbWriteTable(dbi_con, "tbl_Radar_Track_Point", out, append = T)
    message("[",Sys.time(),"] ", "Successfully appended ", nrow(out), " rows to tbl_Radar_Track_Point")
  } else {
    message("[",Sys.time(),"] ", "Exited without change to database.")
  }
  
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
    Landing_Runway = ifelse(grepl("^[0-9]{1}[A-Z]?$", x$Arr_RWY_Dsgntr_Code), paste0("R0", x$Arr_RWY_Dsgntr_Code), ifelse(grepl("^[0-9]{2}[A-Z]?$", x$Arr_RWY_Dsgntr_Code), paste0("R", x$Arr_RWY_Dsgntr_Code), NA)),
    STAR = NA,
    Origin = x$Dep_Aero_Code,
    Departure_Runway = ifelse(grepl("^[0-9]{1}[A-Z]?$", x$Dep_RWY_Dsgntr_Code), paste0("R0", x$Dep_RWY_Dsgntr_Code), ifelse(grepl("^[0-9]{2}[A-Z]?$", x$Dep_RWY_Dsgntr_Code), paste0("R", x$Dep_RWY_Dsgntr_Code), NA)),
    SID = NA
  )
  
  out <- out[!is.na(FP_Date) & !is.na(FP_Time) & !is.na(Callsign) & !is.na(SSR_Code)]
  
  message("[",Sys.time(),"] ", "Checking for duplicates within loaded data...")
  out_pass_1 <- unique(out, by = c("FP_Date", "Callsign"))
  
  message("[",Sys.time(),"] ", "Checking for duplicates with existing data...")
  fp <- as.data.table(dbGetQuery(dbi_con, "SELECT DISTINCT FP_Date, Callsign, Destination FROM tbl_Flight_Plan"))
  if (nrow(fp) > 0) {
    out_pass_2 <- out_pass_1[paste(FP_Date, Callsign, Destination) %!in% paste(fp$FP_Date, fp$Callsign, fp$Destination)]
  } else {
    out_pass_2 <- out_pass_1
  }
  
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
    Landing_Runway = ifelse(grepl("^[0-9]{1}[A-Z]?$", x$ARRIVAL_RUNWAY), paste0("R0", x$ARRIVAL_RUNWAY), ifelse(grepl("^[0-9]{2}[A-Z]?$", x$ARRIVAL_RUNWAY), paste0("R", x$ARRIVAL_RUNWAY), NA)),
    STAR = x$STAR,
    Origin = x$ORIGIN,
    Departure_Runway = NA,
    SID = x$SID
  )
  
  out <- out[!is.na(FP_Date) & !is.na(FP_Time) & !is.na(Callsign) & !is.na(SSR_Code)]
  
  message("[",Sys.time(),"] ", "Checking for duplicates within loaded data...")
  out_pass_1 <- unique(out, by = c("FP_Date", "Callsign"))
  
  message("[",Sys.time(),"] ", "Checking for duplicates with existing data...")
  fp <- as.data.table(dbGetQuery(dbi_con, "SELECT DISTINCT FP_Date, Callsign, Destination FROM tbl_Flight_Plan"))
  if (nrow(fp) > 0) {
    out_pass_2 <- out_pass_1[paste(FP_Date, Callsign, Destination) %!in% paste(fp$FP_Date, fp$Callsign, fp$Destination)]
  } else {
    out_pass_2 <- out_pass_1
  }
  
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
  
  if (nrow(out) > 0) {
    message("[",Sys.time(),"] ", "Generating Flight_Plan_ID...")
    out <- generateFPID(out, dbi_con)
    
    message("[",Sys.time(),"] ", "Appending ", nrow(out), " rows to tbl_Radar_Track_Point...")
    dbWriteTable(dbi_con, "tbl_Radar_Track_Point", out, append = T)
    message("[",Sys.time(),"] ", "Successfully appended ", nrow(out), " rows to tbl_Radar_Track_Point")
  } else {
    message("[",Sys.time(),"] ", "Exited without change to database.")
  }
  
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

process_NavCan_Fusion_Cat62 <- function(LogFilePath, tbl_Adaptation_Data, dbi_con) {
  
  Date_String <- Asterix_Filename_To_Date(basename(LogFilePath))
  message("[",Sys.time(),"] ", "Found date from filename: ", Date_String)
  
  message("[",Sys.time(),"] ", "Begin processing NavCan Fusion CAT62 log file...")
  logs <- readLines(LogFilePath)
  message("[",Sys.time(),"] ", "Read ", length(logs), " lines.")
  
  x <- data.table(grep("^98,160.*$", logs, value = T))[, tstrsplit(V1, ",", fixed = T)]
  message("[",Sys.time(),"] ", "Found ", nrow(x), " valid CAT62 lines.")
  
  names(x) <- c(
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
  )[1:length(names(x))]
  
  nrow1 <- nrow(x)
  x <- x[!(is.na(`I062/135/CTL`) | `I062/135/CTL` == "") & !(is.na(`I062/390/CSN/CSN`) & is.na(x$`I062/380/ID/ID`))]
  nrow2 <- nrow(x)
  message("[",Sys.time(),"] ", "Removed ", nrow1 - nrow2, " rows with NULL values in I062/135/CTL or both I062/390/CSN/CSN and I062/380/ID/ID)")
  
  if (nrow(x) > 0) {

    if (tbl_Adaptation_Data$Use_Local_Coords) {
      x <- cbind(x, usp_GI_Latlong_To_XY(as.numeric(x$`I062/105/Lat`) * fnc_GI_Degs_To_Rads(), as.numeric(x$`I062/105/Lon`) * fnc_GI_Degs_To_Rads(), tbl_Adaptation_Data))
    } else {
      x <- cbind(x, usp_GI_Latlong_From_XY(as.numeric(x$`I062/100/X`), as.numeric(x$`I062/100/Y`), tbl_Adaptation_Data))
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
    Callsign = ifelse(nchar(x$`I062/390/CSN/CSN`) < nchar(x$`I062/380/ID/ID`), x$`I062/380/ID/ID`, x$`I062/390/CSN/CSN`),
    SSR_Code = x$`I062/060/Mode3A`,
    X_Pos = if (tbl_Adaptation_Data$Use_Local_Coords) {x$Position_X} else {x$`I062/100/X`},
    Y_Pos = if (tbl_Adaptation_Data$Use_Local_Coords) {x$Position_Y} else {x$`I062/100/Y`},
    Lat = if (tbl_Adaptation_Data$Use_Local_Coords) {as.numeric(x$`I062/105/Lat`) * fnc_GI_Degs_To_Rads()} else {x$PositionLatitude},
    Lon = if (tbl_Adaptation_Data$Use_Local_Coords) {as.numeric(x$`I062/105/Lon`) * fnc_GI_Degs_To_Rads()} else {x$PositionLongitude},
    Mode_C = as.numeric(x$`I062/135/CTL`) * 100 * fnc_GI_Ft_To_M(),
    Track_SPD = sqrt(as.numeric(x$`I062/185/VX`)^2 + as.numeric(x$`I062/185/VY`)^2),
    Track_HDG = fnc_GI_To_Vector_Angle(as.numeric(x$`I062/185/VX`), as.numeric(x$`I062/185/VY`)),
    Track_Number = as.integer(x$`I062/040/TrackNum`),
    Mode_S_Address = x$`I062/380/ADR/ADR`,
    Mode_S_GSPD = as.numeric(x$`I062/380/GSP/GSP`) * fnc_GI_Kts_To_M_Per_Sec(),
    Mode_S_IAS = as.numeric(x$`I062/380/IAR/IAR`) * fnc_GI_Kts_To_M_Per_Sec(),
    Mode_S_HDG = as.numeric(x$`I062/380/MHG/MHG`) * fnc_GI_Degs_To_Rads(),
    Mode_S_TAS = as.numeric(x$`I062/380/TAS/TAS`) * fnc_GI_Kts_To_M_Per_Sec(),
    Mode_S_Track_HDG = as.numeric(x$`I062/380/TAN/TAN`) * fnc_GI_Degs_To_Rads(),
    Mode_S_Track_HDG_Rate = as.numeric(x$`I062/380/TAR/RateOfTurn`) * fnc_GI_Degs_To_Rads(),
    Mode_S_Roll_Angle = as.numeric(x$`I062/380/RAN/RAN`) * fnc_GI_Degs_To_Rads(),
    Mode_S_BPS = (as.numeric(x$`I062/380/BPS/BPS`) + 800) * fnc_GI_Mbar_To_Pa()
  )
  
  if (nrow(out) > 0) {
    message("[",Sys.time(),"] ", "Generating Flight_Plan_ID...")
    out <- generateFPID(out, dbi_con)
    
    message("[",Sys.time(),"] ", "Appending ", nrow(out), " rows to tbl_Radar_Track_Point...")
    dbWriteTable(dbi_con, "tbl_Radar_Track_Point", out, append = T)
    message("[",Sys.time(),"] ", "Successfully appended ", nrow(out), " rows to tbl_Radar_Track_Point")
  } else {
    message("[",Sys.time(),"] ", "Exited without change to database.")
  }
  
  
}
