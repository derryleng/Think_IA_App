# ----------------------------------------------------------------------- #
# Functions (General) -----------------------------------------------------
# ----------------------------------------------------------------------- #

# Usage: x %!in% y; this is equivalent to !(x %in% y)
'%!in%' <- function(x, y) !('%in%'(x, y))

# Evaluate a string (or multiple comma-separated strings) as R code
evalParse <- function(...) {
  return(eval(parse(text = paste0(..., collapse = ","))))
}

read_SQL_File <- function(filepath){
  # WARNING: R does not support reading UTF-16 (UCS-2 LE BOM) encoded files!
  con <- file(filepath, "r")
  sql.string <- ""
  while (T) {
    line <- readLines(con, n = 1)
    if (length(line) == 0) break
    if (grepl("^--.*$",line)) next
    sql.string <- paste(sql.string, gsub("\\t", " ", gsub("--.*$", "", line)))
  }
  close(con)
  return(sql.string)
}

Asterix_Filename_To_Date <- function(Log_Filename) {
  # This is a very specific function for filename strings containing yyyymmdd
  # If there are other blocks of >=8 numbers in filename this may get confused!
  if (grepl("^.*[2]{1}[0]{1}[0-9]{2}[0-1]{1}[0-9]{1}[0-3]{1}[0-9]{1}.*$", basename(Log_Filename))) {
    return(format(as.Date(gsub("^.*([2]{1}[0]{1}[0-9]{2}[0-1]{1}[0-9]{1}[0-3]{1}[0-9]{1}).*$", "\\1", basename(Log_Filename)), format = "%Y%m%d"), "%d/%m/%Y"))
  } else if (grepl("^TS[0-9]{1}.*[0-3]{1}[0-9]{1}[0-1]{1}[0-9]{1}[2]{1}[0-9]{1}.csv$", basename(Log_Filename))) {
    return(format(as.Date(gsub("^TS[0-9]{1}.*([0-3]{1}[0-9]{1}[0-1]{1}[0-9]{1}[2]{1}[0-9]{1}).csv$", "\\1", basename(Log_Filename)), format = "%d%m%y"), "%d/%m/%Y"))
  } else {
    stop("Unrecognised filename format - cannnot parse date!")
  }
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

Fusion_Filename_To_Time <- function(Log_Filename) {
  #Fusion filename to log start time, for fusion data spanning 2 days to ensure the correct track date is set upon loading
  LogStartTime <- substrRight(Log_Filename, 9)
  LogStartTime <- substr(LogStartTime, 1, 5)

  return(LogStartTime)
}

runway_Opposite_End <- function(rwy) {
  # Example
  # Input: c("36", "07", "21R", "R18L", "R27C", "R32R")
  # Output: c("18", "25", "03L", "R36R", "R09C", "R14L")
  rwy_split <- strsplit(gsub("^([R]{0,1})([0-9]{2})([A-Z]{0,1})$", "\\2 \\3 \\1", rwy), " ")
  new_rwy <- rep("", length(rwy_split))
  for (i in 1:length(rwy_split)) {

    rwy_hdg <- as.numeric(rwy_split[[i]][1])
    new_rwy[i] <- if (rwy_hdg > 18) {
      as.character(rwy_hdg - 18)
    } else {
      as.character(rwy_hdg + 18)
    }
    if (nchar(new_rwy[i]) == 1) {
      new_rwy[i] <- paste0("0", new_rwy[i])
    }

    if (rwy_split[[i]][2] == "L") {
      new_rwy[i] <- paste0(new_rwy[i], "R")
    } else if (rwy_split[[i]][2] == "R") {
      new_rwy[i] <- paste0(new_rwy[i], "L")
    } else {
      new_rwy[i] <- paste0(new_rwy[i], rwy_split[[i]][2])
    }

    if (!is.na(rwy_split[[i]][3])) {
      new_rwy[i] <- paste0("R", new_rwy[i])
    }

  }
  return(new_rwy)
}

# Based on Time_String_To_Seconds
Time_String_To_Milliseconds <- function(Time_String) {
  return(sapply(1:length(Time_String), function(i) {
    x_i <- unlist(strsplit(Time_String[i], ":"))
    return(sum(as.double(x_i) * c(3600000, 60000, 1000)))
  }))
}

generateFPID <- function(tracks, dbi_con = dbi_con, skip_leftover = F) {
  # dbi_con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};Server={192.168.1.23};Database={NavCan_Fusion_Test};Uid={vbuser};Pwd={Th!nkvbuser};")

  fp <- as.data.table(dbGetQuery(dbi_con, "SELECT DISTINCT Flight_Plan_ID, FP_Date, FP_Time, Callsign, SSR_Code FROM tbl_Flight_Plan"))

  # if (nrow(fp) > 0) {
  #
  #   if (nrow(tracks) > 0) {
  #
  #     if ("Flight_Plan_ID" %in% names(tracks)) tracks$Flight_Plan_ID <- NULL
  #
  #     fp$SSR_Code <- as.character(fp$SSR_Code)
  #     tracks$SSR_Code <- as.character(tracks$SSR_Code)
  #
  #     tracks$generateFPID_UID <- seq(1, nrow(tracks), 1)
  #
  #     # tracks_invalid <- tracks[is.na(Track_Date) | is.na(Track_Time) | is.na(Callsign) | is.na(SSR_Code)]
  #     # tracks_valid <- tracks[!is.na(Track_Date) & !is.na(Track_Time) & !is.na(Callsign) & !is.na(SSR_Code)]
  #
  #     tracks_proc <- tracks[fp, roll = "nearest", on = c(Track_Date = "FP_Date", Callsign = "Callsign", SSR_Code = "SSR_Code", Track_Time = "FP_Time")]
  #
  #     if (skip_leftover) {
  #       tracks_proc$generateFPID_UID <- NULL
  #       return(tracks_proc)
  #     } else {
  #       tracks_leftover <- tracks[generateFPID_UID %!in% tracks_proc$generateFPID_UID]
  #       tracks_leftover$Flight_Plan_ID <- NA
  #       tracks_combined <- rbind(tracks_proc, tracks_leftover)[order(generateFPID_UID)]
  #       tracks_combined$generateFPID_UID <- NULL
  #       return(tracks_combined)
  #     }
  #
  #   } else {
  #     message("[",Sys.time(),"] ", "Failed to generate Flight_Plan_ID - no rows in processed track data")
  #   }
  #
  # } else {
  #   message("[",Sys.time(),"] ", "Failed to generate Flight_Plan_ID - no rows in tbl_Flight_Plan")
  # }

  tracks$Flight_Plan_ID <- NULL
  tracks$Flight_Plan_ID <- character()

    for (j in unique(fp[FP_Date %in% unique(tracks$Track_Date)]$Flight_Plan_ID)) {
      tracks[
        Track_Date == fp[Flight_Plan_ID == j]$FP_Date &
          abs(Track_Time - fp[Flight_Plan_ID == j]$FP_Time) < 7200 &
          Callsign == fp[Flight_Plan_ID == j]$Callsign &
          grepl(paste0("^[0]?", fp[Flight_Plan_ID == j]$SSR_Code, "$"), SSR_Code)
      ]$Flight_Plan_ID <- j
    }

    return(tracks)

  }

generateFPID_fusion <- function(tracks, dbi_con = dbi_con, skip_leftover = F) {
  # dbi_con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};Server={192.168.1.23};Database={NavCan_Fusion_Test};Uid={vbuser};Pwd={Th!nkvbuser};")

  fp <- as.data.table(dbGetQuery(dbi_con, "SELECT DISTINCT Flight_Plan_ID, FP_Date, FP_Time, Callsign, SSR_Code FROM tbl_Flight_Plan"))

  # if (nrow(fp) > 0) {
  #
  #   if (nrow(tracks) > 0) {
  #
  #     if ("Flight_Plan_ID" %in% names(tracks)) tracks$Flight_Plan_ID <- NULL
  #
  #     fp$SSR_Code <- as.character(fp$SSR_Code)
  #     tracks$SSR_Code <- as.character(tracks$SSR_Code)
  #
  #     tracks$generateFPID_UID <- seq(1, nrow(tracks), 1)
  #
  #     # tracks_invalid <- tracks[is.na(Track_Date) | is.na(Track_Time) | is.na(Callsign) | is.na(SSR_Code)]
  #     # tracks_valid <- tracks[!is.na(Track_Date) & !is.na(Track_Time) & !is.na(Callsign) & !is.na(SSR_Code)]
  #
  #     tracks_proc <- tracks[fp, roll = "nearest", on = c(Track_Date = "FP_Date", Callsign = "Callsign", SSR_Code = "SSR_Code", Track_Time = "FP_Time")]
  #
  #     if (skip_leftover) {
  #       tracks_proc$generateFPID_UID <- NULL
  #       return(tracks_proc)
  #     } else {
  #       tracks_leftover <- tracks[generateFPID_UID %!in% tracks_proc$generateFPID_UID]
  #       tracks_leftover$Flight_Plan_ID <- NA
  #       tracks_combined <- rbind(tracks_proc, tracks_leftover)[order(generateFPID_UID)]
  #       tracks_combined$generateFPID_UID <- NULL
  #       return(tracks_combined)
  #     }
  #
  #   } else {
  #     message("[",Sys.time(),"] ", "Failed to generate Flight_Plan_ID - no rows in processed track data")
  #   }
  #
  # } else {
  #   message("[",Sys.time(),"] ", "Failed to generate Flight_Plan_ID - no rows in tbl_Flight_Plan")
  # }

  tracks$Flight_Plan_ID <- NULL
  tracks$Flight_Plan_ID <- character()

  #   for (j in unique(fp[FP_Date %in% unique(tracks$Track_Date)]$Flight_Plan_ID)) {
  #     tracks[
  #       Track_Date == fp[Flight_Plan_ID == j]$FP_Date &
  #         abs(Track_Time - fp[Flight_Plan_ID == j]$FP_Time) < 7200 &
  #         Callsign == fp[Flight_Plan_ID == j]$Callsign &
  #         grepl(paste0("^[0]?", fp[Flight_Plan_ID == j]$SSR_Code, "$"), SSR_Code)
  #     ]$Flight_Plan_ID <- j
  #   }
  #
  #   return(tracks)
  #
  # }

  tracks$SSR_Code <- as.numeric(tracks$SSR_Code)
  fp$SSR_Code <- as.numeric(fp$SSR_Code)

  tracks <- tracks[!is.na(Track_Date) & !is.na(SSR_Code) & !is.na(Track_Time)]
  tracks <- tracks[!is.na(Mode_C) & Mode_C != 99999999]

  #tracks_before_for_loop <- tracks

  for (i in unique(tracks[,paste(Track_Date, SSR_Code, Callsign, "-", Track_Number)])) {

    tracks_i <- tracks[paste(Track_Date, SSR_Code, Callsign, "-", Track_Number) == i]

    if (all(is.na(tracks_i$Mode_C)) | all(is.na(tracks_i[Mode_C != 99999999]$Mode_C))) {
      message(i, " - no Mode C data.")
      next
    }

    fpid <- if (is.na(tracks_i$Callsign[1]) & min(tracks_i$Mode_C, na.rm = T) <= 1000 & max(tracks_i[Mode_C != 99999999]$Mode_C, na.rm = T) >= 1000) { # min mode C <= 1000 as two identical SSR_codes at same time. Max Mode C >= 1000 as taxiing aircraft picked up
      fp[paste(FP_Date, SSR_Code, "NA") == strsplit(i, " -")[[1]][1] &
           abs(FP_Time - max(tracks_i$Track_Time, na.rm = T)) < 7200
      ]$Flight_Plan_ID[1]
    } else {
      fp[paste(FP_Date, SSR_Code, Callsign) == strsplit(i, " -")[[1]][1] &
           abs(FP_Time - max(tracks_i$Track_Time, na.rm = T)) < 7200
      ]$Flight_Plan_ID[1]
    }

    tracks[paste(Track_Date, SSR_Code, Callsign, "-", Track_Number) == i]$Flight_Plan_ID <- fpid
    #message(i, " ", fpid)
  }

  tracks$Flight_Plan_ID <- as.numeric(tracks$Flight_Plan_ID)
  tracks[is.na(Callsign)]$Callsign <- " "

  return(tracks)

}





XY_To_Heading <- function(vx, vy) {
  heading <- ifelse(
    vx >= 0 & vy >= 0, atan(abs(vx / vy)), ifelse(
      vx >= 0 & vy < 0, atan(abs(vy / vx)) + pi * 0.5, ifelse(
        vx < 0 & vy < 0, atan(abs(vx / vy)) + pi, ifelse(
          vx < 0 & vy >= 0, atan(abs(vy / vx)) + pi * 1.5, NA
        )
      )
    )
  )
  return(heading)
}

# For handling 90XX Leidos logging formats (used in TBS and CAV Log Loaders)
# Must have a "Date Time Message Type" line directly before each 90XX data line.
parse_log_lines <- function(raw_logs, log_type, col_names = NA) {

  logs <- rbindlist(lapply(grep(paste0("^", log_type, ", .*$"), raw_logs), function(i) {

    Log_Date <- as.character(as.Date(gsub("^([0-9]{2}-[0-9]{2}-[0-9]{2})[T ]{1}([0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3})  Message: [0-9]{4}$", "\\1", raw_logs[i-1]), "%y-%m-%d"))
    Log_Time <- Time_String_To_Seconds(gsub("^([0-9]{2}-[0-9]{2}-[0-9]{2})[T ]{1}([0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3})  Message: [0-9]{4}$", "\\2", raw_logs[i-1]))

    LogContents <- data.table(t(c(Log_Date, Log_Time, unlist(strsplit(raw_logs[i], split = "\\s{0,},\\s{0,}")))))

    if (!is.na(col_names) & ncol(LogContents) > 0) {

      if (length(col_names) < ncol(LogContents)) {
        message("WARNING: Not enough column names supplied for parse_log_lines: ", log_type, " - expected ", ncol(LogContents), " but received ", length(col_names), ".")
        names(LogContents)[1:length(col_names)] <- col_names
      } else if (length(col_names) == ncol(LogContents)) {
        names(LogContents) <- col_names
      } else if (length(col_names) > ncol(LogContents)) {
        # message("WARNING: Too many column names supplied for parse_log_lines: ", log_type, " - expected ", ncol(LogContents), " but received ", length(col_names), ".")
        names(LogContents) <- col_names[1:length(names(LogContents))]
      }

    }

    return(LogContents)

  }), use.names = T, fill = T)

  return(logs)

}

# Required to extend DT filter ability on character columns
factoriseCharCols <- function(df) {
  for (i in which(sapply(df, class) == "character")) df[[i]] = as.factor(df[[i]])
  return(df)
}

# Converts the Start/End Dist of Threshold and Lateral Dist Left/Right to
# polygon point sequence as contained in the database.
configVolume_To_pointSequence <- function(x, dbi_con) {
  # x: Config file 05_Populate_Airspace_Volumes.csv as data.table
  # dbi_con: database connection

  Airfield_Name <- as.vector(unlist(dbGetQuery(dbi_con, "SELECT * FROM tbl_Airfield")$Airfield_Name))
  tbl_Runway <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Runway"))
  tbl_Adaptation_Data <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Adaptation_Data"))

  polygons <- data.table()

  for (i in 1:nrow(x)) {

    polygons_i <- data.table(
      Volume_Name = rep(x$Volume_Name[i], 5),
      Airfield_Name = rep(Airfield_Name, 5),
      Point_Sequence = 1:5,
      Point_X = as.numeric(c(x$Start_Dist_From_Threshold[i], x$Start_Dist_From_Threshold[i], x$End_Dist_From_Threshold[i], x$End_Dist_From_Threshold[i], x$Start_Dist_From_Threshold[i])) * fnc_GI_Nm_To_M(),
      Point_Y = as.numeric(c(x$Lateral_Dist_Left[i], x$Lateral_Dist_Right[i], x$Lateral_Dist_Right[i], x$Lateral_Dist_Left[i], x$Lateral_Dist_Left[i])) * fnc_GI_Nm_To_M()
    )

    grid_i <- usp_GI_Runway_To_XY(
      gsub("^([A-Z0-9]{1,})_.*$", "R\\1", polygons_i$Volume_Name[1]),
      polygons_i$Point_X,
      polygons_i$Point_Y,
      tbl_Runway
    )

    updated_i <- usp_GI_Latlong_From_XY(
      grid_i$Node_X,
      grid_i$Node_Y,
      tbl_Adaptation_Data
    )

    polygons_i$Point_X <- grid_i$Node_X
    polygons_i$Point_Y <- grid_i$Node_Y
    polygons_i$Latitude <- updated_i$PositionLatitude
    polygons_i$Longitude <- updated_i$PositionLongitude

    polygons <- rbind(
      polygons,
      polygons_i
    )

  }

  return(polygons)

}

# ----------------------------------------------------------------------- #
# Functions (Imported) ----------------------------------------------------
# ----------------------------------------------------------------------- #

# Convert time string(s) (hh:mm:ss.sss OR hh:mm.mmm) to seconds after midnight.
Time_String_To_Seconds <- function(Time_String) {
  return(sapply(1:length(Time_String), function(i) {
    x_i <- unlist(strsplit(Time_String[i], ":"))
    return(sum(as.numeric(x_i) * c(3600, 60, 1)))
  }))
}

# Convert seconds to days, hours, minutes, seconds
Time_String_From_Seconds <- function(t) {
  t_days <- t %/% (60*60*24)
  t_hours <- formatC(t %/% (60*60) %% 24, width = 2, format = "d", flag = "0")
  t_minutes <- formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0")
  t_seconds <- formatC(t %% 60, width = 2, format = "d", flag = "0") # Add milliseconds! (For CAV)
  if (t_days > 0) {
    return(paste0(t_days, " ", t_hours, ":", t_minutes, ":", t_seconds))
  } else {
    return(paste0(t_hours, ":", t_minutes, ":", t_seconds))
  }
}

fnc_GI_Nm_To_M <- function() return(1852)

fnc_GI_Kts_To_M_Per_Sec <- function() return(fnc_GI_Nm_To_M() / 3600)

fnc_GI_Ft_To_M <- function() return(0.3048)

fnc_GI_Ft_Per_Min_To_M_Per_Sec <- function() return(fnc_GI_Ft_To_M() / 60)

fnc_GI_Mbar_To_Pa <- function() return(100)

fnc_GI_Degs_To_Rads <- function() return(pi / 180)

fnc_GI_Latlong_String_To_Degrees <- function(x) {
  # x is a string/vector of strings in either format below:
  if (all(nchar(x) == 10)) { # latitude format: ddmmss.hhN
    return(
      as.numeric(substr(x, 1, 2)) +
        as.numeric(substr(x, 3, 4)) / 60 +
        as.numeric(substr(x, 5, 6)) / 3600 +
        as.numeric(substr(x, 8, 9)) / 360000) *
      ifelse(substr(x, 10, 10) == "S", -1, 1
      )
  } else if (all(nchar(x) == 11)) { # longitude format: dddmmss.hhW
    return(
      as.numeric(substr(x, 1, 3)) +
        as.numeric(substr(x, 4, 5)) / 60 +
        as.numeric(substr(x, 6, 7)) / 3600 +
        as.numeric(substr(x, 9, 10)) / 360000) *
      ifelse(substr(x, 11, 11) == "W", -1, 1
      )
  } else { # Error
    return(0)
  }
}

fnc_GI_Latlong_String_To_Radians <- function(x) {
  # x is a string/vector of strings in either format below:
  if (all(nchar(x) == 10)) { # latitude format: ddmmss.hhN
    return(
      (as.numeric(substr(x, 1, 2)) +
         as.numeric(substr(x, 3, 4)) / 60 +
         as.numeric(substr(x, 5, 6)) / 3600 +
         as.numeric(substr(x, 8, 9)) / 360000) *
        ifelse(substr(x, 10, 10) == "S", -1, 1) / 180 * pi
    )
  } else if (all(nchar(x) == 11)) { # longitude format: dddmmss.hhW
    return(
      (as.numeric(substr(x, 1, 3)) +
         as.numeric(substr(x, 4, 5)) / 60 +
         as.numeric(substr(x, 6, 7)) / 3600 +
         as.numeric(substr(x, 9, 10)) / 360000) *
        ifelse(substr(x, 11, 11) == "W", -1, 1) / 180 * pi
    )
  } else { # Error
    return(0)
  }
}

# Calculate the angle component of vector from its X/Y cartesian representation.
fnc_GI_To_Vector_Angle <- function(Vector_X, Vector_Y) {
  return(ifelse(
    Vector_Y == 0 & Vector_X == 0, 0, ifelse(
      Vector_Y == 0 & Vector_X > 0, 0.5 * pi, ifelse(
        Vector_Y == 0 & Vector_X < 0, 1.5 * pi, ifelse(
          Vector_Y > 0 & Vector_X == 0, 0, ifelse(
            Vector_Y < 0 & Vector_X == 0, pi, ifelse(
              Vector_Y > 0 & Vector_X > 0, atan(abs(Vector_X/Vector_Y)), ifelse(
                Vector_Y < 0 & Vector_X > 0, atan(abs(Vector_Y/Vector_X)) + 0.5 * pi, ifelse(
                  Vector_Y < 0 & Vector_X < 0, atan(abs(Vector_X/Vector_Y))  + pi, ifelse(
                    Vector_Y > 0 & Vector_X < 0, atan(abs(Vector_Y/Vector_X)) + 1.5 * pi, NA
                  )
                )
              )
            )
          )
        )
      )
    )
  ))
}

fnc_GI_To_Vector_X <- function(Vector_Amplitude, Vector_Angle) {
  return(Vector_Amplitude * sin(Vector_Angle))
}

fnc_GI_To_Vector_Y <- function(Vector_Amplitude, Vector_Angle) {
  return(Vector_Amplitude * cos(Vector_Angle))
}

# ----------------------------------------------------------------------- #
# Procedures (Imported) ---------------------------------------------------
# ----------------------------------------------------------------------- #

# usp_GI_Latlong_From_XY and usp_GI_Latlong_To_XY
#
# Synopsis:
#   For a position supplied in cartesian representation relative to an
# origin latitude and longitude position, this procedure calculates
# the position as a latitude and longitude.
#
#   Stereographic from OGP (International Association og Oil and Gas Producers)
# using local grid origin and WGS84 Earth.
# Stereographic relates to a projection from the centre of the sphere onto a
# planar surface tangential at the origin.
# The calculations first calculate the conformal lat/long of the origin and the
# point, from the supplied geodetic lat/long using WGS84 Earth spheroid parameters.
# The reverse formula to compute the geodetic coordinates from the grid coordinates
# involves computing the conformal values, then the isomeric latitude and finally the
# geodetic values.
# Details of the maths and reference are included at the bottom of the file.
#
# REF: http://ftp.stu.edu.tw/BSD/NetBSD/pkgsrc/distfiles/epsg-6.11/G7-2.pdf
# (OGP Surveying and Positioning Guidance Note Number 7, Part 2 - August 2006
#   Coordinate Conversion and Transformations including Formulas, Page 42)

usp_GI_Latlong_From_XY <- function(Position_X, Position_Y, tbl_Adaptation_Data) {

  # False Easting / Northings and scaling
  FN <- tbl_Adaptation_Data$Grid_Offset_Y[1]
  FE <- tbl_Adaptation_Data$Grid_Offset_X[1]
  K_0 <- 1.0

  # Geodetic origin latitude / longitude
  Phi_0 <- tbl_Adaptation_Data$Grid_Projection_Origin_Lat[1]
  Lamda_0 <- tbl_Adaptation_Data$Grid_Projection_Origin_Lon[1]

  # Semi_Major_Axis (metres)
  a <- 6378137.0

  # Semi_Minor_Axis (metres)
  b <- 6356752.0

  # Flattening
  f <- (a - b) / a

  # Eccentricity
  e2 <- 2 * f - f^2
  e <- sqrt(e2)

  # Radius of curvature in the meridian
  Rho_0 <- a * (1 - e2) / (1 - e2 * sin(Phi_0)^2)^1.5

  # Radius of curvature in the prime vertical
  Nu_0 <- a / (1 - e2 * sin(Phi_0)^2)^0.5

  # Conformal sphere parameters
  R <- (Rho_0 * Nu_0)^0.5
  n <- (1 + (e2 * cos(Phi_0)^4 / (1 - e2)))^0.5

  # Intermediates for conformal origin
  S1 <- (1 + sin(Phi_0)) / (1 - sin(Phi_0))
  S2 <- (1 - e * sin(Phi_0)) / (1 + e * sin(Phi_0))
  w1 <- (S1 * S2^e)^n
  c <- (n + sin(Phi_0)) * (1 - (w1 - 1) / (w1 + 1)) / ((n - sin(Phi_0)) * (1 + (w1 - 1) / (w1 + 1)))
  w2 <- c * w1

  # Conformal origin latitude / longitude
  Chi_0 <- asin((w2 - 1) / (w2 + 1))
  Delta_0 = Lamda_0

  # Intermediates
  g <- 2 * R * K_0 * tan(pi / 4 - Chi_0 / 2)
  h <- 4 * R * K_0 * tan(Chi_0) + g
  i <- atan((Position_X - FE) / (h + (Position_Y - FN)))
  j <- atan((Position_X - FE) / (g - (Position_Y - FN))) - i

  # Conformal point latitude.
  Chi <- Chi_0 + 2 * atan( ((Position_Y - FN) - (Position_X - FE) * tan(j / 2)) / (2 * R * K_0) )

  # Conformal point longitude.
  Delta <- j + (2 * i) + Delta_0

  # Geodetic longitude.
  Lamda <- (Delta - Delta_0) / n + Delta_0

  # Isomeric latitude.
  Psi <- 0.5 * log( (1 + sin(Chi)) / (c * (1 - sin(Chi))) ) / n

  # First approximation
  Phi_1 <- 2 * atan( exp(Psi) ) - pi / 2

  # Iterate until error in Phi is sufficiently small.
  # Psi_i is the isometric latutude at Phi_i.

  Phi_i <- pi  # An impossible latitude

  Phi_i_Plus1 <- Phi_1  # To get the loop going

  Latitude_Accuracy <- (0.0001 / 180.0) * pi  # About 10m

  # i is effectively 1 first time through.
  for (k in 1:length(i)) {
    Count <- 10  # Count down to make sure we escape the loop!
    while (abs(Phi_i_Plus1[k] - Phi_i) > Latitude_Accuracy & Count >= 0) {
      Phi_i <- Phi_i_Plus1[k]

      Psi_i <- log( tan(Phi_i / 2 + pi / 4) * ((1 - e * sin(Phi_i)) / (1 + e * sin(Phi_i)))^(e / 2) )

      Phi_i_Plus1[k] <- Phi_i - (Psi_i - Psi[k]) * cos(Phi_i) * (1 - e^2 * sin(Phi_i)^2) / (1 - e^2)

      Count <- Count - 1
    }
  }

  # Set function output parameters.
  return(data.table(
    PositionLatitude = Phi_i_Plus1,
    PositionLongitude = Lamda
  ))

}

usp_GI_Latlong_To_XY <- function(PositionLatitude, PositionLongitude, tbl_Adaptation_Data) {

  # Copy inputs for naming convenience.
  Phi <- PositionLatitude
  Lamda <- PositionLongitude

  # Set up False Easting / Northings and scaling as required.
  FN <- tbl_Adaptation_Data$Grid_Offset_Y[1]
  FE <- tbl_Adaptation_Data$Grid_Offset_X[1]
  K_0 <- 1.0

  # Set up origin as required (R27L threshold for local coords).
  # Geodetic origin latitude / longitude.
  Phi_0 <- tbl_Adaptation_Data$Grid_Projection_Origin_Lat[1]
  Lamda_0 <- tbl_Adaptation_Data$Grid_Projection_Origin_Lon[1]

  # WGS84 Earth spheroid parameters.

  # Semi_Major_Axis (metres)
  a <- 6378137.0

  # Semi_Minor_Axis (metres)
  b <- 6356752.0

  # Flattening.
  f <- (a - b) / a

  # Eccentricity.
  e2 <- 2 * f - f^2
  e <- sqrt(e2)

  # Radius of curvature in the meridian.
  Rho_0 <- a * (1 - e2) / (1 - e2 * sin(Phi_0)^2)^1.5

  # Radius of curvature in the prime vertical.
  Nu_0 <- a / (1 - e2 * sin(Phi_0)^2)^0.5

  # Conformal sphere parameters.
  R <- (Rho_0 * Nu_0)^0.5

  n <- (1 + (e2 * cos(Phi_0)^4 / (1 - e2)))^0.5

  # Intermediates for conformal origin.
  S1 <- (1 + sin(Phi_0)) / (1 - sin(Phi_0))

  S2 <- (1 - e * sin(Phi_0)) / (1 + e * sin(Phi_0))

  w1 <- (S1 * S2^e)^n

  c <- (n + sin(Phi_0)) * (1 - (w1 - 1) / (w1 + 1)) / ((n - sin(Phi_0)) * (1 + (w1 - 1) / (w1 + 1)))

  w2 <- c * w1

  # Conformal origin latitude.
  Chi_0 <- asin((w2 - 1) / (w2 + 1))

  # Conformal origin longitude.
  Delta_0 <- Lamda_0

  # Intermediates for conformal point.
  Sa <- (1 + sin(Phi)) / (1 - sin(Phi))

  Sb <- (1 - e * sin(Phi)) / (1 + e * sin(Phi))

  w <- c * (Sa * Sb^e)^n

  # Conformal point latitude.
  Chi <- asin((w - 1) / (w + 1))

  # Conformal point longitude.
  Delta <- n * (Lamda - Delta_0) + Delta_0

  # Intermediates for final grid calcs.
  B_Cap <- (1 + sin(Chi) * sin(Chi_0) + cos(Chi) * cos(Chi_0) * cos(Delta - Delta_0))

  # Eastings (X) and Northings (Y)
  return(data.table(
    Position_X = FE + 2 * R * K_0 * cos(Chi) * sin(Delta - Delta_0) / B_Cap,
    Position_Y = FN + 2 * R * K_0 * (sin(Chi) * cos(Chi_0) - cos(Chi) * sin(Chi_0) * cos(Delta - Delta_0)) / B_Cap
  ))

}

# Synopsis:	This procedure calculates NODE X/Y coordinates from runway
# relative coordinates.
#
# Runway relative coordinates are X/Y coordinates in metres, but relative to
# runway threshold for the specified runway.  The conversion involves a
# rotation to the axis direction of the centre-line and a translation to the
# NODE X/Y coordinates of the runway threshold.

usp_GI_Runway_To_XY <- function(Runway_Nom, Runway_X, Runway_Y, tbl_Runway) {

  # Get the runway threshold data.
  Runway_Threshold_X_Pos <- tbl_Runway[Runway_Name == Runway_Nom]$Threshold_X_Pos
  Runway_Threshold_Y_Pos <- tbl_Runway[Runway_Name == Runway_Nom]$Threshold_Y_Pos

  # We want to rotate the coordinate system by the runway heading offset in the Node
  # system, which is NOT the true runway heading!  This is calculated using the x/y
  # position of the two threshold coordinates at the two ends of the tarmac.
  Theta <- tbl_Runway[Runway_Name == Runway_Nom]$NODE_Heading_Offset

  # Calculate the coordinates.
  Node_X = (Runway_X * cos(Theta) + Runway_Y * sin(Theta)) + Runway_Threshold_X_Pos
  Node_Y = (-Runway_X * sin(Theta) + Runway_Y * cos(Theta)) + Runway_Threshold_Y_Pos

  return(data.table(Node_X = Node_X, Node_Y = Node_Y))
}

# ----------------------------------------------------------------------- #
# Database Functions ------------------------------------------------------
# ----------------------------------------------------------------------- #

# Database connection pop-up dialogue
connection_dialogue <- function() {
  modalDialog(
    div(
      class = "centered",
      style = "margin: -6px",
      h4("Connect to a Database")
    ),
    div(
      style = "display: none;",
      selectizeInput("db_driver", "Driver", db_defaults$driver, options = list(create = T), width = "100%")
    ),
    div(
      style = "margin-bottom: -15px",
      selectizeInput("db_server", "Server", db_defaults$server, options = list(create = T), width = "100%")
    ),
    div(
      style = "margin-bottom: -10px",
      selectizeInput("db_username", "Username", db_defaults$username, options = list(create = T), width = "100%")
    ),
    div(
      style = "margin-bottom: -10px",
      passwordInput("db_password", "Password", db_defaults$password, width = "100%")
    ),
    div(
      style = "margin-bottom: -5px",
      selectizeInput("db_database", "Database", db_defaults$database, options = list(create = T), width = "100%")
    ),
    div(
      class = "centered",
      modalButton("Cancel"),
      div(style = "width: 15px"),
      actionButton("db_refresh_list", "DB List", icon("sync")),
      div(style = "width: 15px"),
      actionButton("db_connect", "Connect", icon("database"))
    ),
    div(style = "height: 5px"),
    uiOutput("db_status"),
    size = "s",
    footer = NULL,
    easyClose = F
  )
}

# ----------------------------------------------------------------------- #
# UI Wrapper Functions ----------------------------------------------------
# ----------------------------------------------------------------------- #

# Used to make buttons on the header bar
headerButtonUI <- function(id, icon_str) {
  tags$li(
    class = "dropdown header_button",
    id = id,
    icon(icon_str)
  )
}

sidebarTabUI <- function(id, text_str, icon_str) {
  menuItem(
    text = text_str,
    tabName = id,
    icon = icon(icon_str)
  )
}

tabContentUI <- function(id, ...) {
  ns <- NS(id)
  tabItem(tabName = id, ...)
}

# pickerInput function with customised styling and action boxes
pickerInput_customised <- function(
  inputId,
  label = NULL,
  choices = NULL,
  selected = NULL,
  multiple = T,
  options = list(`actions-box` = T, `live-search` = T),
  choicesOpt = NULL,
  width = "220px",
  inline = F,
  ...
) {
  pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    options = options,
    choicesOpt = choicesOpt,
    width = width,
    inline = inline,
    ...
  )
}

# datatable function with customised styling
datatable_customised_1 <- function(
  data,
  rownames = F,
  selection = "none",
  style = "bootstrap4",
  options = list(
    pageLength = 15,
    lengthMenu = seq(5, 100, 5),
    columnDefs = list(list(className = 'dt-center', targets = "_all")),
    scrollX = T,
    dom = '<"dataTables_row"lf>rt<"dataTables_row"ip>'
  ),
  ...
){
  datatable(
    data = data,
    rownames = rownames,
    selection = selection,
    style = style,
    options = options,
    ...
  )
}

# datatable function with customised styling and download buttons
datatable_customised_2 <- function(
  data,
  rownames = F,
  selection = "none",
  style = "bootstrap4",
  options = list(
    pageLength = 15,
    lengthMenu = seq(5, 100, 5),
    columnDefs = list(list(className = 'dt-center', targets = "_all")),
    scrollX = T,
    dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>',
    buttons = c('copy', 'csv', 'excel')
  ),
  extensions = c("Buttons"),
  ...
){
  datatable(
    data = data,
    rownames = rownames,
    selection = selection,
    style = style,
    options = options,
    extensions = extensions,
    ...
  )
}

# ----------------------------------------------------------------------- #
# XML Functions -----------------------------------------------------------
# ----------------------------------------------------------------------- #

XML_Tag_Open <- function(tag_name, tag_attr = NA) {
  if (all(is.na(tag_attr))) {
    return(paste0("<", tag_name, ">"))
  } else {
    attrs <- sapply(1:length(tag_attr), function(i) {
      if (is.numeric(tag_attr[i])) {
        paste0(names(tag_attr)[i], "=", tag_attr[i])
      } else {
        paste0(names(tag_attr)[i], "=\"", tag_attr[i], "\"")
      }
    })
    return(paste0("<", tag_name, " ", paste(attrs, collapse = ", "), ">"))
  }
}

XML_Tag_Close <- function(tag_name) {
  return(paste0("</", tag_name, ">"))
}

# Convert list type to vector of XML strings
List_To_XML <- function(x, indent = 0, out_vec = c()) {

  x_names <- names(x)
  i <- 1

  while (i <= length(x)) {

    attrs_list <- NA
    skip_next <- F

    if (i < length(x)) {
      if (x_names[i + 1] == paste0(x_names[i], ".attr")) {
        attrs_list <- x[[i+1]]
        skip_next <- T
      }
    }

    if (is.numeric(x[[i]]) | is.character(x[[i]])) {
      out_vec <- c(out_vec, paste0(
        paste(rep("    ", indent), collapse = ""),
        XML_Tag_Open(x_names[i], attrs_list),
        x[[i]],
        XML_Tag_Close(x_names[i])
      ))
    } else if (is.list(x[[i]])) {
      out_vec <- c(out_vec, paste0(paste(rep("    ", indent), collapse = ""), XML_Tag_Open(x_names[i], attrs_list)))
      out_vec <- List_To_XML(x[[i]], indent = indent + 1, out_vec = out_vec)
      out_vec <- c(out_vec, paste0(paste(rep("    ", indent), collapse = ""), XML_Tag_Close(x_names[i])))
    } else {
      warning(paste0("Invalid data type at ", x_names[i]))
    }

    if (skip_next) {
      i <- i + 2
    } else {
      i <- i + 1
    }

  }

  return(out_vec)

}


######################################################################################################################
# Merges from George's Global Functions.R
######################################################################################################################

# ----------------------------------------------- #
# 0.2.1. COPY FOR RSTUDIO/SHINY COMPATIBILITY
# ----------------------------------------------- #
# This block sets the Global & Working directories
# ----------------------------------------------- #

# # --------------------------------------------------------------------------- #
# ModuleFolder <- "<MODUEL FOLDER IN THINK IA APP"
# ModuleSubfolder <- "<SUBFOLDER IN THINK IA APP>"
# OutputFolder <- "<ALGORITHM FOLDER ON DROPBOX>"
# # --------------------------------------------------------------------------- #
#
# FileFlag <- c("global.R", "GlobalPlaceholder.txt")[1]
# ResourcesFolder <- c("resources", "GlobalFunctionsPlaceholder")[1]
# AlgoResourcesFolder <- c("algorithm_functions", "AlgoFunctionsPlaceholder")[1]
# ModulesFolder <- c("modules", "ModulesPlaceholder")[1]
#
# if (rstudioapi::isAvailable()) {
#   setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#   Base_Dir <- getwd()
#   Global_Dir <- Base_Dir
#   Script_Dir <- file.path(Base_Dir)
#   while (!file.exists(file.path(Global_Dir, FileFlag))){
#     Global_Dir <- file.path(Global_Dir, "..")
#   }
# } else {
#   Global_Dir <- getwd()
#   Script_Dir <- file.path(Global_Dir, ModulesFolder, ModuleFolder, ModuleSubfolder)
# }
#
# Global_Dir <- file.path(Global_Dir, ResourcesFolder)
# Algo_Func_Dir <- file.path(Global_Dir, AlgoResourcesFolder)
#
# # Global Functions, imports & parameters
# source(file.path(Global_Dir, "Imports.R"), local = F)
# source(file.path(Global_Dir, "unit conversions.R"), local = F)
# source(file.path(Global_Dir, "functions.R"), local = F)
#
# Define Project with a numeric value
# Base_Dir <- GetSaveDirectory(Project = project, Algorithm =  OutputFolder, IorO = "Outputs")
# or leave blank and the function will request the value from you
# Base_Dir <- GetSaveDirectory(Algorithm =  OutputFolder, IorO = "Outputs")
# Create_Directory(Base_Dir)

# Function to Source Files based on Directory Precedence System.
GetScriptPath <- function(ScriptDirectory, Airfield, FileName){
  if (file.exists(file.path(ScriptDirectory, Airfield, FileName))){
    message(paste0(FileName, " running from ", Airfield, " script directory."))
    return(file.path(ScriptDirectory, Airfield, FileName))
  } else if (file.exists(file.path(ScriptDirectory, FileName))){
    message(paste0(FileName, " running from global script directory."))
    return(file.path(ScriptDirectory, FileName))
  } else {
    message(paste0("ERROR: ", FileName, " not found. Check the directory/spelling."))
  }
}

GetProjectID <- function(){
  ID <- as.numeric(getPass(msg = "Choose a Project: NAV TBS = 1,  IA LVNL = 2, Heathrow PWS = 3, NODE Replacement = 4", noblank = FALSE, forcemask = FALSE))
  return(ID)
  }

GetSaveDirectory <- function(Project, Algorithm, IorO){

  if(missing(Project)){Project <- as.numeric(getPass(msg = "Choose a Project: NAV TBS = 1,  IA LVNL = 2, Heathrow PWS = 3, NODE Replacement = 4", noblank = FALSE, forcemask = FALSE))}

#Add new project direcotries here later, PWS, NODE, etc
  if (Project == 1){
    Dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Think Research)", "NATS Projects", "NATS NavCanada TBS", "23 Data Analysis")
    # Dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Think Research)", "NATS Projects", "NATS NavCanada TBS", "Data Analysis")
  }

  if (Project == 4){
    Dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Think Research)", "NATS Projects", "NATS Node Replacement", "Data Analysis")
  }

  # Go into Algorithm Folder
  # IorO as "Inputs" or "Outputs" as a string
  Dir <- file.path(Dir, IorO, Algorithm)

  return(Dir)

}


# ----------------------------------------------- #
# 0.2.1. Config Functions
# ----------------------------------------------- #
# SQL Access, Basic Manipulation, Directory Config
# ----------------------------------------------- #

# the "not in" function
"%!in%" <- function(x,y) !("%in%"(x,y))

# Get Database Connection (RODBC)
Get_RODBC_Database_Connection <- function(IP, Database){
  User <- getPass(msg = "Username: ", noblank = FALSE, forcemask = FALSE)
  Pass <- getPass(msg = "Password: ", noblank = FALSE, forcemask = TRUE)
  con <- RODBC::odbcDriverConnect(connection=paste0("Driver={SQL Server};
                                  Server={",IP,"};Database={", Database, "};
                                  Uid={",User,"};Pwd={",Pass,"};"))
  return(con)
}

# Get Database Connection (DBI, odbc)
Get_DBI_Connection <- function(IP, Database){
  User <- getPass(msg = "Username: ", noblank = FALSE, forcemask = FALSE)
  Pass <- getPass(msg = "Password: ", noblank = FALSE, forcemask = TRUE)

  dbi_con <- dbConnect(odbc::odbc(),
                       .connection_string = paste0("Driver={SQL Server};
                                                    Server={",IP,"};
                                                    Database={", Database, "};
                                                    Uid={",User,"};
                                                    Pwd={",Pass,"};"))
  return(dbi_con)
}

# Function to Load in Adaptation. Currently use RODBC. If we change package then all adaptation loads occur here.
Load_Adaptation_Table <- function(con, Table_Name){
  Table <- dbGetQuery(con, paste0("SELECT * FROM ", Table_Name), stringsAsFactors = F)
  return(Table)
}


Load_CSV_Data <- function(con, Name, Query, Version_In_Name, Airfield_Dir, Local_Only, Type, Directory, Working_Version, File_Version){

  if (Type == "Input"){Initial_Dir <- file.path(Airfield_Dir, "Inputs")}
  if (Type == "Output"){Initial_Dir <- file.path(Airfield_Dir, "Outputs")}
  Initial_Dir <- Airfield_Dir

  Version_1_Split <- unlist(strsplit(Working_Version, "-"))
  Version_2_Split <- unlist(strsplit(File_Version, "-"))

  Dir_Current <- file.path(Initial_Dir, paste0("v", Version_1_Split[1]))
  Dir_Alternate <- file.path(Initial_Dir, paste0("v", Version_2_Split[1]))

  if (length(Version_1_Split) > 1){
    Dir_Current <- file.path(Dir_Current, paste0("v", Version_1_Split[1], "-", Version_1_Split[2]))
    Dir_Alternate <- file.path(Dir_Alternate, paste0("v", Version_2_Split[1], "-", Version_2_Split[2]))
  }

  if (length(Version_1_Split) > 2){
    Dir_Current <- file.path(Dir_Current, paste0("v", Version_1_Split[1], "-", Version_1_Split[2], "-", Version_1_Split[3]))
    Dir_Alternate <- file.path(Dir_Alternate, paste0("v", Version_2_Split[1], "-", Version_2_Split[2], "-", Version_2_Split[3]))
  }

  if (Version_In_Name){FileName <- paste0(Name, " v", File_Version, ".csv")} else {FileName <- paste0(Name, ".csv")}
  if (!is.na(Directory)){
    Dir_Current <- file.path(Dir_Current, Directory)
    Dir_Alternate <- file.path(Dir_Alternate, Directory)
  }

  Dir_Current <- file.path(Dir_Current, FileName)
  Dir_Alternate <- file.path(Dir_Alternate, FileName)

  if (file.exists(Dir_Alternate)){
    File <- fread(Dir_Alternate)
    return(File)}
  else if (file.exists(Dir_Current)){
    File <- fread(Dir_Current)
    return(File)}
  else if (!Local_Only & Working_Version == File_Version){
    File <- dbGetQuery(con, Query, stringsAsFactors = F)
    if(class(File) == "data.frame"){ fwrite(File, Dir_Current) }
    return(File)
  }
  else {
    message(paste0("Error: File not Found."))
    return(NA)
  }


}

Load_Generic_DB_Query <- function(con, Name, Query, Version_In_Name, Airfield_Dir, Directory, Working_Version, File_Version){
  File <- Load_CSV_Data(con, Name, Query, Version_In_Name, Airfield_Dir, Local_Only = F, Type = "Input", Directory, Working_Version, File_Version)
  return(File)
}

Load_DB_Input_Data <- function(con, Name, Airfield_Dir, Working_Version, File_Version){
  Query <- paste0("SELECT * FROM ", Name)
  Directory <- file.path("Input Data")
  File <- Load_CSV_Data(con, Name, Query, Version_In_Name = T, Airfield_Dir, Local_Only = F, Type = "Input", Directory, Working_Version, File_Version)
  return(File)
}

Load_Local_Adaptation_Data <- function(Airfield_Dir, Version){
  Directory <- NA
  Name1 <- "Local Adaptation"
  Name2 <- "Local Adaptation Extension"
  File1 <- Load_CSV_Data(con = NA, Name1, Query = NA, Version_In_Name = T, Airfield_Dir, Local_Only = T, Type = "Input", Directory, Version, Version)
  File2 <- Load_CSV_Data(con = NA, Name2, Query = NA, Version_In_Name = T, Airfield_Dir, Local_Only = T, Type = "Input", Directory, Version, Version)
  File <- rbind(File1, File2)
  return(File)
}

Load_Output_Data <- function(Name, Airfield_Dir, Directory, Version){
  File <- Load_CSV_Data(con = NA, Name, Query = NA, Version_In_Name = T, Airfield_Dir, Local_Only = T, Type = "Output", Directory, Version, Version)
  return(File)
}

Load_Reference_Data <- function(con, Name, Airfield_Dir, Version){
  Query = paste0("SELECT * FROM ", Name)
  Directory <- file.path("Reference Data")
  File <- Load_CSV_Data(con, Name, Query, Version_In_Name = F, Airfield_Dir, Local_Only = F, Type = "Input", Directory, Version, Version)
  return(File)
}

Create_Directory <- function(FilePath){
  if (!dir.exists(FilePath)) dir.create(FilePath)
}

Save_Adaptation_Table <- function(File, Name, Airfield, Dir){
  FileName <- paste0("Populate_", Name, "_", Airfield, ".csv")
  fwrite(File, file.path(Dir, FileName))
}


# ----------------------------------------------- #
# 0.2.1. Date/Time Functions
# ----------------------------------------------- #
# Functions to manipulate time/date data.
# ----------------------------------------------- #

# ----------------------------------------------- #
# 0.2.1.1. Convert Time string to Seconds.
# NOTE: ms currently not supported
# - This assumes the format of hh:mm:ss or h:mm:ss
# - The function detects how many hour digits are used
# - It uses this to extract hours, minutes and seconds
# - These are converted to seconds and cumulated

Convert_Time_String_to_Seconds <- function(time_string){
  hour_length <- ifelse(substr(time_string, 3, 3) == ":", 2, 1)
  hours <- as.numeric(substr(time_string, 1, hour_length))
  mins <- as.numeric(substr(time_string, hour_length+2, hour_length+3))
  secs <- as.numeric(substr(time_string, hour_length+5, hour_length+6))
  total_secs <- (hours * h_to_s) + (mins * min_to_s) + secs
  return(total_secs)
}

# ----------------------------------------------- #
# 0.2.1.2. Convert Seconds to Time String.
# NOTE: s/ms currently not functioning for dplyr use
# - This converts seconds into a hh:mm:ss.xxxx format
# - It calculates the hours, minutes, seconds in numeric
# - It then transforms them to characters
# - If these aren't 2 digit a 0 is added on the front
# - They are then pasted to form a time string

Convert_Seconds_to_Time_String <- function(total_secs){
  total_secs <- total_secs %% d_to_s
  hours <- floor(total_secs / h_to_s)
  mins <- floor((total_secs - (hours * h_to_s)) / min_to_s)
  secs <- total_secs - ((hours * h_to_s) + (mins * min_to_s))
  hours <- as.character(hours)
  mins <- as.character(mins)
  secs <- as.character(round(secs, 6))
  hour_string <- ifelse(nchar(hours) == 1, paste0("0", hours), hours)
  mins_string <- ifelse(nchar(mins) == 1, paste0("0", mins), mins)
  secs_string <- ifelse(nchar(secs) == 1, paste0("0", secs), secs)
  time_string <- paste0(hour_string, ":", mins_string, ":", secs_string)
  return(time_string)
}

Auto_Unit_Conversion <- function(dat, conversion){

  table_headings <- names(dat)

  #Add any patterns that want to be here, make sure they are unique to the type
  #they are being used for
  Speed_Patterns <- c("Speed", "Spd")
  Distance_Patterns <- c("Distance", "Dist")
  Decel_Patterns <- c("Deceleration", "Acceleration", "Decel")


  # Add any extra types that wont get recognised by the pattern recognition
  extra_speeds <- c()
  extra_distances <- c("Compression_Commencement_Threshold", "Test")
  extra_decels <- c()

  # decel_exclusions <- c(End_Initial_Deceleration_Distance_Lead, End_Initial_Deceleration_Distance_Follower, End_Final_Deceleration_Distance_Lead, End_Final_Deceleration_Distance_Follower)

  speeds <- table_headings[setdiff(grep(paste(Speed_Patterns, collapse = "|"), table_headings, ignore.case = TRUE), grep("Type", table_headings, ignore.case = TRUE))] %>%
    c(., intersect(extra_speeds, table_headings))

  distances <- table_headings[setdiff(grep(paste(Distance_Patterns, collapse = "|"), table_headings, ignore.case = TRUE), grep("Type", table_headings, ignore.case = TRUE))] %>%
    c(., intersect(extra_distances, table_headings))

  decelerations <- table_headings[setdiff(grep(paste(Decel_Patterns, collapse = "|"), table_headings, ignore.case = TRUE), grep("Type", table_headings, ignore.case = TRUE))] %>%
    c(., intersect(extra_decels, table_headings)) %>%
    setdiff(., .[grep(paste(Distance_Patterns, collapse = "|"), ., ignore.case = TRUE)]) #Removing distance patterns (Deceleration Distance)

  if (conversion == "SI_to_Aviation") {
    for (i in speeds) {
      dat <- dat %>% mutate(!!sym(i) := !!sym(i) / kts_To_mps)
    }

    for (i in distances) {
      dat <- dat %>% mutate(!!sym(i) := !!sym(i) / NM_to_m)
    }
    for (i in decelerations) {
      dat <- dat %>% mutate(!!sym(i) := !!sym(i) / (kts_To_mps / NM_to_m))
    }
  } else if (conversion == "Aviation_to_SI") {
    for (i in speeds) {
      dat <- dat %>% mutate(!!sym(i) := !!sym(i) * kts_To_mps)
    }

    for (i in distances) {
      dat <- dat %>% mutate(!!sym(i) := !!sym(i) * NM_to_m)
    }
    for (i in decelerations) {
      dat <- dat %>% mutate(!!sym(i) := !!sym(i) * (kts_To_mps / NM_to_m))
    }
  } else (message("Unknown conversion"))


  return(dat)

}

# ----------------------------------------------- #

# ----------------------------------------------- #
# 0.2.2. Simple Mathematical Functions
# ----------------------------------------------- #
#
# ----------------------------------------------- #

# ----------------------------------------------- #
# 0.2.2.1. Calculate 2D Amplitude.
# - This takes an (x,y) input
# - It then uses Pythagoras to find the amplitude

Get_2D_Amplitude <- function(x, y){
  amp <- (x^2 + y^2)^(1/2)
  return(amp)
}

# ----------------------------------------------- #
# 0.2.2.2. Calculate 2D range between two points.
# - This takes an (x,y) input
# - It then uses inverse tangent to find the vector angle
# - Quadrant matters here so ATAN2 is used

Get_2D_Angle <- function(x, y){
  angle <- atan2(y, x)
  return(angle)
}

# ----------------------------------------------- #
# 0.2.2.3. Calculate 2D Vector's X Component.
# - This takes an (amplitude,angle) input
# - It then uses the sine of the angle and the amplitude
# - Sine is used as 0 degrees is North, not East

Get_2D_Vx <- function(amp, angle){
  Vx <- amp * sin(angle)
  return(Vx)
}

# ----------------------------------------------- #
# 0.2.2.4. Calculate 2D Vector's Y Component.
# - This takes an (amplitude,angle) input
# - It then uses the cosine of the angle and the amplitude
# - Cosine is used as 0 degrees is North, not East

Get_2D_Vy <- function(amp, angle){
  Vy <- amp * cos(angle)
  return(Vy)
}

# ----------------------------------------------- #
# 0.2.2.5. Calculate 2D range between two points.
# - This takes an (x,y) co-ordinate from two points
# - It finds the deltas of x and y
# - It then uses Get_2D_Amplitude to find the Euclidean distance

Get_2D_Range <- function(x1, y1, x2, y2){
  return(Get_2D_Amplitude((x1-x2), (y1-y2)))
}

# ----------------------------------------------- #
# 0.2.2.6. Calculate 2D Scalar Product.
# - This takes an amplitude and angle from two vectors
# - It then calculates the scalar product.

Get_2D_Scalar_Product <- function(amp1, ang1, amp2, ang2){
  prod <- abs(amp1)*abs(amp2)*cos(abs(ang1 - ang2))
  return(prod)
}



# ----------------------------------------------- #

# The Rolling Join function now compatible with dplyr formulation!
rolling_join <- function(Data1, Data2, Vars1, Vars2, Roll){
  
  String1 <- "setkey(Data1"
  for (i in 1:(length(Vars1))){
    String1 <- paste0(String1, ", ", Vars1[i])
    if (i == length(Vars1)){String1 <- paste0(String1, ")")}
  }
  
  String2 <- "setkey(Data2"
  for (i in 1:(length(Vars2))){
    Data2 <- Data2 %>%
      rename(!!sym(Vars1[i]) := !!sym(Vars2[i])) 
    String2 <- paste0(String2, ", ", Vars1[i])
    if (i == length(Vars2)){String2 <- paste0(String2, ")")}
  }
  
  Data1 <- as.data.table(Data1)
  Data2 <- as.data.table(Data2)
  
  eval(str2lang(String1))
  eval(str2lang(String2))
  
  Data <- Data2[Data1, roll=Roll]
  Data <- as.data.frame(Data)
  
  Data <- Data %>%
    select(all_of(names(Data1)), everything())
  
  return(Data)
}
