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
  return(format(as.Date(gsub("^.*([2]{1}[0]{1}[0-9]{2}[0-1]{1}[0-9]{1}[0-3]{1}[0-9]{1}).*$", "\\1", basename(Log_Filename)), format = "%Y%m%d"), "%d/%m/%Y"))
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

generateFPID <- function(tracks, dbi_con = dbi_con) {
  
  fp <- as.data.table(dbGetQuery(dbi_con, "SELECT DISTINCT Flight_Plan_ID, FP_Date, FP_Time, Callsign, SSR_Code FROM tbl_Flight_Plan"))
  
  if (nrow(fp) > 0 & nrow(tracks) > 0) {
    
    if ("Flight_Plan_ID" %in% names(tracks)) tracks$Flight_Plan_ID <- NULL
    
    fp$SSR_Code <- as.numeric(fp$SSR_Code)
    
    tracks <- tracks[fp, roll = "nearest", on = c(Track_Date = "FP_Date", Track_Time = "FP_Time", Callsign = "Callsign", SSR_Code = "SSR_Code")]
    
  } else {
    
    message("[",Sys.time(),"] ", "Failed to generate Flight_Plan_ID")
    
  }
  
  # for (j in unique(fp[FP_Date %in% unique(out$Track_Date)]$Flight_Plan_ID)) {
  #   out[
  #     Track_Date == fp[Flight_Plan_ID == j]$FP_Date &
  #       abs(Track_Time - fp[Flight_Plan_ID == j]$FP_Time) < 7200 &
  #       Callsign == fp[Flight_Plan_ID == j]$Callsign &
  #       grepl(paste0("^[0]?", fp[Flight_Plan_ID == j]$SSR_Code, "$"), SSR_Code)
  #   ]$Flight_Plan_ID <- j
  # }
  
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
        ifelse(substr(x, 10, 10) == "S", -1, 1) / pi * 180
    )
  } else if (all(nchar(x) == 11)) { # longitude format: dddmmss.hhW
    return(
      (as.numeric(substr(x, 1, 3)) +
         as.numeric(substr(x, 4, 5)) / 60 +
         as.numeric(substr(x, 6, 7)) / 3600 +
         as.numeric(substr(x, 9, 10)) / 360000) *
        ifelse(substr(x, 11, 11) == "W", -1, 1) / pi * 180
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
      
      Psi_i <- log( tan(Phi_i / 2 + pi / 4) * (1 - e * sin(Phi_i)) / (1 + e * sin(Phi_i))^(e / 2) )
      
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
  Lamda <- abs(PositionLongitude)
  
  # Set up False Easting / Northings and scaling as required.
  FN <- tbl_Adaptation_Data$Grid_Offset_Y[1]
  FE <- tbl_Adaptation_Data$Grid_Offset_X[1]
  K_0 <- 1.0
  
  # Set up origin as required (R27L threshold for local coords).
  # Geodetic origin latitude / longitude.
  Phi_0 <- tbl_Adaptation_Data$Grid_Projection_Origin_Lat[1]
  Lamda_0 <- abs(tbl_Adaptation_Data$Grid_Projection_Origin_Lon[1])
  
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
