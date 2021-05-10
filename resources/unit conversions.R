# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Unit Conversions
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
# 
# Description: Resource file for global parameters for the
#              tool. 
#
# Use Guide Section: 
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# Version History
# ------------------------------------------------------------------------------------------------------------------------------------------ # 
#
# v0: Added aviation unit conversion parameters: Time, speed, distance, acceleration, pressure
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ----------------------------------------------- #
# 0.1.1 Unit Conversion Parameters
# ----------------------------------------------- #
# Parameters to convert between aviation/SI units.
# ----------------------------------------------- #

# ---- Time Conversions

# Minutes min to Seconds s
min_to_s <- 60

# Hours h to Minutes min
h_to_min <- 60

# Days d to Hours h
d_to_h <- 24

# Hours h to Seconds s
h_to_s <- min_to_s * h_to_min

# Days d to Seconds s
d_to_s <- d_to_h * h_to_s

# ---- Distance & Derivative Conversions

# Feet Ft to Metres m (Altitude)
Ft_to_m <- 0.3048

# Nautical Miles NM to Metres m (Distance)
NM_to_m <- 1852

# Knots kts (NM/hour) to Metres Per Second mps (Speed) 
kts_To_mps <- NM_to_m/h_to_s

# ---= Other Conversions

# Degrees deg to Radians rad (Angle)
deg_to_rad <- pi/180

# Millibars Mb to Pascals Pa (Pressure)
Mb_to_Pa <- 100

########################################################################

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

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

  
  
  
  
  
  
  
  
  
  
  