# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 0.1. Global Parameters
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



# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

  
  
  
  
  
  
  
  
  
  
  