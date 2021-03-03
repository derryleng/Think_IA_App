# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 0.2. Global Functions
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
# 
# Description: Resource file for global functions for the
#              tool. These are applied across IA. 
#
# Use Guide Section: 
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# Version History
# ------------------------------------------------------------------------------------------------------------------------------------------ # 
#
# v0: Added Convert_Time_String_to_Seconds without functioning milliseconds
#     Added Convert_Seconds_to_Time_String but s/ms currently not functioning
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ----------------------------------------------- #
# 0.2.1. Config Functions
# ----------------------------------------------- #
# SQL Access, Basic Manipulation
# ----------------------------------------------- #

# the "not in" function
"%!in%" <- function(x,y) !("%in%"(x,y))

# Get Database Connection (RODBC)
Get_RODBC_Database_Connection <- function(IP, Database){
  User <- "vbuser"
  Pass <- "Th!nkvbuser"
  con <- RODBC::odbcDriverConnect(connection=paste0("Driver={SQL Server};
                                  Server={",IP,"};Database={", Database, "};
                                  Uid={",User,"};Pwd={",Pass,"};"))
  return(con)
}

# Function to Load in Adaptation. Currently use RODBC. If we change package then all adaptation loads occur here.
Load_Adaptation_Table <- function(con, Table_Name){
  Table <- sqlQuery(con, paste0("SELECT * FROM ", Table_Name), stringsAsFactors = F)
  return(Table)
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

# ----------------------------------------------- #
# 0.2.3. Arbritrary Check Functions
# ----------------------------------------------- #
# Includes 
# ----------------------------------------------- #

Load_Adaptation_Table <- function(con, Table_Name){
  # Currently use RODBC. If we change package then all adaptation loads occur here.
  Table <- sqlQuery(con, paste0("SELECT * FROM ", Table_Name), stringsAsFactors = F)
  return(Table)
}

