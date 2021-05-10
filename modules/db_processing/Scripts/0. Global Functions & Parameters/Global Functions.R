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
# AlgoResourcesFolder <- c("non-global", "AlgoFunctionsPlaceholder")[1]
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
# source(file.path(Global_Dir, "Global Parameters.R"), local = F)
# source(file.path(Global_Dir, "Global Functions.R"), local = F)
# 
# Base_Dir <- GetSaveDirectory(Project, OutputFolder)
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

# Function to choose Output Path depending on Project (Given user Input)
GetSaveDirectory <- function(Project, Algorithm){
  
  Project <- as.numeric(getPass(msg = "Choose a Project: NAV TBS = 1,  IA LVNL = 2, Heathrow PWS = 3", noblank = FALSE, forcemask = FALSE))
  
  if (Project == 1){
    Dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Think Research)", "NATS Projects", "NATS NavCanada TBS", "23 Data Analysis")
  }
  
  # Go into Algorithm Folder
  Dir <- file.path(Dir, "Outputs", Algorithm)
  
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

# Function to Load in Adaptation. Currently use RODBC. If we change package then all adaptation loads occur here.
Load_Adaptation_Table <- function(con, Table_Name){
  Table <- sqlQuery(con, paste0("SELECT * FROM ", Table_Name), stringsAsFactors = F)
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
    File <- sqlQuery(con, Query, stringsAsFactors = F)
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


