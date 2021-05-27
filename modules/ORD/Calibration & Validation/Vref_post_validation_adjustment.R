# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  Vref_post_validation_adjustment                       #
#                |                                                        #
# Version No.    |  1.0                                                   #
#                |                                                        #
# Date Modified  |  27/05/2021                                            #
#                |                                                        #
# Author(s)      |  Andy Hyde                                             #
#                |                                                        #
# Project        |  eTBS Related Projects                                 #
#                |                                                        #
# Purpose        |  Time based calculation to add or remove compression   #
#                |  to meet performance requirements                      #
# ----------------------------------------------------------------------- #

# Version History ------------------------------------------------------- #
#
# 1.0 Initial Version
#
# ----------------------------------------------------------------------- #


rm(list = ls())

library(tidyverse)
library(data.table)
library(gridExtra)
library(getPass)

#Use a version number derived from date or define manually
version <- paste0(Sys.Date(), " ","V1.0 (AH)")
# version <- "2021-05-04 V1.0 (AH)"

use_same_input_version <- T

if (use_same_input_version == T) {
  input_version <- version
} else if (use_same_input_version == F) {
  input_version <- "2021-05-04 V1.0 (AH)"   #Manually define this if you want different input output version numbers
}

#Set server  with IP then tied to this
Server <- "Maverick" #or Goose

if (Server == "Maverick") {ip <- "192.168.1.23"}
if (Server == "Goose") {ip <- "192.168.1.39"}

#Set the database name for SQL connection
database <- "NavCan_TBS_V3"

#Airport Code
Airport_Code <- "CYYZ"

#Find location of script and functions file based for running in shiny or in RSTUDIO

# --------------------------------------------------------------------------- #
ModuleFolder <- "ORD"
ModuleSubfolder <- "Calibration & Validation"
Script_out <- "Post Validation Adjustment"
# OutputFolder <- paste(ModuleFolder, Script_out, version, sep = "/")
# --------------------------------------------------------------------------- #

#Set to 1 when in git structure

FileFlag <- c("global.R", "GlobalPlaceholder.txt")[1]
ResourcesFolder <- c("resources", "GlobalFunctionsPlaceholder")[1]
AlgoResourcesFolder <- c("algorithm_functions", "AlgoFunctionsPlaceholder")[1]
ModulesFolder <- c("modules", "ModulesPlaceholder")[1]

if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  Base_Dir <- getwd()
  Global_Dir <- Base_Dir
  Script_Dir <- file.path(Base_Dir)
  while (!file.exists(file.path(Global_Dir, FileFlag))){
    Global_Dir <- file.path(Global_Dir, "..")
  }
} else {
  Global_Dir <- getwd()
  Script_Dir <- file.path(Global_Dir, ModulesFolder, ModuleFolder, ModuleSubfolder)
}

Global_Dir <- file.path(Global_Dir, ResourcesFolder)
Algo_Func_Dir <- file.path(Global_Dir, AlgoResourcesFolder)

# Global Functions, imports & parameters
source(file.path(Global_Dir, "imports.R"), local = F)
source(file.path(Global_Dir, "unit conversions.R"), local = F)
source(file.path(Global_Dir, "functions.R"), local = F)
source(file.path(Algo_Func_Dir, "ORD Functions.R"), local = F)

project <- as.numeric(getPass(msg = "Choose a Project: NAV TBS = 1,  IA LVNL = 2, Heathrow PWS = 3", noblank = FALSE, forcemask = FALSE))


#IorO for "Ouputs" or "Inputs" directory, must be a string as used in directory
Base_Dir <- GetSaveDirectory(Project = project, Algorithm = ModuleFolder, IorO = "Outputs")
Create_Directory(Base_Dir)

#Directory for reference data
# inputs_dir <- file.path(project_dir, "Inputs")
# input <- file.path(inputs_dir, "GWCS_Input", version)

inputs_dir <- GetSaveDirectory(Project = project, Algorithm = ModuleFolder, IorO = "Inputs")
ref_data <- file.path(inputs_dir, "Reference Data")

# out_data <- Base_Dir
ord_dir <- Base_Dir

ref_data <- file.path(ord_dir, "Validation", input_version)

adaptation_dir <- file.path(ord_dir, "Adaptation", input_version)

prof_data <- file.path(ord_dir, "Speed Profiles", input_version)

out_data <- file.path(ord_dir, "Post Validation Adjustment", version)
if (!dir.exists(out_data)) dir.create(out_data)

con <- Get_RODBC_Database_Connection(IP = ip, Database = database)

ord_data <- fread(file.path(ref_data, "Validation Data Post SepN Accuracy.csv"))

# type_adaptation <- fread(file.path("C:/Users/Andy Hyde/Dropbox (Think Research)/NATS Projects/NATS NavCanada TBS/Data Analysis/Outputs/ORD Output/Adaptation V3 15_04_21/Populate_tbl_ORD_Aircraft_Adaptation_New_DecelCYYZ.csv"))
# adaptation <- adaptation %>% select(c("Aircraft_Type", "Min_Safe_Landing_Speed_Lead"))

type_adaptation <- fread(file.path(adaptation_dir, "Populate_tbl_ORD_Aircraft_Adaptation_CYYZ.csv"))

wake_adaptation <- fread(file.path(adaptation_dir, "Populate_tbl_ORD_Wake_Adaptation_CYYZ.csv"))

DBS_adaptation <- fread(file.path(adaptation_dir, "Populate_tbl_ORD_DBS_Adaptation_CYYZ.csv"))



thousand_ft_gate <- 5321.9/1852
target_reduction <- 0.25
reference_wind <- 5


adjust_Vapp <- function(v2, m2, thousand_ft_gate, t, vref_lead) {


  a <- -t
  b <- 2*m2 - thousand_ft_gate - v2*t
  c <- v2*thousand_ft_gate


  a <- as.complex(a)
  quad <- c((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
            (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))

  adjusted_vref <- NA

  if (quad[1] == quad[2] & Im(quad[1]) == 0) {adjusted_vref <- Re(quad[1])}

  if (Im(quad[1]) == 0 & Im(quad[2]) != 0) {adjusted_vref <- Re(quad[1])}
  if (Im(quad[1]) != 0 & Im(quad[2]) == 0) {adjusted_vref <- Re(quad[2])}

  if (Im(quad[1]) == 0 & Im(quad[2]) == 0 & Re(quad[1]) > 0 & Re(quad[2]) < 0)  {adjusted_vref <- Re(quad[1])}
  if (Im(quad[1]) == 0 & Im(quad[2]) == 0 & Re(quad[1]) < 0 & Re(quad[2]) > 0)  {adjusted_vref <- Re(quad[2])}

  if (Im(quad[1]) == 0 & Im(quad[2]) == 0 & Re(quad[1]) > 0 & Re(quad[2]) > 0 & abs(Re(quad[1]) - vref_lead) < abs(Re(quad[2]) - vref_lead))  {adjusted_vref <- Re(quad[1])}
  if (Im(quad[1]) == 0 & Im(quad[2]) == 0 & Re(quad[1]) > 0 & Re(quad[2]) > 0 & abs(Re(quad[1]) - vref_lead) > abs(Re(quad[2]) - vref_lead))  {adjusted_vref <- Re(quad[2])}

  return(adjusted_vref)

}

# ----------------------------------------------------------------------- #
# Vref adjustment for type adaptation -------------------------------------
# ----------------------------------------------------------------------- #

vref_adjust <- type_adaptation %>% select(c("Aircraft_Type", "Landing_Stabilisation_Speed_Type_Lead",
                                       "Min_Safe_Landing_Speed_Lead", "Min_Safe_Landing_Speed_Follower",
                                       "Steady_Procedural_Speed_Follower", "Steady_Procedural_Speed_Lead",
                                       "Final_Deceleration_Lead"))

vref_adjust$Vapp_lead <- rep(NA, nrow(vref_adjust))
vref_adjust$ToF <- rep(NA, nrow(vref_adjust))
vref_adjust$ToF_adj <- rep(NA, nrow(vref_adjust))
vref_adjust$m2 <- rep(NA, nrow(vref_adjust))
# vref_adjust$steady_adj <- rep(NA, nrow(vref_adjust))
vref_adjust$des_comp <- rep(NA, nrow(vref_adjust))
vref_adjust$Vapp_adj <- rep(NA, nrow(vref_adjust))
vref_adjust$Adjusted_Vref <- rep(NA, nrow(vref_adjust))


for (i in 1:nrow(vref_adjust)) {

  vref_adjust$Vapp_lead[i] <- vref_adjust$Min_Safe_Landing_Speed_Lead[i] + calc_landing_adjustment(vref_adjust$Landing_Stabilisation_Speed_Type_Lead[i], reference_wind)
  vref_adjust$m2[i] <- (vref_adjust$Steady_Procedural_Speed_Lead[i] - vref_adjust$Vapp_lead[i] + vref_adjust$Final_Deceleration_Lead[i] * thousand_ft_gate) / vref_adjust$Final_Deceleration_Lead[i]

  vref_adjust$ToF[i] <- thousand_ft_gate / vref_adjust$Vapp_lead[i] +
    2*(vref_adjust$m2[i] - thousand_ft_gate) / (vref_adjust$Steady_Procedural_Speed_Lead[i] + vref_adjust$Vapp_lead[i])

  vref_adjust$des_comp[i] <- vref_adjust$ToF[i] * vref_adjust$Steady_Procedural_Speed_Follower[i] - vref_adjust$m2[i] + target_reduction

  vref_adjust$ToF_adj[i] <- (vref_adjust$des_comp[i] + vref_adjust$m2[i]) / vref_adjust$Steady_Procedural_Speed_Follower[i]

  vref_adjust$Vapp_adj[i] <- adjust_Vapp(vref_adjust$Steady_Procedural_Speed_Lead[i], vref_adjust$m2[i], thousand_ft_gate, vref_adjust$ToF_adj[i], vref_adjust$Vapp_lead[i])

  vref_adjust$Adjusted_Vref[i] <- vref_adjust$Vapp_adj[i] - calc_landing_adjustment(vref_adjust$Landing_Stabilisation_Speed_Type_Lead[i], reference_wind)

}

adaptation_out <- type_adaptation
adaptation_out$Min_Safe_Landing_Speed_Lead <- round(vref_adjust$Adjusted_Vref, 1)

fwrite(adaptation_out, file.path(out_data, paste0("Populate_tbl_ORD_Aircraft_Adaptation_CYYZ.csv")))


# ----------------------------------------------------------------------- #
# Vref adjustment for wake adaptation -------------------------------------
# ----------------------------------------------------------------------- #

vref_adjust_wake <- wake_adaptation %>% select(c("Wake_Cat", "Landing_Stabilisation_Speed_Type_Lead",
                                            "Min_Safe_Landing_Speed_Lead", "Min_Safe_Landing_Speed_Follower",
                                            "Steady_Procedural_Speed_Follower", "Steady_Procedural_Speed_Lead",
                                            "Final_Deceleration_Lead"))

vref_adjust_wake$Vapp_lead <- rep(NA, nrow(vref_adjust_wake))
vref_adjust_wake$ToF <- rep(NA, nrow(vref_adjust_wake))
vref_adjust_wake$ToF_adj <- rep(NA, nrow(vref_adjust_wake))
vref_adjust_wake$m2 <- rep(NA, nrow(vref_adjust_wake))
# vref_adjust$steady_adj <- rep(NA, nrow(vref_adjust))
vref_adjust_wake$des_comp <- rep(NA, nrow(vref_adjust_wake))
vref_adjust_wake$Vapp_adj <- rep(NA, nrow(vref_adjust_wake))
vref_adjust_wake$Adjusted_Vref <- rep(NA, nrow(vref_adjust_wake))




for (i in 1:nrow(vref_adjust_wake)) {

  vref_adjust_wake$Vapp_lead[i] <- vref_adjust_wake$Min_Safe_Landing_Speed_Lead[i] + calc_landing_adjustment(vref_adjust_wake$Landing_Stabilisation_Speed_Type_Lead[i], reference_wind)
  vref_adjust_wake$m2[i] <- (vref_adjust_wake$Steady_Procedural_Speed_Lead[i] - vref_adjust_wake$Vapp_lead[i] + vref_adjust_wake$Final_Deceleration_Lead[i] * thousand_ft_gate) / vref_adjust_wake$Final_Deceleration_Lead[i]

  vref_adjust_wake$ToF[i] <- thousand_ft_gate / vref_adjust_wake$Vapp_lead[i] +
    2*(vref_adjust_wake$m2[i] - thousand_ft_gate) / (vref_adjust_wake$Steady_Procedural_Speed_Lead[i] + vref_adjust_wake$Vapp_lead[i])

  vref_adjust_wake$des_comp[i] <- vref_adjust_wake$ToF[i] * vref_adjust_wake$Steady_Procedural_Speed_Follower[i] - vref_adjust_wake$m2[i] + target_reduction

  vref_adjust_wake$ToF_adj[i] <- (vref_adjust_wake$des_comp[i] + vref_adjust_wake$m2[i]) / vref_adjust_wake$Steady_Procedural_Speed_Follower[i]

  vref_adjust_wake$Vapp_adj[i] <- adjust_Vapp(vref_adjust_wake$Steady_Procedural_Speed_Lead[i], vref_adjust_wake$m2[i], thousand_ft_gate, vref_adjust_wake$ToF_adj[i], vref_adjust_wake$Vapp_lead[i])

  vref_adjust_wake$Adjusted_Vref[i] <- vref_adjust_wake$Vapp_adj[i] - calc_landing_adjustment(vref_adjust_wake$Landing_Stabilisation_Speed_Type_Lead[i], reference_wind)

}

adaptation_out_wake <- wake_adaptation
adaptation_out_wake$Min_Safe_Landing_Speed_Lead <- round(vref_adjust_wake$Adjusted_Vref, 1)

fwrite(adaptation_out_wake, file.path(out_data, paste0("Populate_tbl_ORD_Wake_Adaptation_CYYZ.csv")))

# ----------------------------------------------------------------------- #
# Vref adjustment for DBS adaptation --------------------------------------
# ----------------------------------------------------------------------- #

vref_adjust_DBS <- DBS_adaptation %>% select(c("DBS_Distance", "Landing_Stabilisation_Speed_Type_Lead",
                                                 "Min_Safe_Landing_Speed_Lead", "Min_Safe_Landing_Speed_Follower",
                                                 "Steady_Procedural_Speed_Follower", "Steady_Procedural_Speed_Lead",
                                                 "Final_Deceleration_Lead"))

vref_adjust_DBS$Vapp_lead <- rep(NA, nrow(vref_adjust_DBS))
vref_adjust_DBS$ToF <- rep(NA, nrow(vref_adjust_DBS))
vref_adjust_DBS$ToF_adj <- rep(NA, nrow(vref_adjust_DBS))
vref_adjust_DBS$m2 <- rep(NA, nrow(vref_adjust_DBS))
# vref_adjust$steady_adj <- rep(NA, nrow(vref_adjust))
vref_adjust_DBS$des_comp <- rep(NA, nrow(vref_adjust_DBS))
vref_adjust_DBS$Vapp_adj <- rep(NA, nrow(vref_adjust_DBS))
vref_adjust_DBS$Adjusted_Vref <- rep(NA, nrow(vref_adjust_DBS))




for (i in 1:nrow(vref_adjust_DBS)) {

  vref_adjust_DBS$Vapp_lead[i] <- vref_adjust_DBS$Min_Safe_Landing_Speed_Lead[i] + calc_landing_adjustment(vref_adjust_DBS$Landing_Stabilisation_Speed_Type_Lead[i], reference_wind)
  vref_adjust_DBS$m2[i] <- (vref_adjust_DBS$Steady_Procedural_Speed_Lead[i] - vref_adjust_DBS$Vapp_lead[i] + vref_adjust_DBS$Final_Deceleration_Lead[i] * thousand_ft_gate) / vref_adjust_DBS$Final_Deceleration_Lead[i]

  vref_adjust_DBS$ToF[i] <- thousand_ft_gate / vref_adjust_DBS$Vapp_lead[i] +
    2*(vref_adjust_DBS$m2[i] - thousand_ft_gate) / (vref_adjust_DBS$Steady_Procedural_Speed_Lead[i] + vref_adjust_DBS$Vapp_lead[i])

  vref_adjust_DBS$des_comp[i] <- vref_adjust_DBS$ToF[i] * vref_adjust_DBS$Steady_Procedural_Speed_Follower[i] - vref_adjust_DBS$m2[i] + target_reduction

  vref_adjust_DBS$ToF_adj[i] <- (vref_adjust_DBS$des_comp[i] + vref_adjust_DBS$m2[i]) / vref_adjust_DBS$Steady_Procedural_Speed_Follower[i]

  vref_adjust_DBS$Vapp_adj[i] <- adjust_Vapp(vref_adjust_DBS$Steady_Procedural_Speed_Lead[i], vref_adjust_DBS$m2[i], thousand_ft_gate, vref_adjust_DBS$ToF_adj[i], vref_adjust_DBS$Vapp_lead[i])

  vref_adjust_DBS$Adjusted_Vref[i] <- vref_adjust_DBS$Vapp_adj[i] - calc_landing_adjustment(vref_adjust_DBS$Landing_Stabilisation_Speed_Type_Lead[i], reference_wind)

}

adaptation_out_DBS <- DBS_adaptation
adaptation_out_DBS$Min_Safe_Landing_Speed_Lead <- round(vref_adjust_DBS$Adjusted_Vref, 1)

fwrite(adaptation_out_DBS, file.path(out_data, paste0("Populate_tbl_ORD_DBS_Adaptation_CYYZ.csv")))


# ----------------------------------------------------------------------- #
# Comparison output -------------------------------------------------------
# ----------------------------------------------------------------------- #




v7_adaptation <- fread(file.path("C:/Users/Andy Hyde/Dropbox (Think Research)/NATS Projects/NATS eTBS Adaptation Support/6. UTMA_Validation_Tool/Config_Directories/Toronto_IA_Config_Versions/Toronto_IA_Config_V7/Populate_tbl_ORD_Aircraft_Adaptation_CYYZ.csv")) %>%
  select(c("Aircraft_Type", "Min_Safe_Landing_Speed_Lead", "Min_Safe_Landing_Speed_Follower", "Final_Deceleration_Lead", "Final_Deceleration_Follower")) %>%
  rename(V7_Vref_Lead = Min_Safe_Landing_Speed_Lead, V7_Vref_Foll = Min_Safe_Landing_Speed_Follower, V7_Decel_Lead = Final_Deceleration_Lead, V7_Decel_Foll = Final_Deceleration_Follower)

v8_adaptation <- adaptation_out %>% select(c("Aircraft_Type", "Min_Safe_Landing_Speed_Lead", "Min_Safe_Landing_Speed_Follower", "Final_Deceleration_Lead", "Final_Deceleration_Follower")) %>%
  rename(V8_Vref_Lead = Min_Safe_Landing_Speed_Lead, V8_Vref_Foll = Min_Safe_Landing_Speed_Follower, V8_Decel_Lead = Final_Deceleration_Lead, V8_Decel_Foll = Final_Deceleration_Follower)


adaptation_diff <- full_join(v7_adaptation, v8_adaptation, by = "Aircraft_Type") %>%
  relocate(V8_Vref_Lead, .after = V7_Vref_Lead) %>%
  relocate(V8_Vref_Foll, .after = V7_Vref_Foll) %>%
  relocate(V8_Decel_Lead, .after = V7_Decel_Lead) %>%
  relocate(V8_Decel_Foll, .after = V7_Decel_Foll)

adaptation_diff$Vref_Diff_Lead <- adaptation_diff$V8_Vref_Lead - adaptation_diff$V7_Vref_Lead
adaptation_diff$Vref_Diff_Foll <- adaptation_diff$V8_Vref_Foll - adaptation_diff$V7_Vref_Foll

fwrite(adaptation_diff, file.path(out_data, paste0("Adaptation_Comparison.csv")))
