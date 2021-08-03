# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |                                                        #
#                |                                                        #
# Version No.    |  1.0                                                   #
#                |                                                        #
# Date Modified  |  19/07/2021                                            #
#                |                                                        #
# Author(s)      |  Dan Brown                                             #
#                |                                                        #
# Project        |  LVNL                                                  #
#                |                                                        #
# Purpose        |  ORD Investigation of new AC Types                     #
#                |                                                        #
# ----------------------------------------------------------------------- #
# ----------------------------------------------------------------------- #

# ----------------------------------------------------------------------- #
# Load Packages ----------------------------------------------------------
# ----------------------------------------------------------------------- #


library(tidyverse)
library(data.table)
library(ggplot2)
library(ggrepel)

database <- "LVNL_UTMA_Validation_GWCS_Update"
database2 <- "EGLL_PWS"
characteristics_dir <- "C:/Users/Daniel Brown/Dropbox (Think Research)/NATS Projects/NATS LVNL Schiphol/Phase 2/2. ORD/Implementation Support Additional Types"
plot_dir_root <- "C:/Users/Daniel Brown/Dropbox (Think Research)/NATS Projects/NATS LVNL Schiphol/Phase 2/2. ORD/Implementation Support Additional Types/Plots_Embraer_Additions"

# ----------------------------------------------------------------------- #
# GitHub Setup ----------------------------------------------------------
# ----------------------------------------------------------------------- #

#Use a version number derived from date or define manually
version <- paste0(Sys.Date(), " ","V2.0 (AH)")
# version <- "2021-05-04 V1.0 (AH)"

use_same_input_version <- T

if (use_same_input_version == T) {
  input_version <- version
} else if (use_same_input_version == F) {
  input_version <- "2021-05-04 V1.0 (AH)"   #Manually define this if you want different input output version numbers
}

#Set server  with IP then tied to this
Server <- "Goose" #or Maverick

if (Server == "Maverick") {ip <- "192.168.1.23"}
if (Server == "Goose") {ip <- "192.168.1.39"}


Server2 <- "Maverick" #or Goose

if (Server2 == "Maverick") {ip2 <- "192.168.1.23"}
if (Server2 == "Goose") {ip2 <- "192.168.1.39"}

#Find location of script and functions file based for running in shiny or in RSTUDIO

# --------------------------------------------------------------------------- #
ModuleFolder <- "ORD"
ModuleSubfolder <- "Reference Data"
Script_out <- "Aircraft Characteristics"
OutputFolder <- paste(ModuleFolder, Script_out, version, sep = "/")
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

out_data <- Script_Dir

con <- Get_DBI_Connection(IP = ip, Database = database)
con2 <- Get_DBI_Connection(IP = ip2, Database = database2)

# ----------------------------------------------------------------------- #
# Functions -------------------------------------------------------------
# ----------------------------------------------------------------------- #

#taken from ORD_Scripts/Function.r (not used)
calc_landing_adjustment <- function(landing_type, headwind) {
  return(
    if (landing_type %in% c(0, 10, 11, 12)) {
      sapply(headwind / 2, function(hw_adj) ifelse(hw_adj < 5, 5, ifelse(hw_adj > 20, 20, hw_adj)))
    } else if (landing_type %in% c(1, 2, 3, 4, 5)) {
      sapply(headwind / 3, function(hw_adj) ifelse(hw_adj < 5, 5, ifelse(hw_adj > 15, 15, hw_adj)))
    } else if (landing_type %in% c(6)) {
      sapply(headwind / 2, function(hw_adj) ifelse(hw_adj < 0, 0, ifelse(hw_adj > 20, 20, hw_adj)))
    } else if (landing_type %in% c(7)) {
      sapply(headwind, function(hw_adj) 10)
    } else if (landing_type %in% c(8)) {
      sapply(headwind, function(hw_adj) 0)
    } else if (landing_type %in% c(9)) {
      sapply(headwind, function(hw_adj) ifelse(hw_adj > 20, 15, ifelse(hw_adj > 10, 10, 5)))
    }
  )
}

# ----------------------------------------------------------------------- #
# Load Data -------------------------------------------------------------
# ----------------------------------------------------------------------- #

characteristic_data <- read_csv(file.path(characteristics_dir, "Aircraft_Characteristics - Skybrary+Misc.csv"))

#gets information on aircraft adaptation
ord_adaptation_ac_type <- as.data.table(dbGetQuery(con, "SELECT * FROM tbl_ORD_Aircraft_Adaptation")) %>%
                          select(Aircraft_Type, Min_Safe_Landing_Speed_Lead, Landing_Stabilisation_Speed_Type_Lead) %>%
                          rename(vref_AC_Type = Min_Safe_Landing_Speed_Lead) %>%
                          rename(LSS_Type_Lead_AC_Type = Landing_Stabilisation_Speed_Type_Lead)

#gets information on new aircraft from Heathrow adaptation
ord_adaptation_ac_type_EGLL <- as.data.table(dbGetQuery(con2, "SELECT * FROM tbl_ORD_Aircraft_Adaptation WHERE Aircraft_Type IN ('B753','A339','E120','E295','HDJT','BCS1','E290')")) %>%
                                select(Aircraft_Type, Min_Safe_Landing_Speed_Lead, Landing_Stabilisation_Speed_Type_Lead) %>%
                                rename(vref_AC_Type = Min_Safe_Landing_Speed_Lead) %>%
                                rename(LSS_Type_Lead_AC_Type = Landing_Stabilisation_Speed_Type_Lead)

#creates copy of Heathrow adaptation aircraft so they are also given separate predicted vrefs
ord_adaptation_ac_type_EGLL_Copy <- ord_adaptation_ac_type_EGLL %>% mutate(vref_AC_Type = NA, LSS_Type_Lead_AC_Type = NA)

#adds rows for new aircraft from Heathrow adaptation
ord_adaptation_ac_type <- ord_adaptation_ac_type %>% add_row(ord_adaptation_ac_type_EGLL) %>%
                          mutate(from_EGLL_Adaptation = ifelse(Aircraft_Type %in% c("B753","A339","E120","E295","HDJT","BCS1","E290"),T,F)) %>%
                          add_row(ord_adaptation_ac_type_EGLL_Copy)

#gets information on Wake adaptation
ord_adaptation_wake <- as.data.table(dbGetQuery(con, "SELECT * FROM tbl_ORD_Wake_Adaptation")) %>%
                       select(Wake_Cat, Min_Safe_Landing_Speed_Lead, Landing_Stabilisation_Speed_Type_Lead) %>%
                       rename(vref_Wake = Min_Safe_Landing_Speed_Lead) %>%
                       rename(LSS_Type_Lead_Wake = Landing_Stabilisation_Speed_Type_Lead)

#gets aircraft type to wake table
AC_Type_To_Wake <- as.data.table(dbGetQuery(con, "SELECT * FROM tbl_Aircraft_Type_To_Wake"))

# ----------------------------------------------------------------------- #
# Processing ------------------------------------------------------------
# ----------------------------------------------------------------------- #


#creates table joining ord adaptation with characteristic data
ac_characteristics <- full_join(characteristic_data, ord_adaptation_ac_type, by=c("Aircraft_Type"="Aircraft_Type")) %>%
                      mutate(Area = Wingspan * Length,
                             Wingspan_Weight = Wingspan/Max_Takeoff_Weight,
                             Wingspan_Sq_Weight = Wingspan^2/Max_Takeoff_Weight,
                             from_EGLL_Adaptation = ifelse(is.na(from_EGLL_Adaptation),F,from_EGLL_Adaptation),
                             New_AC = ifelse(from_EGLL_Adaptation, "Heathrow", ifelse(Aircraft_Type %in% c("B753","A339","E120","E295","HDJT","BCS1","E290"), "New Schipol","Current Schipol"))) %>%
                      left_join(AC_Type_To_Wake, by=c("Aircraft_Type"="Aircraft_Type")) %>% #joins on Wake Category
                      select(-Aircraft_Class) %>%
                      left_join(ord_adaptation_wake, by=c("Wake"="Wake_Cat"))%>%   #joins on Wake category vrefs
                      mutate(vref_AC_Type = vref_AC_Type*1.9438445) %>% #conversion of m/s to knots
                      mutate(vref_Wake = vref_Wake*1.9438445) %>% #conversion of m/s to knots
                      mutate(LSS_Type_Lead_AC_Type = ifelse(is.na(LSS_Type_Lead_AC_Type),0,LSS_Type_Lead_AC_Type)) %>%
                      relocate(vref_AC_Type, .after=Aircraft_Type)%>%
                      relocate(vref_Wake, .after=vref_AC_Type) %>%
                      relocate(LSS_Type_Lead_AC_Type, .after=Wingspan_Sq_Weight) %>%
                      relocate(LSS_Type_Lead_Wake, .after=LSS_Type_Lead_AC_Type) %>%
                      relocate(Wake, .after=Aircraft_Type) %>%
                      relocate(Extra_Details, .after = LSS_Type_Lead_Wake) #%>%
                      #rowwise() %>%
                      #mutate(vref_AC_Type = (vref_AC_Type + calc_landing_adjustment(LSS_Type_Lead_AC_Type,5))) %>% #calculates vref in 5kts headwind based on LSS type of Aircraft
                      #mutate(vref_Wake = (vref_Wake + calc_landing_adjustment(LSS_Type_Lead_AC_Type, 5))) #calculates vref in 5kts headwind based on LSS type of Wake Cat
                      

ac_characteristics$vref_MTOW <- NA
ac_characteristics$vref_Wingspan_Sq <- NA

#adds in extra rows
              
# ----------------------------------------------------------------------- #
# Model -----------------------------------------------------------------
# ----------------------------------------------------------------------- #

#separate into each wake category
for(wake in unique(ac_characteristics$Wake)){
  if(wake=="D"){
    ac_characteristics_wake <- ac_characteristics %>% filter(Wake==wake | Aircraft_Type %in% c("E190", "E195")) %>% filter(New_AC == "Current Schipol")
  }
  else{
    ac_characteristics_wake <- ac_characteristics %>% filter(Wake==wake) %>% filter(New_AC == "Current Schipol")
  }
    #MTOW model
    lm_MTOW <- lm(vref_AC_Type ~ Max_Takeoff_Weight, data = ac_characteristics_wake)
    
    #Wingspan Squared/MTOW model
    lm_Wingspan_Sq <- lm(vref_AC_Type ~ Wingspan_Sq_Weight, data = ac_characteristics_wake)
    
    #MTOW predictions
    predicted_MTOW <- data.frame(Max_Takeoff_Weight = seq(min(ac_characteristics_wake$Max_Takeoff_Weight)-10, max(ac_characteristics_wake$Max_Takeoff_Weight), length.out = 100))
    temp_predicted <- predict(lm_MTOW, newdata = predicted_MTOW, interval = "confidence")
    
    predicted_MTOW$vref <- temp_predicted[,1] #predicted value
    predicted_MTOW$lci <- temp_predicted[,2] #lower confidence interval
    predicted_MTOW$uci <- temp_predicted[,3] #upper confidence interval
    
    if(wake=="D"){
      ac_characteristics_MTOW <- ac_characteristics %>% filter(Wake==wake | Aircraft_Type %in% c("E190", "E195")) %>% rowwise %>%
                               mutate(vref_AC_Type = ifelse(New_AC=="New Schipol", predict(lm_MTOW, newdata = data.frame(Max_Takeoff_Weight = c(Max_Takeoff_Weight))),vref_AC_Type)) %>%
                               mutate(predicted_vref = predict(lm_MTOW, newdata = data.frame(Max_Takeoff_Weight=c(Max_Takeoff_Weight))))
    }
    else{
      ac_characteristics_MTOW <- ac_characteristics %>% filter(Wake==wake) %>% rowwise %>%
        mutate(vref_AC_Type = ifelse(New_AC=="New Schipol", predict(lm_MTOW, newdata = data.frame(Max_Takeoff_Weight = c(Max_Takeoff_Weight))),vref_AC_Type)) %>%
        mutate(predicted_vref = predict(lm_MTOW, newdata = data.frame(Max_Takeoff_Weight=c(Max_Takeoff_Weight))))
    }
    #add MTOW predictions to main table
    ac_characteristics <- ac_characteristics %>% mutate(vref_MTOW = ifelse(Wake==wake & New_AC=="New Schipol",predict(lm_MTOW, newdata = data.frame(Max_Takeoff_Weight=c(Max_Takeoff_Weight))),vref_MTOW))
    
    #Wingspan Squared/Weight predictions
    predicted_Wingspan_Sq <- data.frame(Wingspan_Sq_Weight = seq(min(ac_characteristics_wake$Wingspan_Sq_Weight), max(ac_characteristics_wake$Wingspan_Sq_Weight), length.out = 100))
    temp_predicted <- predict(lm_Wingspan_Sq, newdata = predicted_Wingspan_Sq, interval = "confidence")
    
    predicted_Wingspan_Sq$vref <- temp_predicted[,1] #predicted value
    predicted_Wingspan_Sq$lci <- temp_predicted[,2] #lower confidence interval
    predicted_Wingspan_Sq$uci <- temp_predicted[,3] #upper confidence interval
    
    if(wake=="D"){
      ac_characteristics_Wingspan_Sq <- ac_characteristics %>% filter(Wake==wake | Aircraft_Type %in% c("E190", "E195")) %>% rowwise %>%
                                      mutate(vref_AC_Type = ifelse(New_AC=="New Schipol", predict(lm_Wingspan_Sq, newdata = data.frame(Wingspan_Sq_Weight = c(Wingspan_Sq_Weight))),vref_AC_Type)) %>%
                                      mutate(predicted_vref = predict(lm_Wingspan_Sq, newdata = data.frame(Wingspan_Sq_Weight=c(Wingspan_Sq_Weight))))
    }
    else{
      ac_characteristics_Wingspan_Sq <- ac_characteristics %>% filter(Wake==wake) %>% rowwise %>%
        mutate(vref_AC_Type = ifelse(New_AC=="New Schipol", predict(lm_Wingspan_Sq, newdata = data.frame(Wingspan_Sq_Weight = c(Wingspan_Sq_Weight))),vref_AC_Type)) %>%
        mutate(predicted_vref = predict(lm_Wingspan_Sq, newdata = data.frame(Wingspan_Sq_Weight=c(Wingspan_Sq_Weight))))
    }
    #add Wingspan Squared/Weight predictions to main table
    ac_characteristics <- ac_characteristics %>% mutate(vref_Wingspan_Sq = ifelse(Wake==wake & New_AC=="New Schipol",predict(lm_Wingspan_Sq, newdata = data.frame(Wingspan_Sq_Weight=c(Wingspan_Sq_Weight))),vref_Wingspan_Sq))
    
    #assigns variables for each wake category for use in plots later
    assign(paste("lm_MTOW_",wake, sep=""), lm_MTOW)
    assign(paste("lm_Wingspan_Sq_",wake, sep=""), lm_Wingspan_Sq)
    assign(paste("summary_MTOW_",wake, sep=""), summary(lm_MTOW))
    assign(paste("summary_Wingspan_Sq_",wake, sep=""), summary(lm_Wingspan_Sq))
    assign(paste("ac_characteristics_MTOW_",wake, sep=""), ac_characteristics_MTOW)
    assign(paste("ac_characteristics_Wingspan_Sq_",wake, sep=""), ac_characteristics_Wingspan_Sq)
    assign(paste("predicted_MTOW_",wake, sep=""), predicted_MTOW)
    assign(paste("predicted_Wingspan_Sq_",wake, sep=""), predicted_Wingspan_Sq)
}

model_statistics <- as.data.frame(c("MTOW","Wingspan Sq/Weight"),col.names=c("Model"))
model_statistics$model <- c("MTOW","Wingspan Sq/Weight")
model_statistics <- model_statistics %>% select(model)
model_statistics$Wake_A_rsquared <- c(summary_MTOW_A$r.squared,summary_Wingspan_Sq_A$r.squared)
model_statistics$Wake_B_rsquared <- c(summary_MTOW_B$r.squared,summary_Wingspan_Sq_B$r.squared)
model_statistics$Wake_C_rsquared <- c(summary_MTOW_C$r.squared,summary_Wingspan_Sq_C$r.squared)
model_statistics$Wake_D_rsquared <- c(summary_MTOW_D$r.squared,summary_Wingspan_Sq_D$r.squared)
model_statistics$Wake_E_rsquared <- c(summary_MTOW_E$r.squared,summary_Wingspan_Sq_E$r.squared)
model_statistics$Wake_F_rsquared <- c(summary_MTOW_F$r.squared,summary_Wingspan_Sq_F$r.squared)
write_csv(model_statistics,paste(characteristics_dir,"/Model_Statistics.csv",sep=""))

# ----------------------------------------------------------------------- #
# Plots -----------------------------------------------------------------
# ----------------------------------------------------------------------- #

for(wake in unique(ac_characteristics$Wake)){
  #separate case for D wake as we choose to include E190 and E195 in the graphs (but not the model)
    if(wake=="D"){
      plot_dir <- paste(plot_dir_root,"/",wake,sep="")
    
      ac_characteristics_MTOW <- get(paste("ac_characteristics_MTOW_",wake,sep=""))
      ac_characteristics_Wingspan_Sq <- get(paste("ac_characteristics_Wingspan_Sq_",wake,sep=""))
      predicted_MTOW <- get(paste("predicted_MTOW_",wake,sep=""))
      predicted_Wingspan_Sq <- get(paste("predicted_Wingspan_Sq_",wake,sep=""))
      wake_vref <- ac_characteristics_MTOW$vref_Wake[1]
      
      #plots with prediction line
      model_wake_MTOW <- ac_characteristics_MTOW %>% filter(New_AC=="Current Schipol")
      ggplot() + geom_point(data = model_wake_MTOW, aes(x=Max_Takeoff_Weight, y=vref_AC_Type)) +
                 geom_line(data = predicted_MTOW, aes(x=Max_Takeoff_Weight, y=vref))+
                 geom_smooth(data = predicted_MTOW, aes(x=Max_Takeoff_Weight, y=vref, ymin=lci, ymax=uci), stat="identity") +
                 labs(title = "Vref Predicted by Max Takeoff Weight for Wake Cat D (plus E190 & E195)", x="Max Takeoff Weight (kg)", y="Vref (kts)", color="Adaptation")
      ggsave(paste("MTOW_Model_Wake_",wake,".png",sep=""), device="png", path = plot_dir)
      
      model_wake_Wingspan_Sq <- ac_characteristics_Wingspan_Sq %>% filter(New_AC=="Current Schipol")
      ggplot() + geom_point(data = model_wake_Wingspan_Sq, aes(x=Wingspan_Sq_Weight, y=vref_AC_Type)) +
                 geom_line(data = predicted_Wingspan_Sq, aes(x=Wingspan_Sq_Weight, y=vref))+
                 geom_smooth(data = predicted_Wingspan_Sq, aes(x=Wingspan_Sq_Weight, y = vref, ymin = lci, ymax = uci), stat="identity")+
                 labs(title = "Vref Predicted by Wingspan Squared/Weight for Wake Cat D (plus E190 & E195)", x="Wingspan Squared/Weight", y="Vref (kts)", color="Adaptation")
      ggsave(paste("Wingspan_Sq_Model_Wake_",wake,".png",sep=""), device="png", path = plot_dir)    
      
      #plots with aircraft labels
      ggplot(ac_characteristics_MTOW) + geom_text_repel(aes(x=Max_Takeoff_Weight, y=vref_AC_Type, label = Aircraft_Type, color=New_AC), max.time=5, max.overlaps = Inf, show.legend=F) +
                                        geom_point(aes(x=Max_Takeoff_Weight, y=vref_AC_Type, color=New_AC)) +
                                        geom_abline(slope=0, intercept = wake_vref)+
                                        geom_abline(slope=0, intercept = wake_vref-5, linetype="dashed") +
                                        geom_text(data=data.frame(x=0,y=wake_vref), aes(x, y), label=paste("Vref for Wake ",wake," aircraft",sep=""), vjust=-.5, hjust = -.001) +
                                        geom_text(data=data.frame(x=0,y=wake_vref-5), aes(x, y), label=paste("Vref for Wake ",wake," aircraft -5kts",sep=""), vjust=-.5, hjust= -.001) +
                                        labs(title = paste("Vref by Max Takeoff Weight for Wake Cat D plus E190 & E195"," (Aircraft Labelled)",sep=""), x="Max Takeoff Weight (kg)", y="Vref (kts)", color="Adaptation")
      ggsave(paste("MTOW_Aircraft_Wake_",wake,".png",sep=""), device="png", path = plot_dir)
      
      ggplot(ac_characteristics_Wingspan_Sq) + geom_text_repel(aes(x=Wingspan_Sq_Weight, y=vref_AC_Type, label = Aircraft_Type, color=New_AC), max.time=5, max.overlaps = Inf, show.legend=F) +
        geom_point(aes(x=Wingspan_Sq_Weight, y=vref_AC_Type, color=New_AC)) +
        geom_abline(slope=0, intercept = wake_vref) +
        geom_abline(slope=0, intercept = wake_vref-5, linetype="dashed") +
        geom_text(data=data.frame(x=0,y=wake_vref), aes(x, y), label=paste("Vref for Wake ",wake," aircraft",sep=""), vjust=-.5, hjust = -0.001) +
        geom_text(data=data.frame(x=0,y=wake_vref-5), aes(x, y), label=paste("Vref for Wake ",wake," aircraft -5kts",sep=""), vjust=-.5, hjust = -0.001) +
        labs(title = paste("Vref by Wingspan Squared/Weight for Wake Cat D (plus E190 & E195)", " (Aircraft Labelled)",sep=""), x="Wingspan Squared/Weight", y="Vref (kts)", color="Adaptation")
      ggsave(paste("Wingspan_Sq_Aircraft_Wake_",wake,".png",sep=""), device="png", path = plot_dir)
      
      #plot of MTOW vs Wingspan
      ggplot(ac_characteristics_MTOW) + geom_text_repel(aes(x=Wingspan, y=Max_Takeoff_Weight, label = Aircraft_Type, color=New_AC), max.time=5, max.overlaps = Inf, show.legend=F) +
                                        geom_point(aes(x=Wingspan, y=Max_Takeoff_Weight, color=New_AC)) +
                                        labs(title = paste("Wingspan vs Max Takeoff Weight for each Aircraft Type (Wake Cat D plus E190 & E195",")",sep=""), x="Wingspan (m)", y="Max Takeoff Weight (kg)", color="Adaptation")
      ggsave(paste("Wingspan_vs_MTOW_Wake_",wake,".png",sep=""),device="png",path=plot_dir)
    }
  else{
    plot_dir <- paste(plot_dir_root,"/",wake,sep="")
    
    ac_characteristics_MTOW <- get(paste("ac_characteristics_MTOW_",wake,sep=""))
    ac_characteristics_Wingspan_Sq <- get(paste("ac_characteristics_Wingspan_Sq_",wake,sep=""))
    predicted_MTOW <- get(paste("predicted_MTOW_",wake,sep=""))
    predicted_Wingspan_Sq <- get(paste("predicted_Wingspan_Sq_",wake,sep=""))
    wake_vref <- ac_characteristics_MTOW$vref_Wake[1]
    
    #plots with prediction line
    model_wake_MTOW <- ac_characteristics_MTOW %>% filter(New_AC=="Current Schipol")
    ggplot() + geom_point(data = model_wake_MTOW, aes(x=Max_Takeoff_Weight, y=vref_AC_Type)) +
      geom_line(data = predicted_MTOW, aes(x=Max_Takeoff_Weight, y=vref))+
      geom_smooth(data = predicted_MTOW, aes(x=Max_Takeoff_Weight, y=vref, ymin=lci, ymax=uci), stat="identity") +
      labs(title = paste("Vref Predicted by Max Takeoff Weight for Wake Cat ",wake,sep=""), x="Max Takeoff Weight (kg)", y="Vref (kts)", color="Adaptation")
    ggsave(paste("MTOW_Model_Wake_",wake,".png",sep=""), device="png", path = plot_dir)
    
    model_wake_Wingspan_Sq <- ac_characteristics_Wingspan_Sq %>% filter(New_AC=="Current Schipol")
    ggplot() + geom_point(data = model_wake_Wingspan_Sq, aes(x=Wingspan_Sq_Weight, y=vref_AC_Type)) +
      geom_line(data = predicted_Wingspan_Sq, aes(x=Wingspan_Sq_Weight, y=vref))+
      geom_smooth(data = predicted_Wingspan_Sq, aes(x=Wingspan_Sq_Weight, y = vref, ymin = lci, ymax = uci), stat="identity")+
      labs(title = paste("Vref Predicted by Wingspan Squared/Weight for Wake Cat ",wake,sep=""), x="Wingspan Squared/Weight", y="Vref (kts)", color="Adaptation")
    ggsave(paste("Wingspan_Sq_Model_Wake_",wake,".png",sep=""), device="png", path = plot_dir)    
    
    #plots with aircraft labels
    ggplot(ac_characteristics_MTOW) + geom_text_repel(aes(x=Max_Takeoff_Weight, y=vref_AC_Type, label = Aircraft_Type, color=New_AC), max.time=5, max.overlaps = Inf, show.legend=F) +
      geom_point(aes(x=Max_Takeoff_Weight, y=vref_AC_Type, color=New_AC)) +
      geom_abline(slope=0, intercept = wake_vref)+
      geom_abline(slope=0, intercept = wake_vref-5, linetype="dashed") +
      geom_text(data=data.frame(x=0,y=wake_vref), aes(x, y), label=paste("Vref for Wake ",wake," aircraft",sep=""), vjust=-.5, hjust = -.001) +
      geom_text(data=data.frame(x=0,y=wake_vref-5), aes(x, y), label=paste("Vref for Wake ",wake," aircraft -5kts",sep=""), vjust=-.5, hjust= -.001) +
      labs(title = paste("Vref by Max Takeoff Weight for Wake Cat ",wake," (Aircraft Labelled)",sep=""), x="Max Takeoff Weight (kg)", y="Vref (kts)", color="Adaptation")
    ggsave(paste("MTOW_Aircraft_Wake_",wake,".png",sep=""), device="png", path = plot_dir)
    
    ggplot(ac_characteristics_Wingspan_Sq) + geom_text_repel(aes(x=Wingspan_Sq_Weight, y=vref_AC_Type, label = Aircraft_Type, color=New_AC), max.time=5, max.overlaps = Inf, show.legend=F) +
      geom_point(aes(x=Wingspan_Sq_Weight, y=vref_AC_Type, color=New_AC)) +
      geom_abline(slope=0, intercept = wake_vref) +
      geom_abline(slope=0, intercept = wake_vref-5, linetype="dashed") +
      geom_text(data=data.frame(x=0,y=wake_vref), aes(x, y), label=paste("Vref for Wake ",wake," aircraft",sep=""), vjust=-.5, hjust = -0.001) +
      geom_text(data=data.frame(x=0,y=wake_vref-5), aes(x, y), label=paste("Vref for Wake ",wake," aircraft -5kts",sep=""), vjust=-.5, hjust = -0.001) +
      labs(title = paste("Vref by Wingspan Squared/Weight for Wake Cat ",wake, " (Aircraft Labelled)",sep=""), x="Wingspan Squared/Weight", y="Vref (kts)", color="Adaptation")
    ggsave(paste("Wingspan_Sq_Aircraft_Wake_",wake,".png",sep=""), device="png", path = plot_dir)
    
    #plot of MTOW vs Wingspan
    ggplot(ac_characteristics_MTOW) + geom_text_repel(aes(x=Wingspan, y=Max_Takeoff_Weight, label = Aircraft_Type, color=New_AC), max.time=5, max.overlaps = Inf, show.legend=F) +
      geom_point(aes(x=Wingspan, y=Max_Takeoff_Weight, color=New_AC)) +
      labs(title = paste("Wingspan vs Max Takeoff Weight for each Aircraft Type (Wake Cat ",wake,")",sep=""), x="Wingspan (m)", y="Max Takeoff Weight (kg)", color="Adaptation")
    ggsave(paste("Wingspan_vs_MTOW_Wake_",wake,".png",sep=""),device="png",path=plot_dir)
  }
}


#plots of predicted vref vs wake category vref
predicted_aircraft <- ac_characteristics %>% filter(New_AC=="New Schipol")
ggplot(predicted_aircraft) + geom_point(aes(x=vref_MTOW, y=vref_Wake)) +
  geom_text_repel(aes(x=vref_MTOW, y=vref_Wake, label=Aircraft_Type), show.legend=F) +
  geom_abline(slope=1) +
  geom_abline(slope=1, intercept = -5, linetype="dashed") +
  labs(title = "Predicted vref vs Wake Category Vref (Max Takeoff Weight Model)", x="Predicted Vref (kts)", y="Vref by wake (kts)")
ggsave(paste("MTOW_Wake_Cat_Vref",".png",sep=""), device="png", path = plot_dir)

ggplot(predicted_aircraft) + geom_point(aes(x=vref_Wingspan_Sq, y=vref_Wake)) +
  geom_text_repel(aes(x=vref_Wingspan_Sq, y=vref_Wake, label=Aircraft_Type), show.legend=F) +
  geom_abline(slope=1) +
  geom_abline(slope=1, intercept = -5, linetype="dashed")  +
  labs(title = "Predicted Vref vs Wake Category vref (Wingspan Squared/Weight Model)", x="Predicted Vref (kts)", y="Vref by wake (kts)")
ggsave(paste("Wingspan_Sq_Wake_Cat_Vref",".png",sep=""), device="png", path = plot_dir)



#--------------------------------------------------------------------------------------------------------------
#A339 Model
#--------------------------------------------------------------------------------------------------------------
ac_characteristics_wake <- ac_characteristics %>% filter(Aircraft_Type %in% c("A332","A333","A339", "A343", "A359", "A35K")) %>% filter(New_AC == "Current Schipol")
wake = "Airbus"
#MTOW model
lm_MTOW <- lm(vref_AC_Type ~ Max_Takeoff_Weight, data = ac_characteristics_wake)

#Wingspan Squared/MTOW model
lm_Wingspan_Sq <- lm(vref_AC_Type ~ Wingspan_Sq_Weight, data = ac_characteristics_wake)

#MTOW predictions
predicted_MTOW <- data.frame(Max_Takeoff_Weight = seq(min(ac_characteristics_wake$Max_Takeoff_Weight)-10, max(ac_characteristics_wake$Max_Takeoff_Weight), length.out = 100))
temp_predicted <- predict(lm_MTOW, newdata = predicted_MTOW, interval = "confidence")

predicted_MTOW$vref <- temp_predicted[,1] #predicted value
predicted_MTOW$lci <- temp_predicted[,2] #lower confidence interval
predicted_MTOW$uci <- temp_predicted[,3] #upper confidence interval

ac_characteristics_MTOW <- ac_characteristics %>% filter(Aircraft_Type %in% c("A332","A333","A339", "A343", "A359", "A35K")) %>% rowwise %>%
  mutate(vref_AC_Type = ifelse(New_AC=="New Schipol", predict(lm_MTOW, newdata = data.frame(Max_Takeoff_Weight = c(Max_Takeoff_Weight))),vref_AC_Type)) %>%
  mutate(predicted_vref = predict(lm_MTOW, newdata = data.frame(Max_Takeoff_Weight=c(Max_Takeoff_Weight))))

#add MTOW predictions to main table
ac_characteristics <- ac_characteristics %>% mutate(vref_MTOW = ifelse(Aircraft_Type %in% c("A332","A333","A339", "A343", "A359", "A35K") & New_AC=="New Schipol",predict(lm_MTOW, newdata = data.frame(Max_Takeoff_Weight=c(Max_Takeoff_Weight))),vref_MTOW))

#Wingspan Squared/Weight predictions
predicted_Wingspan_Sq <- data.frame(Wingspan_Sq_Weight = seq(min(ac_characteristics_wake$Wingspan_Sq_Weight), max(ac_characteristics_wake$Wingspan_Sq_Weight), length.out = 100))
temp_predicted <- predict(lm_Wingspan_Sq, newdata = predicted_Wingspan_Sq, interval = "confidence")

predicted_Wingspan_Sq$vref <- temp_predicted[,1] #predicted value
predicted_Wingspan_Sq$lci <- temp_predicted[,2] #lower confidence interval
predicted_Wingspan_Sq$uci <- temp_predicted[,3] #upper confidence interval

ac_characteristics_Wingspan_Sq <- ac_characteristics %>% filter(Aircraft_Type %in% c("A332","A333","A339", "A343", "A359", "A35K")) %>% rowwise %>%
  mutate(vref_AC_Type = ifelse(New_AC=="New Schipol", predict(lm_Wingspan_Sq, newdata = data.frame(Wingspan_Sq_Weight = c(Wingspan_Sq_Weight))),vref_AC_Type)) %>%
  mutate(predicted_vref = predict(lm_Wingspan_Sq, newdata = data.frame(Wingspan_Sq_Weight=c(Wingspan_Sq_Weight))))

#add Wingspan Squared/Weight predictions to main table
ac_characteristics <- ac_characteristics %>% mutate(vref_Wingspan_Sq = ifelse(Aircraft_Type %in% c("A332","A333","A339", "A343", "A359", "A35K") & New_AC=="New Schipol",predict(lm_Wingspan_Sq, newdata = data.frame(Wingspan_Sq_Weight=c(Wingspan_Sq_Weight))),vref_Wingspan_Sq))

#assigns variables for each wake category for use in plots later
assign(paste("lm_MTOW_",wake, sep=""), lm_MTOW)
assign(paste("lm_Wingspan_Sq_",wake, sep=""), lm_Wingspan_Sq)
assign(paste("summary_MTOW_",wake, sep=""), summary(lm_MTOW))
assign(paste("summary_Wingspan_Sq_",wake, sep=""), summary(lm_Wingspan_Sq))
assign(paste("ac_characteristics_MTOW_",wake, sep=""), ac_characteristics_MTOW)
assign(paste("ac_characteristics_Wingspan_Sq_",wake, sep=""), ac_characteristics_Wingspan_Sq)
assign(paste("predicted_MTOW_",wake, sep=""), predicted_MTOW)
assign(paste("predicted_Wingspan_Sq_",wake, sep=""), predicted_Wingspan_Sq)

plot_dir <- paste(plot_dir_root,"/Airbus",sep="")

ac_characteristics_MTOW <- get(paste("ac_characteristics_MTOW_",wake,sep=""))
ac_characteristics_Wingspan_Sq <- get(paste("ac_characteristics_Wingspan_Sq_",wake,sep=""))
predicted_MTOW <- get(paste("predicted_MTOW_",wake,sep=""))
predicted_Wingspan_Sq <- get(paste("predicted_Wingspan_Sq_",wake,sep=""))
wake_vref <- ac_characteristics_MTOW$vref_Wake[1]

#plots with prediction line
model_wake_MTOW <- ac_characteristics_MTOW %>% filter(New_AC=="Current Schipol")
ggplot() + geom_point(data = model_wake_MTOW, aes(x=Max_Takeoff_Weight, y=vref_AC_Type)) +
  geom_line(data = predicted_MTOW, aes(x=Max_Takeoff_Weight, y=vref))+
  geom_smooth(data = predicted_MTOW, aes(x=Max_Takeoff_Weight, y=vref, ymin=lci, ymax=uci), stat="identity") +
  labs(title = "Vref Predicted by Max Takeoff Weight for Wake Cat B (Airbus)", x="Max Takeoff Weight (kg)", y="Vref (kts)", color="Adaptation")
ggsave(paste("MTOW_Model_Wake_",wake,".png",sep=""), device="png", path = plot_dir)

model_wake_Wingspan_Sq <- ac_characteristics_Wingspan_Sq %>% filter(New_AC=="Current Schipol")
ggplot() + geom_point(data = model_wake_Wingspan_Sq, aes(x=Wingspan_Sq_Weight, y=vref_AC_Type)) +
  geom_line(data = predicted_Wingspan_Sq, aes(x=Wingspan_Sq_Weight, y=vref))+
  geom_smooth(data = predicted_Wingspan_Sq, aes(x=Wingspan_Sq_Weight, y = vref, ymin = lci, ymax = uci), stat="identity")+
  labs(title = "Vref Predicted by Wingspan Squared/Weight for Wake Cat B (Airbus)", x="Wingspan Squared/Weight", y="Vref (kts)", color="Adaptation")
ggsave(paste("Wingspan_Sq_Model_Wake_",wake,".png",sep=""), device="png", path = plot_dir)    

#plots with aircraft labels
ggplot(ac_characteristics_MTOW) + geom_text_repel(aes(x=Max_Takeoff_Weight, y=vref_AC_Type, label = Aircraft_Type, color=New_AC), max.time=5, max.overlaps = Inf, show.legend=F) +
  geom_point(aes(x=Max_Takeoff_Weight, y=vref_AC_Type, color=New_AC)) +
  geom_abline(slope=0, intercept = wake_vref)+
  geom_abline(slope=0, intercept = wake_vref-5, linetype="dashed") +
  geom_text(data=data.frame(x=0,y=wake_vref), aes(x, y), label=paste("Vref for Wake B"," aircraft",sep=""), vjust=-.5, hjust = -.001) +
  geom_text(data=data.frame(x=0,y=wake_vref-5), aes(x, y), label=paste("Vref for Wake B"," aircraft -5kts",sep=""), vjust=-.5, hjust= -.001) +
  labs(title = "Vref by Max Takeoff Weight for Wake Cat B (Airbus)", x="Max Takeoff Weight (kg)", y="Vref (kts)", color="Adaptation")
ggsave(paste("MTOW_Aircraft_Wake_",wake,".png",sep=""), device="png", path = plot_dir)

ggplot(ac_characteristics_Wingspan_Sq) + geom_text_repel(aes(x=Wingspan_Sq_Weight, y=vref_AC_Type, label = Aircraft_Type, color=New_AC), max.time=5, max.overlaps = Inf, show.legend=F) +
  geom_point(aes(x=Wingspan_Sq_Weight, y=vref_AC_Type, color=New_AC)) +
  geom_abline(slope=0, intercept = wake_vref) +
  geom_abline(slope=0, intercept = wake_vref-5, linetype="dashed") +
  geom_text(data=data.frame(x=0,y=wake_vref), aes(x, y), label=paste("Vref for Wake B"," aircraft",sep=""), vjust=-.5, hjust = -0.001) +
  geom_text(data=data.frame(x=0,y=wake_vref-5), aes(x, y), label=paste("Vref for Wake B"," aircraft -5kts",sep=""), vjust=-.5, hjust = -0.001) +
  labs(title = "Vref by Wingspan Squared/Weight for Wake Cat B (Airbus)", x="Wingspan Squared/Weight", y="Vref (kts)", color="Adaptation")
ggsave(paste("Wingspan_Sq_Aircraft_Wake_",wake,".png",sep=""), device="png", path = plot_dir)

#plot of MTOW vs Wingspan
ggplot(ac_characteristics_MTOW) + geom_text_repel(aes(x=Wingspan, y=Max_Takeoff_Weight, label = Aircraft_Type, color=New_AC), max.time=5, max.overlaps = Inf, show.legend=F) +
  geom_point(aes(x=Wingspan, y=Max_Takeoff_Weight, color=New_AC)) +
  labs(title = "Wingspan vs Max Takeoff Weight for each Aircraft Type (Cat B - Airbus)", x="Wingspan (m)", y="Max Takeoff Weight (kg)", color="Adaptation")
ggsave(paste("Wingspan_vs_MTOW_Wake_",wake,".png",sep=""),device="png",path=plot_dir)



#------------------------------------------------------------------------------------------------
#unused plots
#------------------------------------------------------------------------------------------------
Testing <- T
if(Testing){
  wake <- "D&E"
  wake_vref_D = 130.6
  wake_vref_E = 125.7
  ac_characteristics_wake <- ac_characteristics %>% filter(Wake=="D" | Wake=="E") %>% filter(New_AC == "Current Schipol")
  
  #MTOW model
  lm_MTOW <- lm(vref_AC_Type ~ Max_Takeoff_Weight, data = ac_characteristics_wake)
  
  #Wingspan Squared/MTOW model
  lm_Wingspan_Sq <- lm(vref_AC_Type ~ Wingspan_Sq_Weight, data = ac_characteristics_wake)
  
  #MTOW predictions
  predicted_MTOW <- data.frame(Max_Takeoff_Weight = seq(min(ac_characteristics_wake$Max_Takeoff_Weight)-10, max(ac_characteristics_wake$Max_Takeoff_Weight), length.out = 100))
  temp_predicted <- predict(lm_MTOW, newdata = predicted_MTOW, interval = "confidence")
  
  predicted_MTOW$vref <- temp_predicted[,1] #predicted value
  predicted_MTOW$lci <- temp_predicted[,2] #lower confidence interval
  predicted_MTOW$uci <- temp_predicted[,3] #upper confidence interval
  
  ac_characteristics_MTOW <- ac_characteristics %>% filter(Wake=="D" | Wake=="E") %>% rowwise %>%
    mutate(vref_AC_Type = ifelse(New_AC=="New Schipol", predict(lm_MTOW, newdata = data.frame(Max_Takeoff_Weight = c(Max_Takeoff_Weight))),vref_AC_Type)) %>%
    mutate(predicted_vref = predict(lm_MTOW, newdata = data.frame(Max_Takeoff_Weight=c(Max_Takeoff_Weight))))
  
  #add MTOW predictions to main table
  ac_characteristics <- ac_characteristics %>% mutate(vref_MTOW = ifelse((Wake=="D" | Wake=="E") && New_AC=="New Schipol",predict(lm_MTOW, newdata = data.frame(Max_Takeoff_Weight=c(Max_Takeoff_Weight))),vref_MTOW))
  
  #Wingspan Squared/Weight predictions
  predicted_Wingspan_Sq <- data.frame(Wingspan_Sq_Weight = seq(min(ac_characteristics_wake$Wingspan_Sq_Weight), max(ac_characteristics_wake$Wingspan_Sq_Weight), length.out = 100))
  temp_predicted <- predict(lm_Wingspan_Sq, newdata = predicted_Wingspan_Sq, interval = "confidence")
  
  predicted_Wingspan_Sq$vref <- temp_predicted[,1] #predicted value
  predicted_Wingspan_Sq$lci <- temp_predicted[,2] #lower confidence interval
  predicted_Wingspan_Sq$uci <- temp_predicted[,3] #upper confidence interval
  
  ac_characteristics_Wingspan_Sq <- ac_characteristics %>% filter(Wake=="D" | Wake=="E") %>% rowwise %>%
    mutate(vref_AC_Type = ifelse(New_AC=="New Schipol", predict(lm_Wingspan_Sq, newdata = data.frame(Wingspan_Sq_Weight = c(Wingspan_Sq_Weight))),vref_AC_Type)) %>%
    mutate(predicted_vref = predict(lm_Wingspan_Sq, newdata = data.frame(Wingspan_Sq_Weight=c(Wingspan_Sq_Weight))))
  
  #add Wingspan Squared/Weight predictions to main table
  ac_characteristics <- ac_characteristics %>% mutate(vref_Wingspan_Sq = ifelse((Wake=="B" | Wake=="C") & New_AC=="New Schipol",predict(lm_Wingspan_Sq, newdata = data.frame(Wingspan_Sq_Weight=c(Wingspan_Sq_Weight))),vref_Wingspan_Sq))
  
  #assigns variables for each wake category for use in plots later
  assign(paste("lm_MTOW_",wake, sep=""), lm_MTOW)
  assign(paste("lm_Wingspan_Sq_",wake, sep=""), lm_Wingspan_Sq)
  assign(paste("summary_MTOW_",wake, sep=""), summary(lm_MTOW))
  assign(paste("summary_Wingspan_Sq_",wake, sep=""), summary(lm_Wingspan_Sq))
  assign(paste("ac_characteristics_MTOW_",wake, sep=""), ac_characteristics_MTOW)
  assign(paste("ac_characteristics_Wingspan_Sq_",wake, sep=""), ac_characteristics_Wingspan_Sq)
  assign(paste("predicted_MTOW_",wake, sep=""), predicted_MTOW)
  assign(paste("predicted_Wingspan_Sq_",wake, sep=""), predicted_Wingspan_Sq)
  
  plot_dir <- paste(plot_dir_root,"/D&E",sep="")
  
  ac_characteristics_MTOW <- get(paste("ac_characteristics_MTOW_",wake,sep=""))
  ac_characteristics_Wingspan_Sq <- get(paste("ac_characteristics_Wingspan_Sq_",wake,sep=""))
  predicted_MTOW <- get(paste("predicted_MTOW_",wake,sep=""))
  predicted_Wingspan_Sq <- get(paste("predicted_Wingspan_Sq_",wake,sep=""))
  wake_vref <- ac_characteristics_MTOW$vref_Wake[1]
  
  #plots with prediction line
  model_wake_MTOW <- ac_characteristics_MTOW %>% filter(New_AC=="Current Schipol")
  ggplot() + geom_point(data = model_wake_MTOW, aes(x=Max_Takeoff_Weight, y=vref_AC_Type)) +
    geom_line(data = predicted_MTOW, aes(x=Max_Takeoff_Weight, y=vref))+
    geom_smooth(data = predicted_MTOW, aes(x=Max_Takeoff_Weight, y=vref, ymin=lci, ymax=uci), stat="identity") +
    labs(title = paste("Vref Predicted by Max Takeoff Weight for Wake Cat ",wake,sep=""), x="Max Takeoff Weight (kg)", y="Vref (kts)", color="Adaptation")
  ggsave(paste("MTOW_Model_Wake_",wake,".png",sep=""), device="png", path = plot_dir)
  
  model_wake_Wingspan_Sq <- ac_characteristics_Wingspan_Sq %>% filter(New_AC=="Current Schipol")
  ggplot() + geom_point(data = model_wake_Wingspan_Sq, aes(x=Wingspan_Sq_Weight, y=vref_AC_Type)) +
    geom_line(data = predicted_Wingspan_Sq, aes(x=Wingspan_Sq_Weight, y=vref))+
    geom_smooth(data = predicted_Wingspan_Sq, aes(x=Wingspan_Sq_Weight, y = vref, ymin = lci, ymax = uci), stat="identity")+
    labs(title = paste("Vref Predicted by Wingspan Squared/Weight for Wake Cat ",wake,sep=""), x="Wingspan Squared/Weight", y="Vref (kts)", color="Adaptation")
  ggsave(paste("Wingspan_Sq_Model_Wake_",wake,".png",sep=""), device="png", path = plot_dir)    
  
  #plots with aircraft labels
  ggplot(ac_characteristics_MTOW) + geom_text_repel(aes(x=Max_Takeoff_Weight, y=vref_AC_Type, label = Aircraft_Type, color=New_AC), max.time=5, max.overlaps = Inf, show.legend=F) +
    geom_point(aes(x=Max_Takeoff_Weight, y=vref_AC_Type, color=New_AC)) +
    geom_abline(slope=0, intercept = wake_vref_D)+
    geom_abline(slope=0, intercept = wake_vref_E) +
    geom_text(data=data.frame(x=0,y=wake_vref_D), aes(x, y), label=paste("Vref for Wake D"," aircraft",sep=""), vjust=-.5, hjust = -.001) +
    geom_text(data=data.frame(x=0,y=wake_vref_E), aes(x, y), label=paste("Vref for Wake E"," aircraft",sep=""), vjust=-.5, hjust= -.001) +
    labs(title = paste("Vref by Max Takeoff Weight for Wake Cat ",wake," (Aircraft Labelled)",sep=""), x="Max Takeoff Weight (kg)", y="Vref (kts)", color="Adaptation")
  ggsave(paste("MTOW_Aircraft_Wake_",wake,".png",sep=""), device="png", path = plot_dir)
  
  ggplot(ac_characteristics_Wingspan_Sq) + geom_text_repel(aes(x=Wingspan_Sq_Weight, y=vref_AC_Type, label = Aircraft_Type, color=New_AC), max.time=5, max.overlaps = Inf, show.legend=F) +
    geom_point(aes(x=Wingspan_Sq_Weight, y=vref_AC_Type, color=New_AC)) +
    geom_abline(slope=0, intercept = wake_vref_D) +
    geom_abline(slope=0, intercept = wake_vref_E) +
    geom_text(data=data.frame(x=0,y=wake_vref_D), aes(x, y), label=paste("Vref for Wake D"," aircraft",sep=""), vjust=-.5, hjust = -0.001) +
    geom_text(data=data.frame(x=0,y=wake_vref_E), aes(x, y), label=paste("Vref for Wake E"," aircraft",sep=""), vjust=-.5, hjust = -0.001) +
    labs(title = paste("Vref by Wingspan Squared/Weight for Wake Cat ",wake, " (Aircraft Labelled)",sep=""), x="Wingspan Squared/Weight", y="Vref (kts)", color="Adaptation")
  ggsave(paste("Wingspan_Sq_Aircraft_Wake_",wake,".png",sep=""), device="png", path = plot_dir)
  
  #plot of MTOW vs Wingspan
  ggplot(ac_characteristics_MTOW) + geom_text_repel(aes(x=Wingspan, y=Max_Takeoff_Weight, label = Aircraft_Type, color=New_AC), max.time=5, max.overlaps = Inf, show.legend=F) +
    geom_point(aes(x=Wingspan, y=Max_Takeoff_Weight, color=New_AC)) +
    labs(title = paste("Wingspan vs Max Takeoff Weight for each Aircraft Type (Wake Cat ",wake,")",sep=""), x="Wingspan (m)", y="Max Takeoff Weight (kg)", color="Adaptation")
  ggsave(paste("Wingspan_vs_MTOW_Wake_",wake,".png",sep=""),device="png",path=plot_dir)
  
}



