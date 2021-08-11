# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  Machine Learning Analysis                             #
# Version No.    |  1.0                                                   #
# Date Modified  |  28/03/2021                                            #
# Author(s)      |  Dan Brown                                             #
# Project        |  PWS                                                   #
# Purpose        |  Generate different types of machine learning models   #
#                |  to analyse the effects of different variables on      #
#                |  landing stabilisation speed                           #
#                |                                                        #
# ----------------------------------------------------------------------- #

rm(list = ls())

library(data.table)
library(lubridate)
library(getPass)
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)


#Use a version number derived from date or define manually
version <- paste0(Sys.Date(), " ","V1.0 (AH)")
version <- "2021-08-04 V1.0 (DB)"

use_same_input_version <- T

if (use_same_input_version == T) {
  input_version <- version
} else if (use_same_input_version == F) {
  input_version <- "2021-07-16 V1.0 (AH)"   #Manually define this if you want different input output version numbers
}

#Find location of script and functions file based for running in shiny or in RSTUDIO

# --------------------------------------------------------------------------- #
ModuleFolder <- "ORD"
ModuleSubfolder <- "Calibration & Validation"
Script_out <- "Radar ORD Analysis"
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


project <- GetProjectID()

#IorO for "Ouputs" or "Inputs" directory, must be a string as used in directory
Base_Dir <- GetSaveDirectory(Project = project, Algorithm = ModuleFolder, IorO = "Outputs")
Create_Directory(Base_Dir)

# out_data <- Base_Dir
ord_dir <- Base_Dir

speed_prof_data <- file.path(ord_dir, "Radar ORD Analysis", input_version)

out_data <- file.path(ord_dir, "Radar ORD Analysis", version)
Create_Directory(out_data)

ml_data <- file.path(out_data,"Machine Learning Outputs")
Create_Directory(ml_data)

###############################################################################
# Initialisation

Speed_Prof_Errors <- read_csv(file.path(speed_prof_data,"Speed_Prof_Errors.csv"),col_types = cols(.default = "?", Gust_Valid = col_character()))
mean_hw <- mean(Speed_Prof_Errors$Surface_Headwind)
mean_qnh <- mean(Speed_Prof_Errors$Baro_Pressure)
mean_gust <- mean(Speed_Prof_Errors$Gust, na.rm=T)

operator_counts <- Speed_Prof_Errors %>% group_by(Operator) %>% summarise(n())

#regular linear regression model
lm_model <- lm(a1 ~ Surface_Headwind + Gust + Baro_Pressure, data=Speed_Prof_Errors)

#mixed effects model
mixed_model <- lmer(a1 ~ Surface_Headwind + Gust + Baro_Pressure + (1 | Operator), data=Speed_Prof_Errors)
summary_mixed <- summary(mixed_model)

#coefficients of mixed effects model
coef_mixed <- coef(mixed_model)$Operator
operators <- rownames(ranef_mixed)

#plotting data
plot_data <- cbind(operators,coef_mixed)
rownames(plot_data) <- NULL

plot_data$Intercept <- plot_data$`(Intercept)`+mean_hw*coef_mixed$Surface_Headwind[1]+mean_qnh*coef_mixed$Baro_Pressure[1] + mean_gust*coef_mixed$Gust[1]
plot_data$Intercept_Gust <- plot_data$`(Intercept)`+mean_hw*coef_mixed$Surface_Headwind[1]+mean_qnh*coef_mixed$Baro_Pressure[1]
plot_data$Intercept_Surface_Headwind <- plot_data$`(Intercept)`+mean_gust*coef_mixed$Gust[1]+mean_qnh*coef_mixed$Baro_Pressure[1]
plot_data <- plot_data %>% left_join(operator_counts, by=c("operators"="Operator")) %>% filter(`n()`>10) %>%
              arrange(Intercept) %>%
              mutate(operators = factor(operators, levels =operators))

plot_sample <- Speed_Prof_Errors %>% left_join(operator_counts, by=c("Operator")) %>%
                group_by(Operator) %>% sample_n(100, replace=TRUE)

###############################################################################
# Plots

ggplot(plot_data) + geom_point(aes(x=operators, y=Intercept)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(plot_data) + geom_abline(aes(slope=Gust, intercept=Intercept_Gust, color=operators),show.legend = FALSE) + geom_point(data = plot_sample, aes(x=Gust,y=a1, color=Operator, alpha=I(1)), position = position_jitter(), show.legend = F)

ggplot(plot_data) + geom_abline(aes(slope=Surface_Headwind, intercept=Intercept_Surface_Headwind, color=operators),show.legend = FALSE) + geom_point(data = plot_sample, aes(x=Surface_Headwind,y=a1, color=Operator, alpha=I(1)), position = position_jitter(), show.legend = F)

ggplot(plot_data%>%filter(operators=='BAW')) +geom_abline(aes(slope=Gust,intercept=Intercept_Gust))+geom_point(data=Speed_Prof_Errors%>%filter(Operator=='BAW'),aes(x=Gust,y=a1))

ggplot(plot_data%>%filter(operators=='ROT')) +geom_abline(aes(slope=Gust,intercept=Intercept_Gust))+geom_point(data=Speed_Prof_Errors%>%filter(Operator=='ROT'),aes(x=Gust,y=a1))

ggplot(plot_data%>%filter(operators=='ABW')) +geom_abline(aes(slope=Gust,intercept=Intercept_Gust))+geom_point(data=Speed_Prof_Errors%>%filter(Operator=='ABW'),aes(x=Gust,y=a1))
