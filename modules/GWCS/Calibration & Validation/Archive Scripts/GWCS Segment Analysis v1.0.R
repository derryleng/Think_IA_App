# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  IA GWCS Segment Exclusion Analysis                    #
#                |                                                        #
# Version No.    |  1.0                                                   #
#                |                                                        #
# Date Modified  |  12/01/2021                                            #
#                |                                                        #
# Author(s)      |  Michael Cowham                                        #
#                |                                                        #
# Project        |  IA (NavCan)                                           #
#                |                                                        #
# Purpose        |  GWCS Analysis - check segment exclusion               #
#                |                                                        #
# ----------------------------------------------------------------------- #

# Version History ------------------------------------------------------- #
#
# 1 Initial Script
#
# ----------------------------------------------------------------------- #

# ----------------------------------------------------------------------- #
# Load Packages ----------------------------------------------------------
# ----------------------------------------------------------------------- #

rm(list = ls())

library(RODBC) 
library(ggplot2)
library(lattice)
library(RColorBrewer)
library(gridExtra)
library(dplyr)
library(data.table)

# ----------------------------------------------------------------------- #
# Parameters   ----------------------------------------------------------
# ----------------------------------------------------------------------- #

version <- "v1.0"

database <- "NavCan_TBS"
database2 <- "NavCan_UTMA_Validation_DB2"

# ----------------------------------------------------------------------- #
# Initialisation --------------------------------------------------------
# ----------------------------------------------------------------------- #


# Set working directory.

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
base_dir <- getwd()
out_dir <- file.path(dirname(base_dir), "Outputs", "GWCS", "Segment Analysis", version)

# Create Output directory

if (!dir.exists(out_dir)) dir.create(out_dir)

# Database Connection

con <- odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", "192.168.1.23", database, "ruser", "Th!nkruser"
))

con2 <- odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", "192.168.1.39", database2, "ruser", "Th!nkruser"
))


# ----------------------------------------------------------------------- #
# Load Data      --------------------------------------------------------
# ----------------------------------------------------------------------- #

wind_seg <- sqlQuery(con, "SELECT * FROM vw_Mode_S_Wind_Seg")
wind_adaptation <- sqlQuery(con, "SELECT * FROM vw_Mode_S_Wind_Adaptation")
wind_seg2 <- sqlQuery(con2, "SELECT * FROM vw_Mode_S_Wind_Seg")

# ----------------------------------------------------------------------- #
# Analysis      --------------------------------------------------------
# ----------------------------------------------------------------------- #

# Create new fields

wind_seg <- mutate(wind_seg, Diff_Mode_S_To_Radar_GSPD = Ave_Track_SPD - Ave_Mode_S_GSPD)

# Rate of Segment Exclusions by DME Overall

ex_dme <- wind_seg %>% group_by(DME_Seg) %>% summarise(TotalN = n(), Total_Invalid = sum(Global_Flag, na.rm = T)) %>% ungroup() %>% mutate(PC_Invalid = 100 * Total_Invalid / TotalN)

# Rate of Segment Exclusions by Flags

ex_dme_flag <- wind_seg %>% group_by(DME_Seg) %>% summarise(TotalN = n(), Total_Seg_Duration = sum(Seg_Duration_Flag, na.rm = T),
                                                                          Total_Track_To_Runway_HDG = sum(Diff_Track_To_Runway_HDG_Flag, na.rm = T),
                                                                          Total_HDG_To_Runway_HDG = sum(Diff_HDG_To_Runway_HDG_Flag, na.rm = T),
                                                                          Total_Mode_S_To_Radar_Track = sum(Diff_Mode_S_To_Radar_Track_Flag, na.rm = T),
                                                                          Total_Mode_S_To_Radar_GSPD = sum(Diff_Mode_S_To_Radar_GSPD_Flag, na.rm = T),
                                                                          Total_Max_Wind_Effect = sum(Max_Wind_Effect_Flag, na.rm = T),
                                                                          Total_Max_Wind_SPD = sum(Max_Wind_SPD_Flag, na.rm = T)) %>% ungroup() %>%
                                                  mutate(PC_Seg_Duration = 100 * Total_Seg_Duration / TotalN,
                                                         PC_Track_To_Runway_HDG = 100 * Total_Track_To_Runway_HDG / TotalN,
                                                         PC_HDG_To_Runway_HDG = 100 * Total_HDG_To_Runway_HDG / TotalN,
                                                         PC_Mode_S_To_Radar_Track = 100 *  Total_Mode_S_To_Radar_Track / TotalN,
                                                         PC_Mode_S_To_Radar_GSPD = 100 *  Total_Mode_S_To_Radar_GSPD / TotalN,
                                                         PC_Total_Max_Wind_Effect = 100 * Total_Max_Wind_Effect / TotalN,
                                                         PC_Max_Wind_SPD = 100 * Total_Max_Wind_SPD / TotalN)

# ----------------------------------------------------------------------- #
# Ad-Hoc Analysis   -----------------------------------------------------
# ----------------------------------------------------------------------- #

DME_Min <- 0
DME_Max <- 9

# Check the Mode S GSPD to Radar Track Speed Differences
# Not too much evidence of problems in Seg 0 - coasting etc.

ggplot(data = filter(wind_seg, DME_Seg >= DME_Min, DME_Seg < DME_Max))+
  geom_histogram(mapping = aes(x = Diff_Mode_S_To_Radar_GSPD, y = ..density..), binwidth = 1, color = "black", fill = "blue")+
  geom_vline(xintercept = -wind_adaptation$Diff_Mode_S_To_Radar_GSPD_Max)+
  geom_vline(xintercept = wind_adaptation$Diff_Mode_S_To_Radar_GSPD_Max)+
  facet_wrap(~DME_Seg)+
  theme_classic()

# Check the Segment Duration versus Flown Distance

ggplot(data = filter(wind_seg, DME_Seg < DME_Max))+
  geom_point(mapping = aes(x = Max_RTT - Min_RTT, y = Max_Track_Time - Min_Track_Time, color = as.factor(Seg_Duration_Flag)))+
  ylim(0, 30)+
  facet_wrap(~DME_Seg)+
  theme_classic()

# Check ths Min_RTT in segment 0
# Looks worse for all except R24L/R.  Did Jason say coverage not as good for R33?

ggplot(data = filter(wind_seg, DME_Seg == 0))+
  geom_histogram(mapping = aes(x = Min_RTT, y = ..density..), binwidth = 0.1, color = "black", fill = "blue")+
  facet_wrap(~Landing_Runway)+
  theme_classic()

# Recommendations

# Decrease the minimum segment duration to 9s as per LVNL.  Improve rate for Seg0
# Increase the Diff_Mode_S_To_Radar_GSPD value to 10kt as per LVNL as well.
#

# ----------------------------------------------------------------------- #
# Segment Availability Comparison ---------------------------------------
# ----------------------------------------------------------------------- #

seg1 <- wind_seg %>% filter(DME_Seg <= 19) %>% group_by(DME_Seg) %>% summarise(Total_Valid = n() - sum(Global_Flag, na.rm = T)) %>% ungroup() %>% mutate(PC_Available =  100 * Total_Valid / max(Total_Valid), Data_Set = "TBS")
seg2 <- wind_seg2 %>% filter(DME_Seg <= 19) %>% group_by(DME_Seg) %>% summarise(Total_Valid = n() - sum(Global_Flag, na.rm = T)) %>% ungroup() %>% mutate(PC_Available =  100 * Total_Valid / max(Total_Valid), Data_Set = "DBS")

seg1 <- wind_seg %>% filter(DME_Seg <= 19) %>% group_by(DME_Seg) %>% summarise(Total = n()) %>% ungroup() %>% mutate(PC_Available =  100 * Total / max(Total), Data_Set = "TBS")
seg2 <- wind_seg2 %>% filter(DME_Seg <= 19) %>% group_by(DME_Seg) %>% summarise(Total = n()) %>% ungroup() %>% mutate(PC_Available =  100 * Total / max(Total), Data_Set = "DBS")


segs <- rbind(seg1, seg2)



p1 <- ggplot(data = segs)+
  geom_line(mapping = aes(x = DME_Seg, y = PC_Available, color = Data_Set), size = 1)+
  scale_color_brewer(palette = "Set1")+
  labs(title = "Segment Availability by Distance to Threshold", x = "Distance to Threshold (NM)", y = "Percentage Available")+
  scale_x_continuous(breaks = seq(0, 20, 1))+
  scale_y_continuous(breaks = seq(0, 100, 10))+
  theme_bw()

png(file.path(out_dir, "Segment_Availability_DTT.png"),  width = 900, height = 500)
print(p1)
dev.off()
