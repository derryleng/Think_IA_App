


Plot_Values <- c(3, 4, 5, 6, 7, 8)

# -------------------------------------------------------------------------------------------------- #
# GWCS Stage 2 Filter Plot
# -------------------------------------------------------------------------------------------------- #

## Plot to Look at the GWCS Stage 2 Filters
FT_Flag_Plot_Dir <- file.path(Adap_Iteration_Dir, paste0("GWCS Stage 2 Flag Statistics v", Adap_Iteration_Version, ".png"))
if (!file.exists(FT_Flag_Plot_Dir)){
  FT_Flag_Investigation <- FT_Data %>%
    select(Mode_S_Wind_Seg_ID,
           DME_Seg,
           Seg_Duration_Flag,
           Diff_Track_To_Runway_HDG_Flag,
           Diff_HDG_To_Runway_HDG_Flag,
           Diff_Mode_S_To_Radar_Track_Flag,
           Diff_Mode_S_To_Radar_GSPD_Flag,
           Max_Wind_Effect_Flag,
           Max_Wind_SPD_Flag,
           Global_Flag)
  FT_Flag_Investigation <- pivot_longer(FT_Flag_Investigation, 3:ncol(FT_Flag_Investigation), values_to = "Flag_Value", names_to = "Flag") %>%
    group_by(DME_Seg, Flag) %>% summarise(Flag_Count = sum(Flag_Value, na.rm = T)) %>% ungroup()
  FT_Flag_Investigation <- filter(FT_Flag_Investigation, Flag != "Global_Flag")
  png(FT_Flag_Plot_Dir, width = 1500, height = 1000)
  plot <- ggplot(data = FT_Flag_Investigation) + geom_col(mapping = aes(x = factor(DME_Seg), y = Flag_Count)) + facet_wrap(~Flag, nrow = 2) + labs(
    title = "Stage 2 Filter Counts by DME Segment",
    subtitle = paste0(Operation, " ", Airfield, " v", Adap_Iteration_Version),
    x = "DME Segment (NM)",
    y = "Count"
  )
  print(plot)
  dev.off()
}

# -------------------------------------------------------------------------------------------------- #
# Existing Boxplots from v2.9
# -------------------------------------------------------------------------------------------------- #

# use bplot (Per Wake Cat, All Wind vs SHW) - Crude
for (wake in c("A", "B", "C", "D", "E", "F", "G")){
  a <- bplot(IAS_Values, wake, 0)
  b <- bplot(IAS_Values, wake, 1)
  png(file.path(Plot_Dir, paste0("IAS Boxplot ", wake, " v", Local_Iteration_Version,".png")))
  grid.arrange(a, b)
  dev.off()
  FileName <- paste0("IAS Boxplot (ACT, Type ", wake, ") v", Local_Iteration_Version, ".png")
  png(file.path(Plot_Dir, FileName), width = 1100, height = 600)
  print(IAS_Aircraft_Boxplot(IAS_Values, wake, Airfield, Local_Iteration_Version))
  dev.off()
}

# IAS by surface headwind group
png(file.path(Plot_Dir, paste0("IAS by Separation Distance and Surface Headwind Group v", Local_Iteration_Version, ".png")))
ggplot(data = filter(IAS_Values, Separation_Distance %in% Plot_Values), mapping = aes(x = factor(Separation_Distance), y = Ave_SPD)) + facet_wrap(~Surface_Headwind_Group) +
  geom_boxplot(aes(group = factor(Separation_Distance), fill = factor(Separation_Distance))) +
  labs(x = "Separation Distance (NM)", y = "IAS (kts)", title = "IAS by Separation Distance and Surface Headwind Group") +
  theme(text = element_text(size=8), legend.position = "none")
dev.off()

All_0DME_Wake <- FT_Boxplot_General(IAS_Values_Primary, 0, 0, "Wake", 50, 250)
SHW_0DME_Wake <- FT_Boxplot_General(IAS_Values_Primary, 0, 1, "Wake", 50, 250)
SW_0DME_Wake <- FT_Boxplot_General(IAS_Values_Primary, 0, 2, "Wake", 50, 250)
SHW_1DME_Wake <- FT_Boxplot_General(IAS_Values_Secondary, 1, 1, "Wake", 50, 250)
SHW_0DME_Runway <- FT_Boxplot_General(IAS_Values_Primary, 0, 1, "Runway", 50, 250)

print(All_0DME_Wake)

png(file.path(Plot_Dir, "FT Boxplot (All v Low SHW).png"),  width = 1100, height = 960)
grid.arrange(All_0DME_Wake, SHW_0DME_Wake)
dev.off()

png(file.path(Plot_Dir, "FT Boxplot (SHW v SWS).png"),  width = 1100, height = 960)
grid.arrange(SHW_0DME_Wake, SW_0DME_Wake)
dev.off()

png(file.path(Plot_Dir, "FT Boxplot (0DME v 1DME).png"),  width = 1100, height = 960)
grid.arrange(SHW_0DME_Wake, SHW_1DME_Wake)
dev.off()

# -------------------------------------------------------------------------------------------------- #
# MC Additions 
# -------------------------------------------------------------------------------------------------- #

# Additional analysis by Landing Runway
Summary_Wake_Runway <- Create_Summary_Main(IAS_Values, "Wake_Cat", "Runway", NA, Case_Var, Delivery_Chosen) 
Summary_Wake_Join <- select(Summary_Wake, Wake_Cat, Separation_Distance, !!sym(Case_Var), Count, Median_Speed)
Summary_Wake_Runway <- Summary_Wake_Runway %>%
  #inner_join(Summary_Wake_Join, by = setNames(c("Wake_Cat", "Wake_Cat"), c("Separation_Distance", "Separation_Distance"), c(Case_Var, Case_Var))) %>%
  inner_join(Summary_Wake_Join, by = setNames(c("Wake_Cat", "Separation_Distance", Case_Var), c("Wake_Cat", "Separation_Distance", Case_Var))) %>%
  rename(Median_Speed_Runway = Median_Speed.x, Median_Speed = Median_Speed.y) %>%
  mutate(Median_Speed_Diff = round(Median_Speed_Runway - Median_Speed, 1))
fwrite(Summary_Wake_Runway, file.path(Local_Iteration_Dir, paste0("IAS Runway Comparison v", Local_Iteration_Version, ".csv")))

# -------------------------------------------------------------------------------------------------- #
# Follower IAS Distributions
# -------------------------------------------------------------------------------------------------- #

### Wake Follower WTC/Sep Distance Speed Distributions.

# Set the Speeds & Config (assumes entire FTA Hub is run)
SpeedsPlot <- IAS_Values
ConfigPlot <- Wake_Adaptation_Speeds_Wake

# Remove any Duplicates.
Wake_Dist_Plot <- select(Recat_Wake_Dist, -Leader_WTC) %>% unique()
DistPath <- file.path(Plot_Dir, "IAS Distributions")
Create_Directory(DistPath)

# Plot All v Ref Wind IAS (against ref value) for all in Wake Dist
for (i in 1:nrow(Wake_Dist_Plot)){
  
  # Get the Sep Dist/Follower WTC
  FollowerWTC <- Wake_Dist_Plot$Follower_WTC[i]
  SepDist <- Wake_Dist_Plot$Reference_Wake_Separation_Distance[i]
  
  # Set up PNG object.
  Plot1 <- PlotAgainstReferenceFTA(Data = SpeedsPlot, Reference = ConfigPlot, RefDists = Recat_Wake_Dist, 
                                   PlotVar = "Ave_SPD", SepDist, FollowerWTC, Colour = "magenta", 
                                   Unit = "IAS", RefWinds = "None")
  Plot2 <- PlotAgainstReferenceFTA(Data = SpeedsPlot, Reference = ConfigPlot, RefDists = Recat_Wake_Dist, 
                                   PlotVar = "Ave_SPD", SepDist, FollowerWTC, Colour = "magenta", 
                                   Unit = "IAS", RefWinds = "SHW")
  png(file.path(DistPath, paste0(FollowerWTC, "-", SepDist, " Average IAS (Ref v All Winds)", ".png")), width = 720, height = 720)
  grid.arrange(Plot1, Plot2)
  dev.off()
  
}

# -------------------------------------------------------------------------------------------------- #
# Temp Analysis
# -------------------------------------------------------------------------------------------------- #

### Quick investigation into aircraft weighting changes
### CODE WILL ONLY WORK IF VERSION IS SET TO 7.0-1-1

# Min_Percent_Diff <- 5
# Min_Old_Obs <- 100
# setwd(Adaptation_Compare_Dir)
# Temp <- fread("Weightings v7.0-1-1 & v6.0-0-0.csv")
# Temp <- arrange(Temp, Follower_WTC, Separation_Distance, Weighting_Diff)
# Temp <- select(Temp, -Separation_Distance) %>% unique()
# Temp <- group_by(Temp, Aircraft_Type) %>% mutate(ID = row_number()) %>% ungroup() %>% filter(ID == 1)
# Temp <- select(Temp, -ID)
# Temp <- arrange(Temp, desc(Weighting_Diff))
# Temp <- filter(Temp, Weighting_Diff >= Min_Percent_Diff | (Weighting_New == 0 & Count_Old >= Min_Old_Obs))
# fwrite(Temp, "Significant ACT Weighting Changes.csv")



