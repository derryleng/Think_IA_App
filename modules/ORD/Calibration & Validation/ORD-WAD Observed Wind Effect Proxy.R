
#con <- Get_DBI_Connection(IP = "192.", Database = "NavCan_TBS_V3")

### Adaptation


### Data

ORD_VV <- dbGetQuery(con, "SELECT * FROM vw_ORD_Validation_View", stringsAsFactors = F)
WAD_VV <- dbGetQuery(con, "SELECT * FROM vw_WAD_Validation_View", stringsAsFactors = F)

### Save Copies
RadarOrig <- Radar
ORD_VV_Orig <- ORD_VV
WAD_VV_Orig <- WAD_VV

# ---------- #
### RESET
Radar <- RadarOrig
ORD_VV <- ORD_VV_Orig
WAD_VV <- WAD_VV_Orig

### Recalculate Radar


### ORD


### WAD
RTTPathLegs <- c("ILS_Leg", "Landing_Leg", "Intercept_Leg", "Extended_Intercept")
WEPathLegs <- c("Intercept_Leg", "Extended_Intercept")
MaxILSRange <- 4
FAF_Distance_Val <- 4.5
MaxInsideBuffer <- 2
Radar <- dbGetQuery(con, GetORDAnalysisRadarQuery(), stringsAsFactors = F)
Radar <- RecalculateRadarValuesORD(Radar, RTTPathLegs, WEPathLegs, MaxILSRange)
WAD_VV <- GenerateProxyWindEffect(WAD_VV, Radar, Algo = "WAD", LorFIn = "Follower", LorFOut = "Follower", MaxInsideBuffer, FAF_Distance_Val, Remove_Old_Observations)
WAD_VV <- GenerateProxyWindEffect(WAD_VV, Radar, Algo = "WAD", LorFIn = "Leader", LorFOut = "Follower", MaxInsideBuffer, FAF_Distance_Val, Remove_Old_Observations)
WAD_VV <- ORDRealignFlags(WAD_VV)
WADSummary <- QuickProxyTablePlot(WAD_VV, "WAD", "Summary")
