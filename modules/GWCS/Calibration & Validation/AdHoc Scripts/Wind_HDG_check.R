library(RODBC) 
library(data.table)

user <- "Catherine"


# Connect to database
database <- "NavCan_TBS"

con <- odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", "192.168.1.23", database, "ruser", "Th!nkruser"
))


# Read in the data 
fta <-as.data.table(sqlQuery(con, "SELECT * FROM vw_Flying_Time_Analysis"))
anem <- as.data.table(sqlQuery(con, "SELECT 
                               Landing_Runway,
                               Surface_Wind_SPD = Anemo_SPD / dbo.fnc_GI_Kts_To_M_Per_Sec(),
                               Surface_Wind_HDG = Anemo_HDG / dbo.fnc_GI_Degs_To_Rads()
                               FROM tbl_Anemometer"))
rawsegs <- as.data.table(sqlQuery(con, "SELECT * FROM vw_Mode_S_Wind_Seg"))


# Processing
#calculates reciprocal of wind direction to change to conventional notation (direction from)
rawsegs1 <- subset(rawsegs, Ave_Wind_HDG > 180)
rawsegs2 <- subset(rawsegs, Ave_Wind_HDG < 180)

rawsegs1$Ave_Wind_HDG <- rawsegs1$Ave_Wind_HDG - 180
rawsegs2$Ave_Wind_HDG <- rawsegs2$Ave_Wind_HDG + 180

rawsegs<- rbind(rawsegs1, rawsegs2)


# Plot
# Wind heading per runway
library(ggplot2)
ggplot(fta, aes(fta$Surface_Wind_HDG)) +
  geom_histogram()+
  facet_wrap(~Landing_Runway, scales = 'free')

ggplot(anem, aes(anem$Surface_Wind_HDG)) +
  geom_histogram()+
  facet_wrap(~Landing_Runway, scales = 'free')

ggplot(rawsegs, aes(rawsegs$Ave_Wind_HDG)) +
  geom_histogram()+
  facet_wrap(~Landing_Runway, scales = 'free')

# Strong winds
ggplot(fta[Surface_Wind_SPD > 10], aes(fta[Surface_Wind_SPD > 10]$Surface_Wind_HDG)) +
  geom_histogram()+
  facet_wrap(~Landing_Runway, scales = 'free')

ggplot(anem[Surface_Wind_SPD > 10], aes(anem[Surface_Wind_SPD > 10]$Surface_Wind_HDG)) +
  geom_histogram()+
  facet_wrap(~Landing_Runway, scales = 'free')

ggplot(rawsegs[Ave_Wind_SPD > 10], aes(rawsegs[Ave_Wind_SPD > 10]$Ave_Wind_HDG)) +
  geom_histogram()+
  facet_wrap(~Landing_Runway, scales = 'free')
