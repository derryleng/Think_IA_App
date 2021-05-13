rm(list = ls())

library(tidyverse)
library(data.table)
library(gridExtra)

user <- "Andy Hyde"

base_dir <- file.path("C:", "Users", user, "Dropbox (Think Research)", "NATS Projects", "NATS NavCanada TBS", "Data Analysis", "Outputs", "ORD Output")

ref_data <- file.path(base_dir, "Validation 26-04-21 Second Adjustment")

out_data <- file.path(base_dir, "Validation 26-04-21 Second Adjustment")

setwd(out_data)

plots_data <- file.path(out_data, "Error Plots")
if (!dir.exists(plots_data)) dir.create(plots_data)

plots_nested <- file.path(out_data, "Error Plots", "Leader IAS Grouped")
if (!dir.exists(plots_nested)) dir.create(plots_nested)

ord_data <- fread(file.path(ref_data, "Validation Data Post SepN Accuracy.csv"))

adaptation <- fread(file.path("C:/Users/Andy Hyde/Dropbox (Think Research)/NATS Projects/NATS NavCanada TBS/Data Analysis/Outputs/ORD Output/Adaptation V3 15_04_21/Populate_tbl_ORD_Aircraft_Adaptation_New_DecelCYYZ.csv"))
adaptation <- fread(file.path(base_dir, "Validation 19-04-21 AH TEST", "Populate_tbl_ORD_Aircraft_Adaptation_CYYZ.csv"))

# adaptation <- adaptation %>% select(c("Aircraft_Type", "Min_Safe_Landing_Speed_Lead"))

Error_Types <- c("Combined_Wind_Effect_Error", "ORD_Compression_Error", 'ORD_Leader_IAS_Error', "Forecast_Mean_Leader_Wind_Effect_Error", "Forecast_Mean_Follower_Wind_Effect_Error")
Group_Types <- c("Leader_Aircraft_Type", "Follower_Aircraft_Type", "Leader_Standard_Flag", "Leader_Extended_Flag", "Follower_Standard_Flag", "Follower_Extended_Flag", "Leader_ICAO4", "LF_Pair_ICAO4")


Flags <- c("Leader_Standard_Flag", "Leader_Extended_Flag", "Follower_Standard_Flag", "Follower_Extended_Flag")

#-----------------------------------------------------------------------------#
# Printing Percentile information --------------------------------------------#


for (i in Error_Types) {
  for (j in Flags) {

    temp1 <- filter(ord_data, ord_data[[j]] == 0)
    temp2 <- filter(ord_data, ord_data[[j]] == 1)

    print(paste0(i, " quantiles for ", j))
    print(paste0(j, "= 0"))
    print(quantile(temp1[[i]]))
    print(paste0(j, "= 1"))
    print(quantile(temp2[[i]]))
    print("--------------------------------------------------------------------------------")
  }
}

#-----------------------------------------------------------------------------#
# Creating Box Plots by for each error type by each grouping------------------#

for (i in Error_Types) {
  for (j in Group_Types) {

    temp <- select(ord_data, c(paste0(i), paste0(j)))

    png(filename = file.path(plots_data, paste0(i, " by ", j, ".png")), width = 1900, height = 1000)
    p <- ggplot(temp) +
      geom_boxplot(aes(x = reorder(temp[[j]], temp[[i]]), y = temp[[i]])) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(title = paste0(i, " by ", j), x = paste0(j), y = paste0(i))
    print(p)
    dev.off()

  }
}


#-----------------------------------------------------------------------------#
# Creating box plots for IAS by wake cat--------------------------------------#

Leader_RECAT_types <- ord_data %>% select(Leader_RECAT) %>% distinct()
Leader_RECAT_types <- sort(Leader_RECAT_types$Leader_RECAT)

Error_Types <- c("ORD_Leader_IAS_Error", "Observed_Mean_Leader_IAS", "ORD_Mean_Leader_IAS")

for (i in Error_Types) {
  for (j in Leader_RECAT_types) {

    temp <- filter(ord_data,ord_data$Leader_RECAT == j)

    png(filename = file.path(plots_nested, paste0(i, " by ", j, ".png")), width = 1900, height = 1000)
    p <- ggplot(temp) +
      geom_boxplot(aes(x = reorder(Leader_Aircraft_Type, temp[[i]]), y = temp[[i]])) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(title = paste0(i, " by ", j), x = paste0(j), y = paste0(i))
    print(p)
    dev.off()

  }
}

#-----------------------------------------------------------------------------#
# Grouping aircraft by type and calculating mean error and 99th percentile ---#
Mean_Summary <- ord_data %>% group_by(Leader_Aircraft_Type) %>% summarise(ORD_Error_mean = mean(ORD_Compression_Error, na.rm = T))

Percentile_Summary <- ord_data %>% group_by(Leader_Aircraft_Type) %>% summarise(ORD_99_percentile = quantile(ORD_Compression_Error, p=0.99))

High_percentile_error <- filter(Percentile_Summary, Percentile_Summary$ORD_99_percentile >= 0.1)

High_percentile_error <- inner_join(High_percentile_error, adaptation %>% select(c("Aircraft_Type", "Min_Safe_Landing_Speed_Lead")), by = c("Leader_Aircraft_Type" = "Aircraft_Type"))

High_percentile_error$Vref_adjustment <- (High_percentile_error$ORD_99_percentile - 0.1) * High_percentile_error$Min_Safe_Landing_Speed_Lead / 4.5

High_percentile_error$new_vref <- High_percentile_error$Min_Safe_Landing_Speed_Lead - High_percentile_error$Vref_adjustment

High_percentile_error$old_adjust <- 0.25*High_percentile_error$Min_Safe_Landing_Speed_Lead / 4.5






ggplot(ord_data) +
  geom_boxplot(aes(x = reorder(Leader_Aircraft_Type, ORD_Compression_Error), y = ORD_Compression_Error)) +
  facet_grid(~Leader_RECAT, scales = "free", space = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


#-----------------------------------------------------------------------------#
#Creating specific plots

i <- "ORD_Compression_Error"
j <- "Follower_Aircraft_Type"

png(filename = file.path(plots_data, paste0(i, " by ", j, ".png")), width = 1900, height = 1000)
p <- ggplot(temp) +
  geom_boxplot(aes(x = reorder(temp[[j]], temp[[i]]), y = temp[[i]])) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = paste0(i, " by ", j), x = paste0(j), y = paste0(i))
print(p)
dev.off()

i <- "Forecast_Mean_Follower_Wind_Effect_Error"
j <- 'Follower_RECAT'


temp <- select(ord_data, c(paste0(i), paste0(j)))

png(filename = file.path(plots_data, paste0(i, " by ", j, ".png")), width = 1900, height = 1000)
p <- ggplot(temp) +
  geom_boxplot(aes(x = reorder(temp[[j]], temp[[i]]), y = temp[[i]])) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = paste0(i, " by ", j), x = paste0(j), y = paste0(i))
print(p)
dev.off()

i <- "ORD_Leader_IAS_Error"
j <- 'Leader_RECAT'


temp <- select(ord_data, c(paste0(i), paste0(j)))

png(filename = file.path(plots_data, paste0(i, " by ", j, ".png")), width = 1900, height = 1000)
p <- ggplot(temp) +
  geom_boxplot(aes(x = reorder(temp[[j]], temp[[i]]), y = temp[[i]])) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = paste0(i, " by ", j), x = paste0(j), y = paste0(i))
print(p)
dev.off()






i <- "Mean_ORD"
j <- "Leader_Aircraft_Type"

temp1 <- ord_data %>% group_by(Leader_Aircraft_Type) %>% summarise(Mean_ORD = mean(ORD_Compression)) %>% arrange(.$Mean_ORD)
temp1 <- left_join(temp1, ord_data %>% select(Leader_Aircraft_Type, Leader_RECAT) %>% distinct(), by = "Leader_Aircraft_Type")
temp1 <- temp1 %>% group_by(Leader_RECAT)

png(filename = file.path(plots_data, paste0(i, " by ", j, ".png")), width = 1900, height = 1000)
p <- ggplot(temp1) +
  geom_col(aes(x = reorder(Leader_Aircraft_Type, Mean_ORD), y = Mean_ORD, fill=Leader_RECAT)) +
  theme(axis.text.x = element_text(angle = 90))
print(p)
dev.off()







MD11 <- filter(ord_data, ord_data$Leader_Aircraft_Type == "MD11")

IAS <- ggplot(MD11) +
          geom_histogram(aes(x = Observed_Mean_Leader_IAS), binwidth = 0.25*sd(MD11$Observed_Mean_Leader_IAS),
                        col = "red",
                        fill = "blue")

ORD_Error <- ggplot(MD11) +
                geom_histogram(aes(x = ORD_Compression_Error), binwidth = 0.25*sd(MD11$ORD_Compression_Error),
                               col = "red",
                               fill = "blue")
