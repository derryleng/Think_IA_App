Ref_Adaptation <- fread(file.path(ref_data, "EGLL_ORD_Aircraft_Adaptation.csv"))

New_Adaptation <- fread(list.files(path = file.path(Project_Directory, adaptation_folder), pattern = "Populate_tbl_ORD_Aircraft_Adaptation_[A-Z]{1,4}.csv", full.names = T)[1])

New_Adaptation_Wake <- fread(list.files(path = file.path(Project_Directory, adaptation_folder), pattern = "Populate_tbl_ORD_Wake_Adaptation_[A-Z]{1,4}.csv", full.names = T)[1])

Shared_Aircraft_Types <- intersect(Ref_Adaptation$Aircraft_Type, New_Adaptation$Aircraft_Type)

out_dir <- file.path(Project_Directory, adaptation_folder)

wake_cats <- sqlQuery(con, "SELECT * FROM tbl_Aircraft_Type_To_Wake")

New_Adaptation <- inner_join(New_Adaptation, wake_cats, by = c("Aircraft_Type"))

# New Adaptation Bar Chart ------------------------------------------------

y_upper <- round_any(max(New_Adaptation$Min_Safe_Landing_Speed_Lead), 10, ceiling)

png(file.path(out_dir, paste0(Airport_Code, "_ORD_Aircraft_Adaptation_Ranking_Wake.png")), width = 1000, height = 600)
ggplot(
  data = New_Adaptation,
  aes(y = Min_Safe_Landing_Speed_Lead, x = reorder(Aircraft_Type, Min_Safe_Landing_Speed_Lead), fill = Wake)
) +
  geom_bar(stat = "identity") +
#  scale_fill_gradientn(colours = brewer.pal(11, "Spectral")) +
    scale_fill_brewer(palette = "Set1") +
#  scale_y_continuous(expand=c(0,0), breaks = seq(0, y_upper, 10), limits = c(0, y_upper)) +
  #  scale_y_continuous(expand=c(0,0), breaks = seq(0, y_upper, 10), limits = c(0, y_upper)) +
  
    labs(
    x = "Aircraft Type",
    y = "Min Safe Landing Speed Lead (kts)"
  ) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.minor.y = element_line(color = "grey"),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
#    legend.position = "none"
  )
Sys.sleep(1)
dev.off()


png(file.path(out_dir, paste0(Airport_Code, "_ORD_Wake_Adaptation_Ranking.png")), width = 1000, height = 600)
ggplot(
  data = New_Adaptation_Wake,
  aes(y = Min_Safe_Landing_Speed_Lead, x = Wake_Cat, fill = Wake_Cat)
) +
  geom_text(aes(label = sprintf("%.0f", Min_Safe_Landing_Speed_Lead)), vjust = -1)+
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand=c(0,0), breaks = seq(0, y_upper, 10), limits = c(0, y_upper)) +
  labs(
    x = "Wake Category",
    y = "Min Safe Landing Speed Lead (kts)"
  ) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.minor.y = element_line(color = "grey"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  )
Sys.sleep(1)
dev.off()


# Aircraft Type Vref Comparison -------------------------------------------
Aircraft_Type_Comparison <- merge(
  Ref_Adaptation[Aircraft_Type %in% Shared_Aircraft_Types, .(Aircraft_Type, Min_Safe_Landing_Speed_Lead)],
  New_Adaptation[Aircraft_Type %in% Shared_Aircraft_Types, .(Aircraft_Type, Min_Safe_Landing_Speed_Lead)],
  by = "Aircraft_Type"
)
names(Aircraft_Type_Comparison) <- c("Aircraft_Type", "EGLL", Airport_Code)
Aircraft_Type_Comparison$Diff <- Aircraft_Type_Comparison[[Airport_Code]] - Aircraft_Type_Comparison$EGLL

# Changes in vref for Airport_Code compared to EGLL
summary(Aircraft_Type_Comparison$Diff)

# Plotted Vrefs vs Aircraft Type for Airports
Aircraft_Type_Comparison.gathered <- gather(Aircraft_Type_Comparison, "Airport", "Vref", 2:3) %>% as.data.table()
Aircraft_Type_Comparison.gathered_labs <- data.table(
  Aircraft_Type_Comparison.gathered[, c("Aircraft_Type", "Diff")],
  Avg_Vref = sapply(Aircraft_Type_Comparison.gathered$Aircraft_Type, function(i) mean(Aircraft_Type_Comparison.gathered[Aircraft_Type == i]$Vref))
)
g1 <- ggplot(data = Aircraft_Type_Comparison.gathered, aes(x = Aircraft_Type, y = Vref)) +
  geom_path(linetype = 5) +
  geom_text(data = Aircraft_Type_Comparison.gathered_labs, aes(y = Avg_Vref, label = round(abs(Diff), 1)), hjust = 1.2) +
  geom_point(aes(col = Airport), size = 3) +
  scale_color_manual(values = c("#462170", "#E8112D")) +
  labs(title = NULL, x = "Aircraft Type", y = "Vref (kts)") +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_line(),
    panel.grid.major.x = element_line(color = "grey")
  )

# png(file.path(out_dir, "Vref Comparison Diff.png"), width = 1750*5-550*5, height = 550*5, res = 360)
# g1
# dev.off()

# Plotted EGLL Vrefs vs Airport_Code Vrefs
g2_vref_range <- range(c(Aircraft_Type_Comparison$EGLL, Aircraft_Type_Comparison[[Airport_Code]]))
g2_lim_min <- round_any(g2_vref_range[1], 10, floor)
g2_lim_max <- round_any(g2_vref_range[2], 10, ceiling)

g2 <- ggplot(data = Aircraft_Type_Comparison, aes_string(x = Airport_Code, y = "EGLL")) +
  geom_point() +
  geom_text_repel(aes(label = Aircraft_Type), vjust = 1.4) +
  geom_abline() +
  coord_cartesian(xlim=c(g2_lim_min, g2_lim_max), ylim=c(g2_lim_min, g2_lim_max)) + 
  xlim(g2_lim_min, g2_lim_max) +
  ylim(g2_lim_min, g2_lim_max) +
  labs(title = NULL, x = paste0(Airport_Code, " Vref (kts)"), y = "EGLL Vref (kts)") +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.major.x = element_line(color = "grey")
  )

# png(file.path(out_dir, "Vref Comparison Scatter.png"), width = 550*5, height = 550*5, res = 360)
# g2
# dev.off()

png(file.path(out_dir, "Vref Comparison Full.png"), width = 1000, height = 800)
#grid.arrange(g2, g1, widths=c(0.35, 0.65), nrow = 1)
#print(g2)
g2
dev.off()
