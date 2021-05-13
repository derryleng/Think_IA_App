
scheme <- "RECAT"
time_required <- F
sep_scheme <- sep_scheme
data1$plot_data_1 <- data1$Recat_Perfect_1DME_Time_Separation
data1$plot_data_2 <- data1$Actual_1DME_Wake_Time_Separation
plot_title_1 <- "RECAT Perfect Time Wake Separation: "
plot_title_2 <- "RECAT Actual Time Wake Separation: "
plot_header <- "Histograms of RECAT-EU Perfect/Actual Wake Time Separation at 1DME (All Wind) for "

# Create a word document
doc <- read_docx()
doc <- body_add_par(doc, "Distributions of Perfect/Actual Time Separations for IA", style = "heading 1")

Output_To_Word <- function(doc, data, scheme, time_required, sep_scheme, plot_title_1, plot_title_2, plot_header){

for (i in 1:nrow(sep_scheme)){
  
  # Select the relevant Category pair.
  cat_pair <- sep_scheme$LF_Pair[i]
  
  # Grab the Reference Time if required.
  if(time_required){time <- sep_scheme$RECAT_Time_Reference[i]}
  
  # Filter for the relevant category pair
  if(scheme == "RECAT"){plot_data_1 <- filter(data, RECAT_Wake_Pair == cat_pair)}
  else {plot_data_1 <- filter(data, ICAO_Wake_Pair == cat_pair)}
  
  # Make a copy of the plot_data
  plot_data_2 <- plot_data_1
  
  # Apply relevant distances to plot data fields to work with 
  plot_data_1$plot_data <- plot_data_1$plot_data_1
  plot_data_2$plot_data <- plot_data_2$plot_data_2
  
  # filter to remove NA pairs
  plot_data_1 <- filter(plot_data_1, !is.na(plot_data))
  plot_data_2 <- filter(plot_data_2, !is.na(plot_data))
  
  # Get the value counts for the plot
  nrow1 = nrow(plot_data_1)
  nrow2 = nrow(plot_data_2)
  
  # Get the Plot titles for this pair
  plot_title_1_new <- paste0(plot_title_1, cat_pair, " (n = ", nrow1, ")")
  plot_title_2_new <- paste0(plot_title_2, cat_pair, " (n = ", nrow2, ")")
  
  # ---------------------------------------------------- #
  # Jump to next iteration if certain criteria are met
  
  # If no data for either plots
  if (nrow1 == 0 & nrow2 == 0){next}
  
  # If doing RECAT wake pairs and no ref time is available
  if (time_required){if (is.na(recat_time)){next}}
  
  # ---------------------------------------------------- #
  
  # ---------------------------------------------------- #
  # Generate Plots
  
  if(nrow1 > 0){
    
    if( time_required ){ plot1 <- plot_time_histogram(plot_data_1, plot_title_1_new, time, "light blue") }
    else { plot1 <- plot_time_histogram_2(plot_data_1, plot_title_1_new, "light blue") }
    
  } else { plot1 <- ggplot() }
  
  if(nrow2 > 0){
    
    if( time_required ){ plot2 <- plot_time_histogram(plot_data_2, plot_title_2_new, time, "light green") }
    else { plot2 <- plot_time_histogram_2(plot_data_2, plot_title_2_new, "light green") }
    
  } else { plot2 <- ggplot() }
  
  message("Printing for ", cat_pair)
  
  # ---------------------------------------------------- #
  
  doc <- body_add_par(doc, paste0(plot_header, cat_pair), style = "heading 2")
  
  src <- tempfile(fileext = ".png")
  png(filename = src, width = 5, height = 6, units = 'in', res = 300)
  grid.arrange(plot1, plot2, ncol = 1)
  dev.off()
  
  doc <- body_add_img(doc, src = src, width = 5, height = 6, style = "centered")
  
}
  return(doc)
}

doc <- Output_To_Word(doc, data1, scheme, time_required, sep_scheme, plot_title_1, plot_title_2, plot_header)

# Export document
print(doc, target = file.path(outdir, "IA_Wake_Pair_Distributions_Test.docx"))