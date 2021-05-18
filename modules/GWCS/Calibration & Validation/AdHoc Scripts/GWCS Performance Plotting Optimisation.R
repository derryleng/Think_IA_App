Plot_CIs <- function(Data, Metric){
  
  RateVar <- paste0("O-F >", Metric, " Rate")
  UPRVar <- paste0("O-F >", Metric, " UPR")
  LWRVar <- paste0("O-F >", Metric, " LVR")
  TitleText <- paste0(">", Metric, " rate with Confidence Intervals")
  XText <- paste0("Surface Wind Band (kts)")
  YText <- paste0(">", Metric, " Error Rate")
  
  Data <- select(Data, `Surface Wind` !!sym(RateVar), !!sym(UPRVar), !!sym(LWRVar)) %>%
    pivot_longer(!`Surface Wind`, names_to = "Series", values_to = "Rate") %>%
    rename(plot_var = `Surface Wind`)
  
  plot <- ggplot() +
    geom_point(data=Data, aes(x = plot_var, y = Rate,colour=Series, group = Series), size =2)+
    geom_line(data=Data, aes(x = plot_var, y = Rate,colour=Series, group = Series), size = 1)+
    ggtitle(TitleText)+
    labs(x = XText, y = YText)+
    #coord_cartesian(ylim=c(-0.3,0.3))+
    #scale_y_continuous(labels = scales::scientific)+
    scale_y_log10(labels = scales::scientific)+
    scale_colour_brewer(type = "seq", palette = "Paired", direction = -1)+
    theme(axis.line = element_line(colour = "black",size = 0.5, linetype = "solid"), plot.title =  element_text(size=16), legend.title=element_blank(),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))
  
  return(plot)
  
}

Plot_CI_Grid <- function(Data, OutDir, FileName){
  
  plot_10 <- Plot_CIs(Data, "10kts")
  plot_20 <- Plot_CIs(Data, "20kts")
  plot_25 <- Plot_CIs(Data, "0.25NM")
  plot_50 <- Plot_CIs(Data, "0.5NM")
  png(width=1000, height = 600, file=file.path(OutDir, paste0(FileName, ".png")))
  print(grid.arrange(plot_10, plot_20, plot_25, plot_50))
  dev.off()
  
}

Plot_CI_Grid(PR_Arr_SW, out_data, "Plots by Surfance Wind Band CI")
Plot_CI_Grid(PR_Arr_SW_Test_Sample_Size, out_dir, "Plots by Surfance Wind Band CI Size 40000")

