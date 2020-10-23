data_loader_ui <- function(id) {
  
  ns <- NS(id)
  
  div(
    
    class = "wrapper",
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "flex.css")
    ),
    
    box(
      class = "mid_1",
      title = "Database Connection",
      width = NULL,
      collapsible = F
    ),
    
    box(
      class = "mid_2",
      title = "Latest Command",
      width = NULL,
      collapsible  = F
    ),
    
    box(
      class = "left_1",
      title = "Data Loading",
      width = NULL,
      collapsible  = F
    ),
    
    box(
      class = "right_1",
      title = "Generic Processing",
      width = NULL,
      collapsible  = F
    ),
    
    box(
      class = "right_2",
      title = "TBS Specific Processing",
      width = NULL,
      collapsible  = F
    ),
    
    box(
      class = "right_3",
      title = "Adaptation Management",
      width = NULL,
      collapsible  = F
    ),
    
  )
  
}
