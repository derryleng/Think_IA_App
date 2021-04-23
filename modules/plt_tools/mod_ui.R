plt_tools_ui <- function(id) {
  
  ns <- NS(id)
  
  div(
    
    box(
      title = "PLT Adaptation Editor",
      status = "primary",
      solidHeader = T,
      width = NULL,
      collapsible = T,
      collapsed = F,
      actionButton(ns("editor_template"), "Create New"),
      shinyDirButton(ns("editor_load"), label="Load Existing", title="Select Airspace Adaptation Folder"),
      hidden(shinyDirButton(ns("editor_export"), label="Export", title="Select Export Folder")),
      uiOutput(ns("editor_view"))
    ),
    
    box(
      title = "PLT Analysis Tool",
      status = "primary",
      solidHeader = T,
      width = NULL,
      collapsible = T,
      collapsed = F,
      uiOutput(ns("analysis_view"))
    )
    
  )
  
}
