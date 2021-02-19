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
      div(
        style = "display: flex; justify-content: flex-start; gap: 5px;",
        actionButton(ns("editor_template"), "Create New"),
        shinyDirButton(ns("editor_load"), label="Load Existing", title="Select Airspace Adaptation Folder"),
        shinyDirButton(ns("editor_export"), label="Export", title="Select Export Folder")
      ),
      uiOutput(ns("editor_view"))
    )
    
  )
  
}
