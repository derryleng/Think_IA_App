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
      title = "PLT Analysis Run",
      status = "primary",
      solidHeader = T,
      width = NULL,
      collapsible = T,
      collapsed = T,
      checkboxGroupInput(
        ns("plt_analysis_variants"), label = "Variant Selection", 
        choices = list("Variant 1" = 1, "Variant 2" = 2, "Variant 3" = 3),
        selected = 1
      ),
      actionButton(ns("plt_analysis_run"), "Run PLT Analysis")
    ),
    
    box(
      title = "PLT Summary Stats",
      status = "primary",
      solidHeader = T,
      width = NULL,
      collapsible = T,
      collapsed = T,
      DT::dataTableOutput(outputId = ns("plt_summary_table"))
    ),
    
    box(
      title = "PLT Detailed Analysis",
      status = "primary",
      solidHeader = T,
      width = NULL,
      collapsible = T,
      collapsed = F,
      uiOutput(ns("analysis_view_1"))
    )
    
  )
  
}
