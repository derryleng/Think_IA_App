plt_tools_ui <- function(id) {
  
  ns <- NS(id)
  
  div(
    
    div(
      style = "padding-bottom: 15px;",
      actionButton(ns("editor_template"), "Create New"),
      shinyDirButton(ns("editor_load"), label="Load Existing", title="Select Airspace Adaptation Folder"),
      shinyDirButton(ns("editor_export"), label="Export", title="Select Export Folder")
    ),
    
    div(
      
      style = "display: flex; gap: 20px; padding-bottom: 20px;",
      
      div(
        style = "flex: 1; height: 600px; min-width: 50%; max-width: 50%;",
        box(
          title = "PLT Adaptation Editor",
          status = "primary",
          solidHeader = T,
          width = NULL,
          collapsible = T,
          collapsed = F,
          uiOutput(ns("editor_view"))
        )
      ),
      
      div(
        style = "flex: 1; height: 600px; padding-top: 40px;",
        div(
          style = "display: flex; background: #D51067; border-radius: 3px 3px 0 0; margin-top: -40px",
          div(style = "padding: 10px; color: white", tags$b("PLT Adaptation Preview")),
          # dropdown(
          #   div(style = "font-weight: bold; padding-bottom: 10px", "Runways"),
          #   pickerInput_customised(ns("toggle_tbl_Runway"), NULL, choices = NULL),
          #   style = "minimal", icon = icon("road"),
          #   tooltip = tooltipOptions(title = "Runways", placement = "right")
          # ),
          dropdown(
            div(style = "font-weight: bold; padding-bottom: 10px", "Volumes"),
            pickerInput_customised(ns("toggle_tbl_Volumes"), "Variant 1", choices = NULL),
            hidden(
              pickerInput_customised(ns("toggle_tbl_Volumes_2"), "Variant 2", choices = NULL),
              pickerInput_customised(ns("toggle_tbl_Volumes_3"), "Variant 3", choices = NULL)
            ),
            style = "minimal", icon = icon("vector-square"),
            tooltip = tooltipOptions(title = "Volumes", placement = "right")
          )
        ),
        leafletOutput(ns("map"), height = "563px")
      )
      
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
