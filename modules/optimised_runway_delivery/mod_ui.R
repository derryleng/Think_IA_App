optimised_runway_delivery_ui <- function(id) {
  
  speed_types <- list(
    "Mode S IAS" = "This option uses the available Mode S indicated airspeed data (Recommended)",
    "Track Speed" = "This option estimates IAS using groundspeed data, use when Mode S IAS is not available.",
    "Calculated Speed" = "This option estimates the IAS by first estimating groundspeed using point-to-point time and distance changes with additional filtering for stationary points and outlier speeds based on available groundspeed data."
  )
  
  ns <- NS(id)
  
  div(
    
    box(
      title = "Calibrate Speed Profile",
      width = NULL,
      collapsible  = T,
      collapsed = T,
      tabBox(
        width = NULL,
        tabPanel(
          title = "Single Aircraft",
          radioGroupButtons(
            inputId = ns("cali_speed_type"),
            label = "Speed Type", 
            choices = names(speed_types),
            selected = "Mode S IAS"
          ),
          uiOutput(ns("cali_more_options")),
          div(
            style = "centered",
            textInput(ns("cali_single_ffpid"), "Follower Flight Plan ID", 215163),
            actionButton(ns("cali_single_run"), "Calibrate"),
            uiOutput(ns("cali_single_outputs"))
          )
        ),
        tabPanel(
          title = "Batch Processing",
          "WIP"
        )
      )
    )
    
  )
  
}
