track_visualiser_ui <- function(id) {
  
  ns <- NS(id)
  
  div(
    style = "margin: -15px; height: calc(100vh - 50px) !important; z-index: 1;",
    box(
      style = "padding: 0",
      width = NULL,
      height = 0,
      solidHeader = T,
      leafletOutput(ns("pltmap")),
      uiOutput(ns("pltmap_filters_ui")),
      hidden(
        uiOutput(ns("pltmap_time_range_ui"))
      )
    )
  )
  
}