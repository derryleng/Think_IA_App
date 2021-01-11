data_loader_ui <- function(id) {
  
  ns <- NS(id)
  
  div(
    
    tabBox(
      width = NULL,
      
      tabPanel(
        title = "Database Management",
        div(
          style = "display: flex; justify-content: flex-start;",
          actionButton(ns("clear_db"), "Clear Database"),
          div(style = "width: 5px"),
          uiOutput(ns("clear_db_confirmed_wrapper"))
        ),
        hr(),
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          shinyFilesButton(ns("logs_select"), label="Browse Log File(s)", title="Select Log File(s)", multiple=T),
          div(style = "padding: 7px;", "Select Log Type:"),
          pickerInput_customised(
            ns("logs_type"),
            choices = c(
              "eTBS system logs (NATS)",
              "Cat48 radar (NATS)",
              "Cat62 radar (NATS)",
              "Cat20 radar (NATS)",
              "Non-Mode_S radar (NAVCAN)",
              "Flight Plan logs (NAVCAN)",
              "Alt Flight Plan logs (NAVCAN)",
              "Ground radar (NAVCAN)",
              "Surface wind and QNH (NAVCAN)",
              "Surveillance radar (LVNL)",
              "Flight Plan logs (LVNL)",
              "QNH logs (LVNL)",
              "Surface Wind logs (LVNL)"
            ),
            multiple = F
          ),
          div(style = "width: 5px"),
          actionButton(ns("logs_load"), "Load File(s)")
        ),
        div(style = "height: 5px"),
        textOutput(ns("console_output")),
        h5("NOTE: Errors will occur if your user does not have database write permission.")
      ),
      
      tabPanel(
        title = "Run Scripts",
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          shinyFilesButton(ns("script_select"), label="Browse Script", title="Select SQL File", multiple=F),
          div(style = "padding: 5px", "Parameters"),
          textInput(ns("script_run_params"), NULL),
          div(style = "width: 5px"),
          actionButton(ns("script_run"), "Run Script"),
        )
      ),
      
      tabPanel(
        title = "TBS Specific Processing",
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          actionButton(ns("ord_pre_proc"), "Run ORD Pre-Processing"),
          div(style = "padding: 5px", "ORD Pre-P Date (dd/mm/yy)"),
          textInput(ns("ord_pre_proc_date"), NULL)
        ),
        div(style = "height: 15px;"),
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          actionButton(ns("ord_proc"), "Run ORD Processing"),
          div(style = "padding: 5px", "ORD Run Date (dd/mm/yy)"),
          textInput(ns("ord_proc_date"), NULL)
        ),
        div(style = "height: 15px;"),
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          actionButton(ns("plt_pre_proc"), "Run PLT Pre-Processing"),
          div(style = "padding: 5px", "PLT Pre-P Date (dd/mm/yy)"),
          textInput(ns("plt_pre_proc_date"), NULL)
        ),
        div(style = "height: 15px;"),
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          actionButton(ns("plt_proc"), "Run PLT Processing"),
          div(style = "padding: 5px", "PLT Run Date (dd/mm/yy)"),
          textInput(ns("plt_proc_date"), NULL)
        ),
        div(style = "height: 15px;"),
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          actionButton(ns("selective_purge"), "Selective Purge"),
          div(style = "padding: 5px", "Only Keep days with A/C Type:"),
          textInput(ns("selective_purge_actypes"), NULL)
        )
      ),
      
      tabPanel(
        title = "Adaptation Management",
        h4("Import CSV Configs to Database"),
        div(
          style = "display: flex; justify-content: flex-start;",
          shinyDirButton(ns("load_config_folder"), label="Browse Config Folder", title="Select Config Folder"),
          div(style = "width: 5px"),
          actionButton(ns("load_config"), "Load Config"),
        ),
        hr(),
        h4("Export Database Configs to XML Format"),
        div(
          style = "display: flex; justify-content: flex-start;",
          shinyDirButton(ns("export_config_folder"), label="Select Output Folder", title="Select Config Output Folder"),
          div(style = "width: 5px"),
          actionButton(ns("export_xml"), "Export XML")
        ),
        div(style = "height: 5px"),
        textOutput(ns("console_output_config"))
      )
      
    )
    
  )
  
}
