data_loader_ui <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(
    
    box(
      title = "Console",
      width = NULL,
      div(
        style = "max-height: 30vh; overflow-y: auto;",
        h5("NOTE: Errors will occur if your user does not have database write permission."),
        textOutput(ns("console_output"))
      )
    ),
    
    div(
      style = "
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
      ",
      box(
        title = "Adaptation Management",
        h4("Import CSV Configs to Database"),
        checkboxInput(ns("load_legacy_wake"), "Load Legacy Wake (Populate_tbl_Aircraft_Type_To_Wake_Legacy)", width = "100%"),
        checkboxInput(ns("load_DW_volumes"), "Load DW Volumes (05_Populate_Airspace_Volumes_*_DW)", width = "100%"),
        div(
          style = "display: flex; justify-content: flex-start;",
          shinyDirButton(ns("load_config_folder"), label="Browse Config Folder", title="Select Config Folder"),
          div(style = "width: 5px"),
          actionButton(ns("load_config"), "Load Config")
        ),
        hr(),
        h4("Export Database Configs to XML Format"),
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          shinyDirButton(ns("export_config_folder"), label="Select Output Folder", title="Select Config Output Folder"),
          div(style = "padding: 8px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; min-width: 0;", "Version Suffix"),
          textInput(ns("export_xml_version"), NULL, "V0.0.0"),
          div(style = "width: 5px"),
          actionButton(ns("export_xml"), "Export XML")
        )
      ),
      
      box(
        title = "Data Loading",
        actionButton(ns("clear_db"), "Clear Database"),
        hr(),
        shinyFilesButton(ns("logs_select"), label="Browse Log File(s)", title="Hold shift to select multiple", multiple=T),
        div(style = "height: 5px; width: 5px;"),
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          div(style = "padding: 7px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; min-width: 0;", "Select Log Type:"),
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
              "Cat62 Fusion (NAVCAN)",
              "Surveillance radar (LVNL)",
              "Flight Plan logs (LVNL)",
              "QNH logs (LVNL)",
              "Surface Wind logs (LVNL)",
              "CAV logs"
            ),
            multiple = F
          ),
          
        ),
        div(style = "height: 5px; width: 5px;"),
        actionButton(ns("logs_load"), "Load File(s)"),
        hr(),
        actionButton(ns("add_proc"), "Additional Processing")
      ),
      
      box(
        title = "IA Processing",
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          actionButton(ns("ord_pre_proc"), "Run ORD Pre-Processing"),
          div(style = "padding: 8px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; min-width: 0;", "ORD Pre-P Date (dd/mm/yy)"),
          textInput(ns("ord_pre_proc_date"), NULL)
        ),
        div(style = "height: 15px;"),
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          actionButton(ns("ord_proc"), "Run ORD Processing"),
          div(style = "padding: 8px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; min-width: 0;", "ORD Run Date (dd/mm/yy)"),
          textInput(ns("ord_proc_date"), NULL)
        ),
        div(style = "height: 15px;"),
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          actionButton(ns("plt_pre_proc"), "Run PLT Pre-Processing"),
          div(style = "padding: 8px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; min-width: 0;", "PLT Pre-P Date (dd/mm/yy)"),
          textInput(ns("plt_pre_proc_date"), NULL)
        ),
        div(style = "height: 15px;"),
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          actionButton(ns("plt_proc"), "Run PLT Processing"),
          div(style = "padding: 8px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; min-width: 0;", "PLT Run Date (dd/mm/yy)"),
          textInput(ns("plt_proc_date"), NULL)
        ),
        div(style = "height: 15px;"),
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          actionButton(ns("selective_purge"), "Selective Purge"),
          div(style = "padding: 8px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; min-width: 0;", "Only Keep days with A/C Type:"),
          textInput(ns("selective_purge_actype"), NULL)
        )
      ),
      
      box(
        title = "Run Scripts",
        div(
          style = "display: flex; justify-content: flex-start; height: 34px;",
          shinyFilesButton(ns("script_select"), label="Browse Script", title="Select SQL File", multiple=F),
          div(style = "padding: 8px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; min-width: 0;", "Parameters"),
          textInput(ns("script_run_params"), NULL),
          div(style = "width: 5px"),
          actionButton(ns("script_run"), "Run Script"),
        )
      )
    )
    
  )
  
}
