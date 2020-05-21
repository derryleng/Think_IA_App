server <- function(input, output, session) {
  
  # Stop app on session end
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # ----------------------------------------------------------------------- #
  # Database Dialogue -------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  # Start-up show database connection dialogue
  showModal(connection_dialogue())
  
  # Show database connection dialogue on button click
  onclick("db_button", showModal(connection_dialogue()))
  
  db_connect_click <- reactiveVal(0)
  
  observeEvent(input$db_connect, {
    db_connect_click(db_connect_click() + 1)
  })
  
  con <- eventReactive(db_connect_click(), {
    if (db_connect_click()) {
      get_db_connection(input$db_driver, input$db_server, input$db_database, input$db_username, input$db_password)
    }
  })
  
  # Database connection status feedback
  observeEvent(con(), {
    output$db_status <- renderUI({
      if (con() != -1L) {
        div(
          style="margin: 7px 0 0 6px;",
          div(
            style = "text-align: center;",
            "Connected", icon("check-circle")
          ),
          div(style = "height: 3px"),
          renderPrint(con())
        )
      } else {
        div(style="margin: 7px 0 0 6px; text-align: center;", "Connection Error", icon("times-circle"))
      }
    })
  })
  
  # ----------------------------------------------------------------------- #
  # Call Modules ------------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  observeEvent(con(), {

    # Keep track of modules that haven't been loaded
    unloaded_tabs <- reactiveVal(names(sidebarSettings))

    # While database is connected, call unloaded modules when selected
    if (con() != -1L) {
      observeEvent(input$tabs, {
        for (x in names(sidebarSettings)) {
          if (input$tabs == x & x %in% unloaded_tabs() & exists(paste0(x, "_server"))) {
            unloaded_tabs(unloaded_tabs() %>% .[. != x])
            callModule(evalParse(x, "_server"), x, con())
          }
        }
      })
    }

  })
  
}
