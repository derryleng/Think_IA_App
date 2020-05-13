server <- function(input, output, session) {
  
  output$think_logo <- renderImage({
    list(
      src = "www/Think_Logo_White.png",
      contentType = "image/png",
      height = 37,
      width = 100
    )
  }, deleteFile = F)
  
  output$spinner <- renderUI({
    htmltools::HTML('<div class="loader"></div>')
  })
  
  # ----------------------------------------------------------------------- #
  # Database Dialogue -------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  # Start-up show database connection dialogue
  showModal(connection_dialogue())
  
  # Show database connection dialogue on button click
  onclick("db_button", showModal(connection_dialogue()))
  
  db_connect_clicked <- reactiveVal(0)
  
  observeEvent(input$db_connect, {
    db_connect_clicked(db_connect_clicked() + 1)
  })
  
  con <- eventReactive(db_connect_clicked(), {
    if (db_connect_clicked()) {
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
    if (con() != -1L) {
      invisible(lapply(list.files(path = "modules"), function(x) {
        if (exists(paste0(x, "_server"))) callModule(evalParse(x, "_server"), x, con())
      }))
    }
  })
  
}
