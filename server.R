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
      connection_string <- sprintf(
        "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
        input$db_driver, input$db_server, input$db_database, input$db_username, input$db_password
      )
      return(odbcDriverConnect(connection=connection_string))
    }
  })
  
  dbi_con <- eventReactive(db_connect_click(), {
    if (db_connect_click()) {
      connection_string <- sprintf(
        "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
        input$db_driver, input$db_server, input$db_database, input$db_username, input$db_password
      )
      return(dbConnect(odbc::odbc(), .connection_string=connection_string))
    }
  })
  
  # Database connection status feedback
  observeEvent(con(), {
    if (con() != -1L) {
      output$db_status <- renderUI({
        div(
          style="margin: 7px 0 0 6px;",
          div(
            style = "text-align: center;",
            "Connected", icon("check-circle")
          ),
          div(style = "height: 5px"),
          div(
            style = "margin-left: -7px;",
            renderPrint(con())
          )
        )
      })
      removeModal()
    } else {
      output$db_status <- renderUI({
        div(style="margin: 7px 0 0 6px; text-align: center;", "Connection Error", icon("times-circle"))
      })
    }
  })
  
  # Refresh database list
  observeEvent(input$db_refresh_list, {
    con <- odbcDriverConnect(connection=sprintf(
      "Driver={%s};Server={%s};Uid={%s};Pwd={%s};",
      input$db_driver, input$db_server, input$db_username, input$db_password
    ))
    db_list <- "SELECT name FROM sys.databases WHERE database_id > 4 ORDER BY name" %>%
      sqlQuery(con,. ) %>% as.data.table()
    fwrite(db_list, "temp/db_names.csv", col.names = F)
    updateSelectizeInput(
      session,
      "db_database",
      choices = fread("temp/db_names.csv", header = F)$V1
    )
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
            callModule(evalParse(x, "_server"), x, con(), dbi_con())
          }
        }
      })
    }

  })
  
  # ----------------------------------------------------------------------- #
  # Debug -------------------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  onclick("debug_test", showModal(modalDialog(
    div(class = "centered", h4("input")),
    verbatimTextOutput("inputdataText"),
    div(class = "centered", h4("session$clientData")),
    verbatimTextOutput("clientdataText"),
    size = "l",
    footer = NULL,
    easyClose = T
  )))
  
  inputsTable <- reactive({
    x <- NULL
    for (i in 1:length(names(input))) {
      if (names(input)[i] %!in% c("db_password")) {
        x <- c(x, paste0(names(input)[i], " = ", input[[names(input)[i]]]))
      }
    }
    return(paste(sort(x), collapse = "\n"))
  })
  
  output$inputdataText <- renderText({
    inputsTable()
  })
  
  cdata <- session$clientData
  
  output$clientdataText <- renderText({
    cnames <- sort(names(cdata)) %>% .[. %!in% grep("^output_spinner.*$", ., value=T)]
    allvalues <- lapply(cnames, function(name) {
      paste(name, cdata[[name]], sep = " = ")
    })
    paste(allvalues, collapse = "\n")
  })
  
}
