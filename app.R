load_packages <- function(req_file, install = T, update = F, silent = F) {
  # req_file - .txt file containing list of required packages (one per line)
  req <- scan(req_file, character(), quiet = T)
  
  if (update) update.packages(repos = "https://cloud.r-project.org", ask = F)
  
  if (length(req) > 0 & install) {
    missing_packages <- req[!(req %in% installed.packages()[,"Package"])]
    if (length(missing_packages) > 0) {
      install.packages(missing_packages, repos = "https://cloud.r-project.org", dependencies = T, clean = T)
    }
  }
  
  if (silent) {
    suppressPackageStartupMessages(invisible(lapply(req, library, character.only = T)))
  } else {
    lapply(req, library, character.only = T)
  }
  
  if (!webshot::is_phantomjs_installed()) {
    webshot::install_phantomjs()
  }
}

load_packages("req.txt", silent = T)

invisible(lapply(list.files(path = "modules/", pattern = "mod_ui.R|mod_server.R", recursive = T, full.names = T), source))

invisible(lapply(list.files(pattern = "settings.R|functions.R"), source))

# ----------------------------------------------------------------------- #
# UI ----------------------------------------------------------------------
# ----------------------------------------------------------------------- #

header <- dashboardHeader(
  title = htmltools::HTML('<img src="Think_Logo_White.png" width="100" height="37">'),
  titleWidth = sidebarWidth,
  headerButtonUI("db_button", "database"),
  headerButtonUI("debug_test", "info-circle")
)

sidebar <- dashboardSidebar(
  width = sidebarWidth,
  sidebarMenu(
    id = "tabs",
    lapply(names(sidebarSettings), function(x) {
      tab_title <- sidebarSettings[[x]][1]
      tab_icon <- sidebarSettings[[x]][2]
      sidebarTabUI(x, tab_title, tab_icon)
    })
  )
)

body <- dashboardBody(
  
  useShinyjs(),
  
  # Load all CSS files within www folder
  lapply(list.files("www", pattern = ".css"), function(x) tags$link(rel = "stylesheet", type = "text/css", href = x)),
  
  # Load all JS files within www folder
  lapply(list.files("www", pattern = ".js"), function(x) tags$script(src = x)),
  
  # Apply minor page layout adjustment
  tags$script(HTML("$('body').addClass('fixed');")),
  
  # Apply all module UI content defined in mod_ui.R files
  evalParse(
    "tabItems(", paste0(
      sapply(names(sidebarSettings), function(x) {
        paste0("tabItem(\"", x, "\",", x, "_ui(\"", x ,"\")", ")", collapse = ",")
      }),
      collapse = ","
    ), ")"
  ),
  
  # Loading spinner
  conditionalPanel(
    condition="$('html').hasClass('shiny-busy')",
    id = "spinner_wrapper",
    htmltools::HTML('<div class="spinner"></div>')
  )
  
)

ui <- dashboardPage(title = "Think IA App", skin = "red", header, sidebar, body)

# ----------------------------------------------------------------------- #
# Server ------------------------------------------------------------------
# ----------------------------------------------------------------------- #

server <- function(input, output, session) {
  
  addResourcePath("www", "www/")
  
  session$allowReconnect(T)
  
  # Stop app on session end
  session$onSessionEnded(function() {
    odbcCloseAll()
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
  
  # Legacy database connection
  con <- eventReactive(db_connect_click(), {
    if (db_connect_click()) {
      connection_string <- sprintf(
        "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
        input$db_driver, input$db_server, input$db_database, input$db_username, input$db_password
      )
      return(odbcDriverConnect(connection=connection_string))
    }
  })
  
  # New database connection
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
      choices = fread("temp/db_names.csv", header = F, sep = NULL)$V1
    )
    odbcClose(con)
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

shinyApp(ui, server, options = list(port = 3838))
