# ----------------------------------------------------------------------- #
# Headers -----------------------------------------------------------------
# ----------------------------------------------------------------------- #

header <- dashboardHeader(
  title = "Think IA App",
  tags$li(class = "dropdown header_button", id = "db_button", icon("database")),
  tags$li(class = "dropdown logo", style = "width: 125px; height: 50px; padding-left: 15px; padding-right: 15px;", imageOutput("think_logo"))
)

# ----------------------------------------------------------------------- #
# Sidebar Menu Items ------------------------------------------------------
# ----------------------------------------------------------------------- #

sidebar_tab_db <- menuItem(
  text = "Database Dashboard",
  tabName = "tab_db",
  icon = icon("database")
)

sidebar_tab_plt <- menuItem(
  text = "Path Leg Tracking",
  tabName = "tab_plt",
  selected = T,
  icon = icon("map-marker-alt")
)

sidebar_tab_ord <- menuItem(
  text = "Optimised Runway Delivery",
  tabName = "tab_ord",
  icon = icon("plane-arrival")
)

# ----------------------------------------------------------------------- #
# Sidebar -----------------------------------------------------------------
# ----------------------------------------------------------------------- #

sidebar <- dashboardSidebar(
  collapsed	= T,
  width = 250,
  sidebarMenu(
    id = "tabs",
    sidebar_tab_db,
    sidebar_tab_plt,
    sidebar_tab_ord
  )
)

# ----------------------------------------------------------------------- #
# CSS/JS ------------------------------------------------------------------
# ----------------------------------------------------------------------- #

www <- tagList(
  tags$link(rel = "stylesheet", type = "text/css", href = "header_buttons.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "theme.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "wrappers.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "cheeky_tweaks.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "spinner.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "bttn.min.css"),
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$script(src = "pltmap.js")
)

# ----------------------------------------------------------------------- #
# Body Tab Items ----------------------------------------------------------
# ----------------------------------------------------------------------- #

body_tab_db <- tabItem(
  "tab_db",
  box(
    title = "Database Explorer",
    width = NULL,
    collapsible  = T,
    collapsed = T,
    tabBox(
      width = NULL,
      tabPanel(
        "Query Tool",
        div(
          textAreaInput(
            "db_query",
            NULL,
            placeholder = "Enter your query here...",
            width = "100%",
            height = "246px",
            resize = "vertical"
          ),
          div(
            class = "centered",
            actionButton("db_execute", "Execute", icon("play")),
            div(style = "margin: 0 5px 0 5px"),
            actionButton("db_clear", "Clear", icon("eraser"))
          ),
          DT::dataTableOutput("db_output")
        )
      ),
      tabPanel("Flight Plan",DT::dataTableOutput("db_fp_table")),
      tabPanel("Landing Pair",DT::dataTableOutput("db_lp_table")),
      tabPanel("Volumes", DT::dataTableOutput("db_volumes")),
      tabPanel("Legs", DT::dataTableOutput("db_legs"))
    )
  ),
  box(
    title = "Lookup Flight Statistics",
    width = NULL,
    collapsible = T,
    collapsed = T,
    NA
  ),
  box(
    title = "Flight Plan Statistics",
    width = NULL,
    collapsible = T,
    collapsed = T,
    tabBox(
      width = NULL,
      tabPanel("General", DT::dataTableOutput("db_fp_general_table")),
      tabPanel("Aircraft Type", DT::dataTableOutput("db_fp_type_table")),
      tabPanel("Wake", DT::dataTableOutput("db_fp_wake_table")),
      tabPanel("Runway", DT::dataTableOutput("db_fp_lrwy_table")),
      tabPanel("Runway Hourly", DT::dataTableOutput("db_fp_lrwyt_table"))
    )
  ),
  box(
    title = "Landing Pair Statistics",
    width = NULL,
    collapsible = T,
    collapsed = T,
    pickerInput("db_lp_type", "Landing Pair Type",  NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
    tabBox(
      width = NULL,
      tabPanel("Wake", DT::dataTableOutput("db_lp_wake_table")),
      tabPanel("Runway", DT::dataTableOutput("db_lp_lrwy_table")),
      tabPanel("Wake By Runway", DT::dataTableOutput("db_lp_wakerwy_table"))
    )
  ),
  box(
    title = "Adaptation Data",
    width = NULL,
    collapsible = T,
    collapsed = T,
    tabBox(
      width = NULL,
      tabPanel("Aircraft", DT::dataTableOutput("db_aircraft_adaptation_table")),
      tabPanel("Wake", DT::dataTableOutput("db_wake_adaptation_table")),
      tabPanel("DBS", DT::dataTableOutput("db_dbs_adaptation_table")),
      tabPanel("Runway", DT::dataTableOutput("db_runway_adaptation_table"))
    )
  )
)

body_tab_plt <- tabItem(
  "tab_plt",
  style = "margin: -15px;",
  box(
    style = "padding: 0",
    width = NULL,
    height = 0,
    solidHeader = T,
    leafletOutput("pltmap"),
    uiOutput("pltmap_filters_ui"),
    hidden(
      uiOutput("pltmap_time_range_ui")
    )
  )
)

body_tab_ord <- tabItem(
  "tab_ord",
  box(
    title = "ORD Calibration",
    width = NULL,
    collapsible = T,
    collapsed = T,
    div(
      style = "display: flex; justify-content: space-between; flex-wrap: wrap;",
      uiOutput("ordc_stage"),
      hr(),
      imageOutput("ordc_flow_diagram", height = "100%", width = "auto")
    )
  ),
  box(
    title = "ORD Validation",
    width = NULL,
    collapsible = T,
    collapsed = T,
    uiOutput("ordv_stage")
  ),
  box(
    title = "Adaptation Comparison",
    width = NULL,
    collapsible = T,
    collapsed = T,
    tabBox(
      width = NULL,
      tabPanel(
        "Aircraft",
        "Test"
      )
    )
  )
)

# ----------------------------------------------------------------------- #
# Loading Message ---------------------------------------------------------
# ----------------------------------------------------------------------- #

loading <- conditionalPanel(
  condition="$('html').hasClass('shiny-busy')",
  div(id="loadmessage", uiOutput("spinner"))
)

# ----------------------------------------------------------------------- #
# Body --------------------------------------------------------------------
# ----------------------------------------------------------------------- #

body <- dashboardBody(
  useShinyjs(),
  www,
  tabItems(
    body_tab_db,
    body_tab_plt,
    body_tab_ord
  ),
  loading
)

# ----------------------------------------------------------------------- #
# Page --------------------------------------------------------------------
# ----------------------------------------------------------------------- #

dashboardPage(
  skin = "red",
  header,
  sidebar,
  body
)
