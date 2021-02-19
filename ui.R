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
