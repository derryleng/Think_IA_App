header <- dashboardHeader(
  title = imageOutput("think_logo"),
  titleWidth = sidebarWidth,
  headerButtonUI("db_button", "database")
)

sidebar <- dashboardSidebar(
  width = sidebarWidth,
  sidebarMenu(
    id = "tabs",
    lapply(names(sidebarNameIcon), function(x) {
      tab_title <- sidebarNameIcon[[x]][1]
      tab_icon <- sidebarNameIcon[[x]][2]
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
      sapply(list.files("modules"), function(x) {
        paste0("tabItem(\"", x, "\",", x, "_ui(\"", x ,"\")", ")", collapse = ",")
      }),
      collapse = ","
    ), ")"
  ),
  
  # Loading spinner dialogue
  conditionalPanel(
    condition="$('html').hasClass('shiny-busy')",
    div(id = "loading_spinner", uiOutput("spinner"))
  )
  
)

ui <- dashboardPage(skin = "red", header, sidebar, body)
