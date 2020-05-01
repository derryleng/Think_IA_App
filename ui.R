sidebarWidth <- "180px"

header <- dashboardHeader(
  title = imageOutput("think_logo"),
  titleWidth = sidebarWidth
  # headerButton("db", "database")
)

sidebar <- dashboardSidebar(
  width = sidebarWidth,
  sidebarMenu(
    id = "tabs"
    
  )
)

body <- dashboardBody(
  # useShinyjs(),
  lapply(list.files("www", pattern = ".css"), function(x) tags$link(rel = "stylesheet", type = "text/css", href = x)),
  lapply(list.files("www", pattern = ".js"), function(x) tags$script(src = x)),
  tags$script(HTML("$('body').addClass('fixed');")),
  # tabItems(
  #   eval(parse(text = paste0("list(", paste0(grep("^body_.*$", ls(), value = T), collapse = ","), ")")))
  # ),
  conditionalPanel(
    condition="$('html').hasClass('shiny-busy')",
    div(id = "loading_spinner", uiOutput("spinner"))
  )
)

dashboardPage(skin = "red", header, sidebar, body)
