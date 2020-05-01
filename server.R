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
  
}
