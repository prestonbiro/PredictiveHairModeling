#Hair ranking game

library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  dashboardHeader(title = 'Hair Ranking Game'),
  dashboardSidebar(collapsed = T),
  dashboardBody(
    fluidRow(
      box(width = 12,height = 200,title = h2('Select the row of images with higher quality:')),
      imageOutput('ImgTop')
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  output$ImgTop <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    # filename <- normalizePath(file.path('Pictures/',
    #                                     paste('Headsho', 1791, '.HEIC', sep='')))
    filename <- 'Logo.png'
    
    # Return a list containing the filename
    list(src = filename,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = F)
}

shinyApp(ui, server)