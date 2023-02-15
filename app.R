library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("App Demo",
  tabPanel("Page 1"),
  tabPanel("Page 2"),
  tabPanel("Page 3"),
  navbarMenu("More",
             tabPanel("Information"),
             tabPanel("About"))
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
