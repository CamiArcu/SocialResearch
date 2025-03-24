library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  titlePanel("Timeline Slider Under Plot"),
  
  mainPanel(
    plotOutput("myPlot"),
    
    # Slider input acting as timeline
    sliderTextInput("timePoint", "Timeline:", 
                    choices = c("Jan", "Feb", "Mar", "Apr", "May"), 
                    selected = "Mar")
  )
)

server <- function(input, output, session) {
  output$myPlot <- renderPlot({
    plot(1:10, type = "l", col = "blue", main = "Timeline Plot")
    abline(v = input$timePoint, col = "red", lwd = 2)  # Show selected point on plot
  })
}

shinyApp(ui, server)
