library(ggplot2)
library(plotly)
library(shiny)

# Create a sample ggplot object
p_test <- ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  theme(legend.position = "bottom")

# Shiny app
ui <- fluidPage(
  plotlyOutput("testPlot")
)

server <- function(input, output) {
  output$testPlot <- renderPlotly({
    p_test <- plotly_build(p_test)
    p_test %>% 
      layout(legend = list(orientation = "h", y = -3)) %>% 
      htmlwidgets::onRender("function(el) {
      el.querySelector('.legend').style = 'position: relative; left: 0; top: 0; width: auto; height: auto;';
    }")
  })
}

shinyApp(ui, server)
