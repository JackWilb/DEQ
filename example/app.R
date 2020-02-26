#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Diamonds Numeric Variables Scatterplot"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(
           "variable1",
           "X-axis Variable",
           diamonds[, unlist(lapply(diamonds, is.numeric))] %>% names()
         ),
         
         selectInput(
           "variable2",
           "Y-axis Variable",
           diamonds[, unlist(lapply(diamonds, is.numeric))] %>% names()
         ),
         sliderInput(
           "x_range",
           "X-axis range",
           min = 0,
           max = 20,
           value = c(0, 20),
           dragRange = TRUE
          ),
         sliderInput(
           "y_range",
           "Y-axis range",
           min = 0,
           max = 20,
           value = c(0, 20),
           dragRange = TRUE
         ),
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   output$plot <- renderPlot({
      ggplot(diamonds, aes(x = get(input$variable1), y = get(input$variable2))) + 
       geom_point() +
       labs(
         x = input$variable1,
         y = input$variable2
       ) + 
       scale_x_continuous(limits = c(input$x_range[1], input$x_range[2])) + 
       scale_y_continuous(limits = c(input$y_range[1], input$y_range[2]))
   })
   
   observe({
     x_min = min(diamonds[, input$variable1])
     x_max = max(diamonds[, input$variable1])
     updateSliderInput(session, "x_range",
                       min = x_min,
                       max = x_max,
                       value = c(x_min, x_max))
     
     y_min = min(diamonds[, input$variable2])
     y_max = max(diamonds[, input$variable2])
     updateSliderInput(session, "y_range",
                       min = y_min,
                       max = y_max,
                       value = c(y_min, y_max))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

