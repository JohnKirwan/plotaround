# Define server logic required to draw a histogram
library('shiny')

server <- function(input, output) {
  
  output$AnglePlot <- renderPlot({
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    #input$goButton

    df <- eventReactive(input$goButton, {
       as.numeric(unlist(strsplit(input$angles.txt,",")))
      }
    )
    Cplot2(df())#, alpha, ax, rho.col, out.by, kappa.ci)
    
      
  })
}
