# Define server logic required to draw a histogram
library('shiny')

server <- function(input, output) {
  
  output$AnglePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- as.numeric(unlist(strsplit(input$angles.txt,",")))

    # draw the histogram with the specified number of bins
    Cplot2(x)#, alpha, ax, rho.col, out.by, kappa.ci)
  })
}
