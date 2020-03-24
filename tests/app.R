#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Brewhula circular plotting"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # Simply copy and paste in values
            textInput(angles.txt)
            
            # Input a *.csv or tab-delimited *.txt file or even an excel file.
            # fileInput("file1", "Choose CSV File",
            #           accept = c(
            #               "text/csv",
            #               "text/comma-separated-values,text/plain",
            #               ".csv"))
            
            # Use this to include degree or radian buttons
          #  radioButtons()
            
            # Button to make go now
          #  submitButton()
            
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("AnglePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$AnglePlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x <- c(as.numeric(angles.txt))

        # draw the histogram with the specified number of bins
        Cplot2(x)#, alpha, ax, rho.col, out.by, kappa.ci)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
