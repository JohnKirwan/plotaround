#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# shiny webapp to run circular plotting function
# James Foster and John Kirwan
# Uses the 'circular' package

library('shiny')
#library('circular')
source('Cplot2.R')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("plotaround: plot angles around a circle"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # Simply copy and paste in values
            textInput("angles.txt",
                      "Enter at least 2 angles (in degrees) separated by commas",
                      value=""),
            verbatimTextOutput("value"),
            
            # Button to make go now
            actionButton("goButton", "Show"),       
            
        
            # Input a *.csv or tab-delimited *.txt file or even an excel file.
            # fileInput("file1", "Choose CSV File",
            #           accept = c(
            #               "text/csv",
            #               "text/comma-separated-values,text/plain",
            #               ".csv"))
            
            # Use this to include degree or radian buttons
          #  radioButtons()
            

            
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
