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
    titlePanel("ARMA generator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            textInput('theta1','theta1',value=0.5),
            textInput('theta2','theta2',value=0.5),
            textInput('phi1','phi1',value=0.5),
            textInput('phi2','phi2',value=0),
            numericInput('n','time steps',value=100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("tsplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$tsplot <- renderPlot({
        set.seed(1234)
        sigma=1
        e=numeric(input$n)
        e[1]=rnorm(1,0,sigma);e[2]=rnorm(1,0,sigma)
        x=numeric(input$n)
        x[1]=e[1]
        x[2]=e[2]+as.numeric(input$theta1)*e[1]
        for(i in 3:input$n){
            e[i]=rnorm(1,0,sigma)
            x[i]=e[i]+as.numeric(input$phi1)*x[i-1]+as.numeric(input$phi2)*x[i-2]+
                as.numeric(input$theta1)*e[i-1]+as.numeric(input$theta2)*e[i-2]
        }
        plot(x,type='l',ylim=c(-5,5))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
