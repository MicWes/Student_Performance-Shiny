#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
library(corrplot)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Student Performance"),
               
    # layout

    inputPanel(
        selectInput("student", "Choose a correlation:",
            choices = list("Student Mat" = 1, "Student Por" = 2, "Student Merge" = 3),
            selected = 1
            )  
    ),
    hr(),
    # Show a plot of the correlations
    mainPanel(
        plotOutput("corrPlot")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    d1=read.table("student-mat.csv",sep=";",header=TRUE)
    
    d2=read.table("student-por.csv",sep=";",header=TRUE)
    
    d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
    
    #View(d3)
    summary(d1)
    
    summary(d2)
    
    summary(d3)
    
    sapply(d1,is.numeric)
    
    sapply(d2,is.numeric)
    
    sapply(d3,is.numeric)
    
    d1numeric <- d1[, sapply(d1, is.numeric)]

    d2numeric <- d2[, sapply(d2, is.numeric)]
    
    d3numeric <- d3[, sapply(d3, is.numeric)]
    
    output$corrPlot <- renderPlot({
        
        if (input$student == 1) {
            matriz_cor1 <- cor(d1numeric)
            corrplot(matriz_cor1, method="color")
            #View(matriz_cor)
        }
        else if (input$student == 2) {
            matriz_cor2 <- cor(d2numeric)
            corrplot(matriz_cor2, method="color")
            #View(matriz_cor) 
        }
        else {
            matriz_cor3 <- cor(d3numeric)
            corrplot(matriz_cor3, method="color")
            #View(matriz_cor)
        }
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
