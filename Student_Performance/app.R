#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(datasets)
library(corrplot)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    # Application title
    titlePanel("Student Performance"),
               
    # layout

    
    hr(),
    # Show a plot of the correlations
    mainPanel(
        inputPanel(
            selectInput("student", "Choose Course Correlation:",
                        choices = list("Math" = 1, "Portuguese" = 2, "Merge (Math+Portuguese)" = 3),
                        selected = 1
            )  
        ),
        plotOutput("corrPlot"),
        hr(),
        actionButton("button", "Description"),
        hidden(
            div(id='text_div',
                verbatimTextOutput("text")
            )
        )
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
    
    observeEvent(input$button, {
        toggle('text_div')
        output$text <- renderText({"Attribute Information:
# Attributes for both student-mat.csv (Math course) and student-por.csv (Portuguese language course) datasets:
age - student's age (numeric: from 15 to 22)
Medu - mother's education (numeric: 0 - none, 1 - primary education (4th grade), 2 â€“ 5th to 9th grade, 3 â€“ secondary education or 4 â€“ higher education)
Fedu - father's education (numeric: 0 - none, 1 - primary education (4th grade), 2 â€“ 5th to 9th grade, 3 â€“ secondary education or 4 â€“ higher education)
traveltime - home to school travel time (numeric: 1 - <15 min., 2 - 15 to 30 min., 3 - 30 min. to 1 hour, or 4 - >1 hour)
studytime - weekly study time (numeric: 1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours)
failures - number of past class failures (numeric: n if 1<=n<3, else 4)
famsup - family educational support (binary: yes or no)
freetime - free time after school (numeric: from 1 - very low to 5 - very high)
goout - going out with friends (numeric: from 1 - very low to 5 - very high)
Dalc - workday alcohol consumption (numeric: from 1 - very low to 5 - very high)
Walc - weekend alcohol consumption (numeric: from 1 - very low to 5 - very high)
health - current health status (numeric: from 1 - very bad to 5 - very good)
absences - number of school absences (numeric: from 0 to 93)

# these grades are related with the course subject, Math or Portuguese:
G1 - first period grade (numeric: from 0 to 20)
G2 - second period grade (numeric: from 0 to 20)
G3 - final grade (numeric: from 0 to 20, output target)
"})
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
