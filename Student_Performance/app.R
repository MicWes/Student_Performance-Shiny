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
ui <- fluidPage(theme = "bootstrap.min.css",
                
                # Application title
                titlePanel("Student Performance"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                    sidebarPanel(
                        sliderInput("bins",
                                    "Number of bins:",
                                    min = 1,
                                    max = 50,
                                    value = 30)
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                        plotOutput("distPlot")
                    )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    d1=read.table("student-mat.csv",sep=";",header=TRUE)
    
    d2=read.table("student-por.csv",sep=";",header=TRUE)
    
    d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
    
    #M <- cor(d3)
    #corrplot(M, type="upper", order="hclust", col=brewer.pal(n=8, name="RdYlBu"))
    
    #View(d1)
    summary(d3)
    
    sapply(d3,is.numeric)
    
    d3numeric <- d3[,sapply(d3, is.numeric)]

    output$distPlot <- renderPlot({
        matriz_cor <- cor(d3numeric)
        corrplot(matriz_cor, method="ellipse")
        View(matriz_cor, type="upper", order="hclust", col=brewer.pal(n=8, name="RdYlBu"))
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
