#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
# Define UI for application that gives choices between the datasets "airquality", "mtcars", "iris" and a choise between the variables 

ui <- fluidPage(
   
   # Application title
   titlePanel("The Data Explorer"),
   
   # short description of the context 
   h4("Select a dataset and choose which variables you want to plot 
      and get its linear regression  
      and adjusted r² value (default data set = mtcars):"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
       sidebarPanel(
           
           # input boxes to define the dataset to plot 
           checkboxInput("dataset1","Choose airquality", value =F),
           checkboxInput("dataset2","Choose mtcars", value =F),
           checkboxInput("dataset3","Choose iris", value =F),
           
           # col numbers of the dataset  
         h5("max col number in this dataset:"),
         verbatimTextOutput("colLength"),
         
         # slider for the variable to select 
         sliderInput("variablechoosen", "choose variable y-axis:",
                     min = 1, 
                     max = 11,
                     value = 2),
         sliderInput("variablechoosen1", "choose variable x-axis:",
                     min = 1, 
                     max = 11,
                     value = 2)
      ),
   
      
      mainPanel(
          
          # Show a plot of the chosen variables 
        plotlyOutput("newPlot"),
        h4("adj.r.squared value:"),
        
        # Show the correlation 
        textOutput(outputId = "out1"),
        h5("  
           for more information on the used here datasets and variables:"),
        uiOutput("tab")
        
      )
    ) 
)



# Define server logic required to draw a histogram

server <- function(input, output) {
library(plotly)
library(dplyr)

   output$newPlot <- renderPlotly({
       
       
       # choose the dataset 
       if(input$dataset1){
           plottitle <- "airquality"
           dataToPlot <- airquality %>% filter(!is.na(Ozone)) %>% filter(!is.na(Solar.R)) 
       } else if(input$dataset2){
           plottitle <- "mtcars"
           dataToPlot <- mtcars
       } else if(input$dataset3){
           plottitle <- "iris"
           dataToPlot <- iris
       } else{
           dataToPlot <- mtcars 
           plottitle <- "mtcars"
       }

       # n columns in the dataset defintion
       output$colLength <- renderText(ncol(dataToPlot))
       
       # linear regression 
       fit <- lm(dataToPlot[,input$variablechoosen] ~ dataToPlot[,input$variablechoosen1], data = dataToPlot)
       output$out1 <- renderText(summary(fit)$adj.r.squared)
       
       # plotly 
       dataToPlot %>% plot_ly(x= dataToPlot[,input$variablechoosen1]) %>%
                                layout(title = paste0("dataset: ",plottitle),
                                 xaxis = list(title = colnames(dataToPlot)[input$variablechoosen1]),
                                 yaxis = list(title = colnames(dataToPlot)[input$variablechoosen])) %>% 
           add_markers(y = ~dataToPlot[,input$variablechoosen]) %>% 
           add_lines(x = ~ dataToPlot[,input$variablechoosen1], y = fitted(fit))
   })
   
   # further information
   url <- a("https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/00Index.html", href="https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/00Index.html")
   output$tab <- renderUI({
       tagList("URL link:", url)
       })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

