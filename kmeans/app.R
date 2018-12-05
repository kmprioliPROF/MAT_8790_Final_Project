# Katherine M. Prioli
# MAT 8790 Final Project - k-means cluster sensitivity analysis
# Wed Dec 05 16:46:28 2018 ------------------------------


#### Loading libraries and data ----

library(shiny)
library(tidyverse)
library(readxl)
library(wesanderson)   # For Wes Anderson palette
library(ggthemr)       # For prettifying plot framework

ggthemr("fresh")       # Establishes blank plot canvas with dashed gridlines

clusterdata <- read_csv("www/clusterdata.csv")             # Establishing dataset
wes <- wes_palette("FantasticFox1", 5, type = "discrete")  # Establishing color scheme of plot
set.seed(19811221)                                         # Ensuring stable performance


#### UI side code ----

ui <- pageWithSidebar(
  headerPanel("Quality of Life Analysis:  K-Means Clustering"),
  sidebarPanel(
    selectInput(inputId = "xcol",
                label = "x Variable",
                choices = names(clusterdata),
                selected = names(clusterdata)[[10]]),
    selectInput(inputId = "ycol",
                label = "y Variable",
                choices = names(clusterdata),
                selected = names(clusterdata)[[2]]),
    numericInput(inputId = "clusters",
                 label = "Number of clusters", 
                 value = 4,
                 min = 1,
                 max = 5)
  ),
  mainPanel(
    plotOutput('kmeans')
  )
)


#### Server side code ----

server <- function(input, output, session) {
  selectedData <- reactive({
    clusterdata[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    set.seed(19811221)
    kmeans(selectedData(), input$clusters)
  })
  
  output$kmeans <- renderPlot({
    # Need to add the size & shape arguments used in .Rmd
    ggplot(data = selectedData(), aes(x = selectedData()[[1]], 
                                      y = selectedData()[[2]], 
                                      color = factor(clusters()$cluster))) +
      geom_point(size = 3) +
      scale_color_manual(values = wes)  
  })
  
}

shinyApp(ui = ui, server = server)