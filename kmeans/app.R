# Katherine M. Prioli
# MAT 8790 Final Project - k-means cluster sensitivity analysis
# Sun Dec 09 12:27:18 2018 ------------------------------


#### Loading libraries and data ----

library(shiny)
library(tidyverse)
library(readxl)
library(wesanderson)   # For Wes Anderson palette
library(ggthemr)       # For prettifying plot framework

ggthemr("fresh")       # Establishes blank plot canvas with dashed gridlines
wes <- wes_palette("Darjeeling1", 5, type = "discrete")    # Establishing color scheme of plot

clusterdata <- read_csv("www/clusterdata_2018-12-09.csv")  # Establishing dataset
clusterdata <- clusterdata %>% 
  select(-GDP_USD_2018) %>% 
  select(country, 
         US, 
         color, 
         HDI_cat, 
         SPI, 
         HDIrank, 
         HDIindex, 
         happiness, 
         genderequality_index, 
         infantmort, 
         birth_MF, 
         sixty_MF, 
         logGDP) %>% 
  mutate(sixty_MF = sixty_MF + 60) %>% 
  rename(`Social Progress Index` = SPI,
         `Human Development Index Rank` = HDIrank,
         `Human Development Index` = HDIindex,
         `Happiness Score` = happiness,
         `Gender Equality Index` = genderequality_index,
         `Infant Mortality Rate` = infantmort,
         `Total Life Expectancy at Birth` = birth_MF,
         `Total Life Expectancy at Sixty` = sixty_MF,
         `Gross Domestic Product (log-transformed)` = logGDP) %>% 
  mutate(US_size = case_when(
    US == "US" ~ 6,
    TRUE       ~ 3
  )) %>% 
  mutate(HDI_cat = factor(HDI_cat, levels = c("Low", "Medium", "High", "Very High")))

set.seed(19811221)                                         # Ensuring stable performance


#### UI side code ----

ui <- fluidPage(
  titlePanel(tags$h4(span(HTML("<center>MAT 8790 Final Project </center>"), style = "color: #545454; font-weight: bold")),
             windowTitle = "MAT 8790 Final Project - QoL K-Means Clustering Analysis"),
  tags$h1(span(HTML("<center>Quality of Life Analysis:  K-Means Clustering </center>"), style = "color: #00A08A; font-weight: bold")),
  br(), br(),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        "To perform a sensitivity analysis on the Quality of Life clustering results, choose the variables of interest and number of clusters below.  The default cluster number is 4, consistent with the base case analysis.  This model supports up to 5 clusters.",
        br(), br(),
        selectInput(inputId = "xcol",
                    label = "x Variable",
                    choices = names(clusterdata)[5:13],
                    selected = names(clusterdata)[[8]]),
        selectInput(inputId = "ycol",
                    label = "y Variable",
                    choices = names(clusterdata)[5:13],
                    selected = names(clusterdata)[[9]]),
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
  ),
  fluidRow(HTML("<center"),
           br(), br(), br(),
           HTML(paste("© 2018 Katherine M. Prioli.  For more information including full code, report, and data sources, visit the ",
                      tags$a(href = "https://github.com/kmprioliPROF/MAT_8790_Final_Project", 
                             target = "_blank", "GitHub repository"), " for this project."
           )),
           HTML("</center>"))
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
    ggplot(data = selectedData(), aes(x = selectedData()[[1]], 
                                      y = selectedData()[[2]], 
                                      color = factor(clusters()$cluster),
                                      shape = clusterdata$HDI_cat)) +
      geom_point(size = clusterdata$US_size) +
      scale_color_manual(values = wes) +
      scale_shape_manual(values = c(18, 17, 15, 16)) +
      guides(size = FALSE, shape = guide_legend(reverse = TRUE, title = "HDI Category"), 
             color = guide_legend(title = "Cluster")) +
      xlab(paste(input$xcol)) +
      ylab(paste(input$ycol)) +
      ggtitle("Clustering Sensitivity Analysis") + 
      theme(plot.title = element_text(size = 18, face = "bold"))
  })
}


shinyApp(ui = ui, server = server)