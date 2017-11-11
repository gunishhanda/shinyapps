  library(shiny)
library(gridExtra)

shinyUI(fluidPage(
  titlePanel("Clustering"),
  
  
  sidebarLayout(
    sidebarPanel(
      h4("Factors in Hierarchical Algorithm"),
      
      radioButtons("scale", 
                  label = "Scale Data",
                  choices = c("Scale both mean and variance",
                  "Scale only variance",
                  "Do not scale data"),
                  selected = "Scale both mean and variance"),
      
      sliderInput("k", 
                label = "Number of Clusters",
                min = 2, max = 20, value = 1),
      
      
      radioButtons("method", 
                   label = "Clustering Method",
                   choices = c("Complete Linkage - Maximum Distance",
                               "Single Linkage - Minimum Distance",
                               "Average Linkage - Mean"),
                   selected = "Complete Linkage - Maximum Distance")
    ),
    mainPanel(h2("Cluster Formation"),
              
              checkboxInput("labels", 
                           label = "Labels",
                           value = TRUE),
      
      fluidRow(

         column(12,plotOutput("cluster1",
                             dblclick = dblclickOpts("cluster1_dblclick"),
                             brush = brushOpts(
                               id = "cluster1_brush",
                               resetOnNew = TRUE))

                ) ,
        column(12, plotOutput("cluster2"))
      )                
    )
  )
)
)


