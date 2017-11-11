library(shiny)
library(gridExtra)

shinyUI(fluidPage(
  titlePanel("Clustering"),
  
  
  sidebarLayout(
    sidebarPanel(
      h4("Factors in K-Means Algorithm"),
      
      radioButtons("scale", 
                  label = "Scale Data",
                  choices = c("Scale both mean and variance",
                  "Scale only variance",
                  "Do not scale data"),
                  selected = "Scale both mean and variance"),
      
      sliderInput("k", 
                label = "Number of Clusters",
                min = 1, max = 20, value = 1),
      
      sliderInput("itermax", 
                  label = "Number of Maximum Iterations - for convergence",
                  min = 1, max = 50, value = 1, step = 5),
      
      sliderInput("nstart", 
                  label = "Number of Random Starts",
                  min = 1, max = 50, value = 1, step = 5)
    ),
    mainPanel(h2("Cluster Formation"),
              
              checkboxInput("labels", 
                           label = "Labels",
                           value = TRUE),
      
      fluidRow(
      
         column(10,plotOutput("cluster1",
                             dblclick = dblclickOpts("cluster1_dblclick"),
                             brush = brushOpts(
                               id = "cluster1_brush",
                               resetOnNew = TRUE))
            
                ) ,
       
        
        column(2, plotOutput("cluster2",width = "200px",height = "100px"), offset = -4))
        
      )                
      
    )
      
             
  )
)
  



