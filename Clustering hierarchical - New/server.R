library(shiny)
library(ggplot2)
library(ggthemes)
library(dendextend)


shinyServer(function(input, output){
  set.seed(1000)
  test <- as.data.frame(read.csv("Main.csv"))
  
  # test$scaledx <-    switch(input$scale,
  #          "Scale both mean and variance" = scale(test$Percentage.Illiterate, center = TRUE, scale = TRUE),
  #          "Scale only variance"= scale(test$Percentage.Illiterate, center = FALSE, scale = TRUE),
  #          "Do not scale data"= test$Percentage.Illiterate)
  
  
  
  output$cluster1 <- renderPlot({
    
    set.seed(1000)
    
    if (input$scale == c("Scale both mean and variance")){
      test$scaledx <- scale(test$Percentage.Illiterate, center = TRUE, scale = TRUE)
      test$scaledy <- scale(test$Percentage.Graduate...above, center = TRUE, scale = TRUE)
    } else if (input$scale == c("Scale only variance")){
      test$scaledx <- scale(test$Percentage.Illiterate, center = FALSE, scale = TRUE)
      test$scaledy <- scale(test$Percentage.Graduate...above, center = FALSE, scale = TRUE)
    } else{
      test$scaledx <- test$Percentage.Illiterate
      test$scaledy <- test$Percentage.Graduate...above
    }
    
    
    if (input$method == c("Complete Linkage - Maximum Distance")){
      mthd ="complete"
    } else if (input$method == c("Single Linkage - Minimum Distance")){
      mthd ="single"
    } else{
      mthd ="average"
    }
    hc <- hclust(dist(cbind(test$scaledx, test$scaledy)), method = mthd)
    
    
    clusterhc <- cutree(hc, k = input$k)
    
    
    test$clusterhc <- clusterhc
    
    if(input$labels == TRUE){
      ggplot(test, aes(x = test$scaledx, y = test$scaledy, col = as.factor(test$clusterhc),label = test$Area.Name)) + geom_point(size = 2)+ geom_label(size = 4, alpha = 0.3) + labs(x = "Illiteracy percentage", y = "Graduate and above percentage",colour= "Cluster") + theme_minimal() + scale_x_continuous(expand = c(0.2,0.05)) + scale_y_continuous(expand = c(0.05,0.05))  + coord_cartesian(xlim = ranges$x , ylim = ranges$y , expand = TRUE) +  theme(legend.position="none")
    } else{
      ggplot(test, aes(x = test$scaledx, y = test$scaledy, col = as.factor(test$clusterhc))) + geom_point(size = 2) + labs(x = "Illiteracy percentage", y = "Graduate and above percentage",colour= "Cluster") + theme_minimal() + scale_x_continuous(expand = c(0.2,0.05)) + scale_y_continuous(expand = c(0.05,0.05))  + coord_cartesian(xlim = ranges$x , ylim = ranges$y , expand = TRUE) +  theme(legend.position="none")
    }
    
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$cluster1_dblclick, {
    brush <- input$cluster1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$cluster2 <- renderPlot({
    
    
    set.seed(1000)
    
    if (input$scale == c("Scale both mean and variance")){
      test$scaledx <- scale(test$Percentage.Illiterate, center = TRUE, scale = TRUE)
      test$scaledy <- scale(test$Percentage.Graduate...above, center = TRUE, scale = TRUE)
    } else if (input$scale == c("Scale only variance")){
      test$scaledx <- scale(test$Percentage.Illiterate, center = FALSE, scale = TRUE)
      test$scaledy <- scale(test$Percentage.Graduate...above, center = FALSE, scale = TRUE)
    } else{
      test$scaledx <- test$Percentage.Illiterate
      test$scaledy <- test$Percentage.Graduate...above
    }
    
    
    if (input$method == c("Complete Linkage - Maximum Distance")){
      mthd ="complete"
    } else if (input$method == c("Single Linkage - Minimum Distance")){
      mthd ="single"
    } else{
      mthd ="average"
    }
    
    hc1 <- hclust(dist(cbind(test$scaledx, test$scaledy)), method = mthd)
    
    hcd <- as.dendrogram(hc1)
    par(cex = 0.7, mar = c(15,3,3,1))
    colLab <- function(n) {
      if(is.leaf(n)) {
        a <- attributes(n)
        attr(n, "label") <- substr(a$label,1,2)             #  change the node label 
      }
      n
    }
    labels(hcd) <- test$Area.Name[order.dendrogram(hcd)] 
    hcd1 <- dendrapply(hcd,colLab)
    plot(hcd,xlab = "", ylab = "Separation",main = "Dendrogram")
    
    
    rect.dendrogram(hcd1, input$k,lower_rect = 0)
    
    
  })
})


