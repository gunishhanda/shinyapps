library(shiny)
library(ggplot2)
library(ggthemes)

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
    
    

    clusterkm <- kmeans(cbind(test$scaledx, test$scaledy),centers = input$k, nstart = input$nstart, iter.max = input$itermax)
    test$clusterkm <- clusterkm$cluster
    if(input$labels == TRUE){
      ggplot(test, aes(x = test$scaledx, y = test$scaledy, col = as.factor(test$clusterkm),label = test$Area.Name)) + geom_point(size = 2)+ geom_label(size = 4, alpha = 0.3) + labs(x = "Illiteracy percentage", y = "Graduate and above percentage",colour= "Cluster") + theme_minimal() + scale_x_continuous(expand = c(0.2,0.05)) + scale_y_continuous(expand = c(0.05,0.05))  + coord_cartesian(xlim = ranges$x , ylim = ranges$y , expand = TRUE) +  theme(legend.position="none")
    } else{
      ggplot(test, aes(x = test$scaledx, y = test$scaledy, col = as.factor(test$clusterkm))) + geom_point(size = 2) + labs(x = "Illiteracy percentage", y = "Graduate and above percentage",colour= "Cluster") + theme_minimal() + scale_x_continuous(expand = c(0.2,0.05)) + scale_y_continuous(expand = c(0.05,0.05))  + coord_cartesian(xlim = ranges$x , ylim = ranges$y , expand = TRUE) +  theme(legend.position="none")
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
    
    within <- rnorm(20)
    for(i in 1:20){
      km <- kmeans(cbind(test$scaledx, test$scaledy),center = i, nstart = 20)
      within[i] <-  km$betweenss/km$totss
    }
    within <- as.data.frame(within)
   # colnames(within) <-"error"
    #3 w1 <- subset(within,row.names(within)==as.character(input$k))
    # w1 <- as.data.frame(within[input$k])
    within$selected <- ifelse(row.names(within)==input$k,1,0)
   ggplot(within, aes(x = seq(from = 1 , to = 20), y = within,col= as.factor(selected))) + geom_point(aes(size = as.factor(selected))) + scale_size_discrete(range=c(2,4)) + labs(x = "Number of Clusters", y = "R - Sq Value") + theme_minimal() +  theme(legend.position="none")
   
  })
  
  
})
