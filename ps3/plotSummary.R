# ---- plotSummary-ext ----
# Declaring the function
plotSummary <- function(X, Y, df, xlab = "Years", ylab, color = "red"){
  
  # Loading libraries
  library(ggplot2)
  
  # Defining the plot
  plotfile <- ggplot() +
    
    # Modify the borders, grid and lines colors and format
    theme(axis.line = element_line(size = 1, colour = "black"),
          panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank()) +
    
    # Modify the font and legend format
#    theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
#          text = element_text(family = "Tahoma"),
#          axis.text.x = element_text(colour = "black", size = 10),
#          axis.text.y = element_text(colour = "black", size = 10),
#          legend.key = element_rect(fill = "white", colour = "white"))+
    
    # Add the line to the graph
    geom_line(aes(y = Y, x = X, group = 1), size = 1.2,
              data = df, stat = "identity", colour = color, alpha = 0.5) +
    
    # Vertical labels for x-axis
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    
    # Check if we have explicit labels: define labels and title
    if ((!is.null(xlab)) & (!is.null(ylab))){
      labs(x = xlab, y = ylab, 
           title = paste(ylab, " in Shakespeare's plays vs ", xlab, sep = ''))  
    }
    # Else, a generic name is given
    else{
      labs(title = paste("Number of Y in Shakespeare's plays vs Years", sep = ''))  
    }
  
  # Plot the current graph
  print(plotfile)
}