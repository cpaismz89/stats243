# Plot function
plotComparison <- function(x, ks, tocompareName, ntimes = 100, threshold = 800){
  
  # Define times as x-labels for each k value
  times <- sapply(ks, function(k) {
    
    # Create labels and extract the relevant information
    op <- microbenchmark(
      New_PIKK = PIKKVF(x, k, threshold),
      RSample  = RSample(x, k),
      times = ntimes
    )
    
    by(op$time, op$expr, function(t) mean(t) / 1000)
  }
  )
  
  # Transponse k-values and create a data frame 
  times <- t(times)
  times <- as.data.frame(cbind(times, k = ks))
  
  # Define the time values and keys of the plot 
  times <- gather(times, -k, key = "fun", value = "time")
  pd <- position_dodge(width = 0.2)
  
  # Create the plot
  ggplot(times, aes(x = k, y = time, group = fun, color = fun)) +
    theme(axis.line = element_line(size = 1, colour = "black"),
          panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank()) +
    
    geom_point(position = pd) +
    geom_line(position = pd) +
    
    xlab("k Values") +
    ylab("Time [microseconds]") +
    
    ggtitle(paste("PIKK Comparison [x size: ", as.character(length(x)), "]", sep = "")) 
  
}



# Plot function (With original PIKK)
plotComparisonAll <- function(x, ks, tocompareName, ntimes = 100, threshold = 800){
  
  # Define times as x-labels for each k value
  times <- sapply(ks, function(k) {
    
    # Create labels and extract the relevant information
    op <- microbenchmark(
      PIKK     = PIKK(x, k),
      New_PIKK = PIKKVF(x, k, threshold),
      RSample  = RSample(x, k),
      times = ntimes
    )
    
    by(op$time, op$expr, function(t) mean(t) / 1000)
  }
  )
  
  # Transponse k-values and create a data frame 
  times <- t(times)
  times <- as.data.frame(cbind(times, k = ks))
  
  # Define the time values and keys of the plot 
  times <- gather(times, -k, key = "fun", value = "time")
  pd <- position_dodge(width = 0.2)
  
  # Create the plot
  ggplot(times, aes(x = k, y = time, group = fun, color = fun)) +
    theme(axis.line = element_line(size = 1, colour = "black"),
          panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank()) +
    
    geom_point(position = pd) +
    geom_line(position = pd) +
    
    xlab("k Values") +
    ylab("Time [microseconds]") +
    
    ggtitle(paste("PIKK Comparison [x size: ", as.character(length(x)), "]", sep = "")) 
  
}


# Plot function (With original PIKK)
plotComparisonPIKKAll_A <- function(x, ks, tocompareName, ntimes = 100, threshold = 800){
  
  # Define times as x-labels for each k value
  times <- sapply(ks, function(k) {
    
    # Create labels and extract the relevant information
    op <- microbenchmark(
      PIKK     = PIKK(x, k),
      New_PIKK1 = PIKKV1(x, k),
      New_PIKK2 = PIKKV2(x, k),
      New_PIKKVF = PIKKVF(x, k, threshold),
      RSample  = RSample(x, k),
      times = ntimes
    )
    
    by(op$time, op$expr, function(t) mean(t) / 1000)
  }
  )
  
  # Transponse k-values and create a data frame 
  times <- t(times)
  times <- as.data.frame(cbind(times, k = ks))
  
  # Define the time values and keys of the plot 
  times <- gather(times, -k, key = "fun", value = "time")
  pd <- position_dodge(width = 0.2)
  
  # Create the plot
  ggplot(times, aes(x = k, y = time, group = fun, color = fun)) +
    theme(axis.line = element_line(size = 1, colour = "black"),
          panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank()) +
    
    geom_point(position = pd) +
    geom_line(position = pd) +
    
    xlab("k Values") +
    ylab("Time [microseconds]") +
    
    ggtitle(paste("PIKK Comparison [x size: ", as.character(length(x)), "]", sep = "")) 
  
}