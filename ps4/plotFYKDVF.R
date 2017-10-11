# Plot function
plotComparisonFYKD_B <- function(x, ks, ntimes = 100, threshold = 800){
  
  # Define times as x-labels for each k value
  times <- sapply(ks, function(k) {
    
    # Create labels and extract the relevant information
    op <- microbenchmark(
      Improved_FYKDV2 = FYKD2_V2(x, k, threshold),
      Improved_FYKDV3 = FYKD3_V3(x, k, threshold),
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
    
    ggtitle(paste("FYKD Comparison [x size: ", as.character(length(x)), "]", sep = "")) 
  
}



# Plot function (With original FYKD)
plotComparisonFYKD_All_AB <- function(x, ks, ntimes = 100, threshold = 800){
  
  # Define times as x-labels for each k value
  times <- sapply(ks, function(k) {
    
    # Create labels and extract the relevant information
    op <- microbenchmark(
      FYKD     = FYKD(x, k),
      Improved_FYKDV2 = FYKD2_V2(x, k, threshold),
      Improved_FYKDV3 = FYKD3_V3(x, k, threshold),
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
    
    ggtitle(paste("FYKD Comparison [x size: ", as.character(length(x)), "]", sep = "")) 
  
}

# Plot function (With original FYKD)
plotComparisonFYKD_All_A <- function(x, ks, ntimes = 100, threshold = 800){
  
  # Define times as x-labels for each k value
  times <- sapply(ks, function(k) {
    
    # Create labels and extract the relevant information
    op <- microbenchmark(
      FYKD     = FYKD(x, k),
      New_FYKDV1 = FYKDV1(x, k),
      New_FYKDV2 = FYKDV2(x, k),
      New_FYKDV3 = FYKDV3(x, k),
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
    
    ggtitle(paste("FYKD Comparison [x size: ", as.character(length(x)), "]", sep = "")) 
  
}
