ToPlotF <- function(filename, Title, color = "#ff3333"){
  # Reaf the resulting file (processed in Spark)
  dat <- read.csv(filename)
  
  # Change column names
  names(dat) <- c('date','time','lang','hits')
  
  # Date and time as characters
  dat$date <- as.character(dat$date)
  dat$time <- as.character(dat$time)
  
  # Time format
  dat$time[dat$time %in%  c("0", "1")] <- "000000"
  wh <- which(nchar(dat$time) == 5)
  dat$time[wh] <- paste0("0", dat$time[wh])
  
  # Chron object: Date and time format
  dat$chron <- chron(dat$date, dat$time,
                     format = c(dates = 'ymd', times = "hms"))
  
  # Filter dataset: English, Spanish, and French
  ENdat <- dat %>% filter(dat$lang == 'en')
  SPdat <- dat %>% filter(dat$lang == 'es')
  FRdat <- dat %>% filter(dat$lang == 'fr')
  CHdat <- dat %>% filter(dat$lang == 'zh')
  
  # Date format
  ENdat$date <- as.Date(ENdat$date, "%Y%m%d") 
  SPdat$date <- as.Date(SPdat$date, "%Y%m%d") 
  FRdat$date <- as.Date(FRdat$date, "%Y%m%d") 
  CHdat$date <- as.Date(CHdat$date, "%Y%m%d") 
  
  # Time zone format for Date and Hour
  ENdat$chron <- as.POSIXlt(ENdat$chron, tz="EST")
  SPdat$chron <- as.POSIXlt(SPdat$chron, tz="EST")
  FRdat$chron <- as.POSIXlt(FRdat$chron, tz="EST")
  CHdat$chron <- as.POSIXlt(CHdat$chron, tz="EST")
  
  # Order by date (for simplicity)
  ENdat <- ENdat[order(ENdat$chron),]
  SPdat <- SPdat[order(SPdat$chron),]
  FRdat <- FRdat[order(FRdat$chron),]
  CHdat <- CHdat[order(CHdat$chron),]
  
  # Scatter plot
  pdf(paste(filename,'-traffic.pdf', sep = ""), width = 9, height = 5)
  plotfile <- ggplot(ENdat, aes(x=date,y=hits)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.line = element_line(size = 1, colour = "black"),
          panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank()) +
    theme(legend.position="none") +
    geom_point(colour = color, alpha = 0.5) +
    scale_x_date(breaks = date_breaks("weeks"), labels = date_format("%d-%b")) +
    scale_y_continuous(breaks = c(0, 1e5, 2e5, 3e5)) +
    labs(x = "Time [Weeks]", y = "Number of hits", 
         title = paste(Title, " frequency vs Time [weeks]", sep = ''))   
  
  print(plotfile)
  dev.off()
  
  
  # Line plot
  pdf(paste(filename,'-traffic-line.pdf', sep = ""), width = 9, height = 5)
  plotfile2 <- ggplot(ENdat, aes(x=chron,y=hits)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.line = element_line(size = 1, colour = "black"),
          panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank()) +
    theme(legend.position="none") +
    geom_line(colour = color, alpha = 0.8) +
    labs(x = "Time [months]", y = "Number of hits", 
         title = paste(Title, " frequency vs Time [months]", sep = "")) 
  print(plotfile2)
  dev.off()
  
  
  toReturn <- list(ENdat, SPdat, FRdat, CHdat)
  return (toReturn)
}

ToPlotInd <- function(DF, Title, color = "#ff3333"){
  # Line plot
  pdf(paste(Title,'-traffic-line.pdf', sep = ""), width = 9, height = 5)
  plotfile2 <- ggplot(DF, aes(x=chron,y=hits)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.line = element_line(size = 1, colour = "black"),
          panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank()) +
    theme(legend.position="none") +
    geom_line(colour = color, alpha = 0.8) +
    labs(x = "Time [months]", y = "Number of hits", 
         title = paste(Title, " frequency vs Time [months]", sep = "")) 
  print(plotfile2)
  dev.off()
}


ToPlotMult <- function(DF1, DF2, DF3, Title){
  # Line plots
  pdf(paste(Title,'-traffic-lines.pdf', sep = ""), width = 6, height = 3)
  
  plotfile1 <- ggplot(DF1, aes(x=chron,y=hits)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.line = element_line(size = 1, colour = "black"),
          panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank()) +
    theme(legend.position="none") +
    geom_line(colour = "#ff3333", alpha = 0.8) +
    labs(x = "Time [months]", y = "Number of hits", 
         title = paste(Title, " EN frequency vs Time [months]", sep = "")) 
  print(plotfile1)
  
  plotfile2 <- ggplot(DF2, aes(x=chron,y=hits,colour= "#0066ff" )) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.line = element_line(size = 1, colour = "black"),
          panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank()) +
    theme(legend.position="none") +
    geom_line(colour = "#0066ff", alpha = 0.8) +
    labs(x = "Time [months]", y = "Number of hits", 
         title = paste(Title, " SP frequency vs Time [months]", sep = "")) 
  print(plotfile2)
  
  plotfile3 <- ggplot(DF3, aes(x=chron,y=hits)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.line = element_line(size = 1, colour = "black"),
          panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank()) +
    theme(legend.position="none") +
    geom_line(colour = "#47d147", alpha = 0.8) +
    labs(x = "Time [months]", y = "Number of hits", 
         title = paste(Title, " FR frequency vs Time [months]", sep = "")) 
  print(plotfile3)
  
  dev.off()
}