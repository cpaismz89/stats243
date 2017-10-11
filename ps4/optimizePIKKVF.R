# Loading libraries
library(microbenchmark)
library(tidyr)
library(pryr)
library(ggplot2)

# Testing data
x <- 1:1e4
ks  <- c(1:10, seq(20,90,10),seq(100,length(x)/100,100)) 
thresholds <- c(seq(10, 100, 10), seq(200, 1000, 100))

## Microbenchmarks
# Microbenchmark loop (same function, different k)
microbenchmark(PIKKVF(x, ks[1]), PIKKVF(x, ks[2]), PIKKVF(x, ks[3]), PIKKVF(x, ks[4]),
               PIKKVF(x, ks[5]), PIKKVF(x, ks[6]), PIKKVF(x, ks[7]), PIKKVF(x, ks[8]),
               times = 100)

microbenchmark(PIKKVF(x, ks[9]), PIKKVF(x, ks[10]), PIKKVF(x, ks[11]), PIKKVF(x, ks[12]),
               PIKKVF(x, ks[13]), PIKKVF(x, ks[14]), PIKKVF(x, ks[15]), PIKKVF(x, ks[16]),
               times = 100)


# Microbenchmark loop (different thresholds, same k)
microbenchmark(PIKKVF(x, ks[19], 1), PIKKVF(x, ks[19], 50), PIKKVF(x, ks[19], 100), 
               PIKKVF(x, ks[19], 200), PIKKVF(x, ks[19], 300), PIKKVF(x, ks[19], 400), 
               PIKKVF(x, ks[19], 500), PIKKVF(x, ks[19], 600), PIKKVF(x, ks[19], 700), 
               PIKKVF(x, ks[19], 800), PIKKVF(x, ks[19], 900), PIKKVF(x, ks[19], 1000),
               PIKKVF(x, ks[19], 2000), PIKKVF(x, ks[19], 3000), PIKKVF(x, ks[19], 4000), 
               PIKKVF(x, ks[19], 5000), PIKKVF(x, ks[19], 1e10),
               times = 1000)

# Microbenchmark loop (different scales)
microbenchmark(PIKKVF_scale(x, ks[19], , 1), PIKKVF_scale(x, ks[19], , 2), 
               PIKKVF_scale(x, ks[19], , 3), PIKKVF_scale(x, ks[19], , 4), 
               PIKKVF_scale(x, ks[19], , 5), PIKKVF_scale(x, ks[19], , 6), 
               PIKKVF_scale(x, ks[19], , 7), PIKKVF_scale(x, ks[19], , 8), 
               PIKKVF_scale(x, ks[19], , 9), PIKKVF_scale(x, ks[19], , 10), 
               PIKKVF_scale(x, ks[19], , 11), PIKKVF_scale(x, ks[19], , 12), 
               times = 1000)

# Microbenchmark loop (different scales + thresholds
microbenchmark(PIKKVF_scale(x, ks[19], 100, 5), PIKKVF_scale(x, ks[19], 100, 6), 
               PIKKVF_scale(x, ks[19], 100, 7), PIKKVF_scale(x, ks[19], 100, 4), 
               PIKKVF_scale(x, ks[19], 500, 5), PIKKVF_scale(x, ks[19], 500, 6), 
               PIKKVF_scale(x, ks[19], 500, 7), PIKKVF_scale(x, ks[19], 500, 4), 
               PIKKVF_scale(x, ks[19], 1000, 5), PIKKVF_scale(x, ks[19], 1000, 6), 
               PIKKVF_scale(x, ks[19], 1000, 7), PIKKVF_scale(x, ks[19], 1000, 4), 
               times = 1000)



# Microbenchmark: PIKKVF vs RSample function
x <- 1:1e4
microbenchmark(PIKKVF(x, ks[19]), RSample(x, ks[19]), 
               PIKKVF(x, ks[18]), RSample(x, ks[18]), 
               PIKKVF(x, ks[17]), RSample(x, ks[17]), 
               PIKKVF(x, ks[16]), RSample(x, ks[16]), 
               PIKKVF(x, ks[15]), RSample(x, ks[15]), 
               PIKKVF(x, ks[14]), RSample(x, ks[14]), 
               times = 100)


## Plots
# Testing Data: x size 1e3
x <- 1:1e3

# Up to 1/10 of the total size
ks <- c(seq(1, length(X) / 10, 1))

# Focus on cases up to 1/100 of the total size
ks  <- c(seq(1, length(x) / 100, 1)) 

# Plot comparison vs RSample
plotComparisonPIKKAll_A(x, ks, "NEW_PIKK", 100)
plotComparison(x, ks, "NEW_PIKK", 1000)
plotComparisonAll(x, ks, "NEW_PIKK", 1000)
microbenchmark(PIKK(x, ks[length(ks)]), RSample(x, ks[length(ks)]), 
               PIKKVF(x, ks[length(ks)]), times = 10000)

# Testing Data: x size 1e4
x <- 1:1e4

# Up to 1/10 of the total size
ks  <- c(seq(1, 10, 1), seq(20, 100, 10), seq(200, length(x) / 10, 100)) 

# Focus on cases up to 1/100 of the total size
ks  <- c(seq(1, 10, 1), seq(20, length(x) / 100, 10))

# Plot & microbenchmark comparisons
plotComparisonPIKKAll_A(x, ks, "NEW_PIKK", 100)
plotComparison(x, ks, "NEW_PIKK", 10000)
plotComparisonAll(x, ks, "NEW_PIKK", 1000)
microbenchmark(PIKK(x, ks[length(ks)]), RSample(x, ks[length(ks)]), 
               PIKKVF(x, ks[length(ks)]), times = 10000)


## Testing Data: x size 1e5
x <- 1:1e5

# Up to 1/10 of the total size
ks  <- c(seq(1, 10, 1), seq(20, 100, 10), seq(200, length(x) / 10, 100)) 

# Focus on cases up to 1/100 of the total size
ks2  <- c(seq(1, 10, 1), seq(20, 100, 10), seq(200, length(x) / 100, 100)) 

# Plot & microbenchmark comparisons
plotComparisonPIKKAll_A(x, ks, "NEW_PIKK", 100)
plotComparison(x, ks, "NEW_PIKK", 1000)
plotComparisonAll(x, ks, "NEW_PIKK", 100)
microbenchmark(PIKK(x, ks[length(ks)]), RSample(x, ks[length(ks)]), 
               PIKKVF(x, ks[length(ks)]), times = 10000)


## Testing Data: x size 1e6
x <- 1:1e6

# Up to 1/10 of the total size
ks <- c(seq(1, 10, 1), seq(20, 100, 10), seq(200, length(x) / 10, 100))

# Focus on cases up to 1/100 of the total size
ks2  <- c(seq(1, 10, 1), seq(20, 100, 10), seq(200, length(x) / 100, 100)) 

# Plot & microbenchmark comparisons
plotComparisonPIKKAll_A(x, ks, "NEW_PIKK", 100)
plotComparison(x, ks, "NEW_PIKK", 1000)
microbenchmark(PIKK(x, ks[length(ks)]), RSample(x, ks[length(ks)]), 
               PIKKVF(x, ks[length(ks)]), times = 10000)


## Testing Data: x size 5e6
x <- 1:5e6

# Up to 1/10 of the total size
ks  <- c(seq(1, 10, 1), seq(20, 100, 10), seq(200, length(x)/10, 100)) 

# Focus on cases up to 1/100 of the total size
ks2  <- c(seq(1, 10, 1), seq(20, 100, 10), seq(200, length(x)/100, 100)) 

# Plot & microbenchmark comparisons
plotComparisonPIKKAll_A(x, ks, "NEW_PIKK", 100)
plotComparison(x, ks, "NEW_PIKK", threshold = 10000)
microbenchmark(PIKK(x, ks[length(ks)]), RSample(x, ks[length(ks)]), 
               PIKKVF(x, ks[length(ks)]), times = 10000)