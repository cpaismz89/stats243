# Loading libraries
library(microbenchmark)
library(tidyr)
library(pryr)
library(ggplot2)

## Plots A)
# Testing Data: x size 1e3
x <- 1:1e3

# Up to 1/10 of the total size
ks <- c(seq(1, length(x) / 10, 1))

# Focus on cases up to 1/100 of the total size
ks2  <- c(seq(1, length(x) / 100, 1)) 

# Plot & microbenchmark comparisons
plotComparisonFYKD_All_A(x, ks2, 100)
plotComparisonFYKD_All_AB(x, ks2, 100)
plotComparisonFYKD_B(x, ks2, 100)
microbenchmark(FYKD(x, ks2[length(ks2)]), FYKDV1(x, ks2[length(ks2)]), 
               FYKDV2(x, ks2[length(ks2)]), FYKDV3(x, ks2[length(ks2)]),
               times = 100)

# Testing Data: x size 1e4
x <- 1:1e4

# Up to 1/10 of the total size
ks  <- c(seq(1, 10, 1), seq(20, 100, 10), seq(200, length(x) / 10, 100)) 

# Focus on cases up to 1/100 of the total size
ks2  <- c(seq(1, 10, 1), seq(20, length(x) / 100, 10))

# Plot & microbenchmark comparisons
plotComparisonFYKD_All_A(x, ks2, 100)
plotComparisonFYKD_All_AB(x, ks2, 100)
plotComparisonFYKD_B(x, ks2, 1000)
microbenchmark(FYKD(x, ks2[length(ks2)]), FYKDV1(x, ks2[length(ks2)]), 
               FYKDV2(x, ks2[length(ks2)]), FYKDV3(x, ks2[length(ks2)]),
               times = 100)


## Testing Data: x size 1e5
x <- 1:1e5

# Up to 1/10 of the total size
ks  <- c(seq(1, 10, 1), seq(20, 100, 10), seq(200, length(x) / 10, 100)) 

# Focus on cases up to 1/100 of the total size
ks2  <- c(seq(1, 10, 1), seq(20, 100, 10), seq(200, length(x) / 100, 100)) 

# Plot & microbenchmark comparisons
plotComparisonFYKD_All_A(x, ks2, 100)
plotComparisonFYKD_All_AB(x, ks2, 100)
plotComparisonFYKD_B(x, ks2, 100)
microbenchmark(FYKD(x, ks2[length(ks2)]), FYKDV1(x, ks2[length(ks2)]), 
               FYKDV2(x, ks2[length(ks2)]), FYKDV3(x, ks2[length(ks2)]),
               times = 100)


## Testing Data: x size 1e6
x <- 1:1e6

# Up to 1/10 of the total size
ks <- c(seq(1, 10, 1), seq(20, 100, 10), seq(200, length(x) / 10, 100))

# Focus on cases up to 1/100 of the total size
ks2  <- c(seq(1, 10, 1), seq(20, 100, 10), seq(200, length(x) / 100, 100)) 

# Plot & microbenchmark comparisons
plotComparisonFYKD_All_A(x, ks2, 100)
plotComparisonFYKD_All_AB(x, ks2, 100)
plotComparisonFYKD_B(x, ks2, 100)
microbenchmark(FYKD(x, ks2[length(ks2)]), FYKDV1(x, ks2[length(ks2)]), 
               FYKDV2(x, ks2[length(ks2)]), FYKDV3(x, ks2[length(ks2)]),
               times = 100)


## Testing Data: x size 5e6
x <- 1:5e6

# Up to 1/10 of the total size
ks  <- c(seq(1, 10, 1), seq(20, 100, 10), seq(200, length(x)/10, 100)) 

# Focus on cases up to 1/100 of the total size
ks2  <- c(seq(1, 10, 1), seq(20, 100, 10), seq(200, length(x)/100, 100)) 

# Plot & microbenchmark comparisons
plotComparisonFYKD_All_A(x, ks2, 100)
plotComparisonFYKD_All_AB(x, ks2, 100)
plotComparisonFYKD_B(x, ks2, 100)
microbenchmark(FYKD(x, ks2[length(ks2)]), FYKDV1(x, ks2[length(ks2)]), 
               FYKDV2(x, ks2[length(ks2)]), FYKDV3(x, ks2[length(ks2)]),
               times = 100)