#PS6 Eigendecomposition and condition value
# Loading libraries
library(matrixcalc)

# Dimension
n = 100

# Random matrix Z
Z = matrix(runif(n * n, 0, 100), n, n)
Z = matrix(runif(n * n), n, n)

# A = Z^{T}Z
A = crossprod(Z)

# Compute Gamma using eigen function
Gamma = eigen(A)$vectors

# Function: Tester
Tester <- function(evalues, n, Gamma){
  Lambda <- matrix(0, ncol = n, nrow = n)
  diag(Lambda) <- evalues
  NewA = Gamma %*% Lambda %*% t(Gamma)
  cat("Eigenvalues by engen():", eigen(NewA)$values, "\n")
  cat("Actual eigenvalues:    ", sort(evalues, decreasing = TRUE), "\n")
  Condition = max(evalues) / min(evalues)
  cat("Condition number:", Condition, "\n")
  Error = sum((sort(evalues, decreasing = TRUE) - eigen(NewA)$values)^2)
  cat("Square error:", Error, "\n")
  #cat("Positive semi-definite:", is.positive.semi.definite(NewA,tol = 1e-3))
  return(Error)
}

# Square error and Condition number vector
SE = integer(17)
CN = integer(17)

##Sequences analysis
# First Test: all eigenvalues equal to 1
EigL1 <- rep(1, n)
SE[1] = Tester(EigL1, n, Gamma)
CN[1] = max(EigL1) / min(EigL1)
  
# Second Test: sequence up to n by 1
EigL2 <- c(1:n)
SE[2] = Tester(EigL2, n, Gamma)
CN[2] = max(EigL2) / min(EigL2)

# Third test: 1 to 100 total n elements
EigL3 <- seq(1, 100, length = n)
SE[3] = Tester(EigL3, n, Gamma)
CN[3] = max(EigL3) / min(EigL3)

# Fourth test: 1 to 1000 
EigL4 <- seq(1, 1000, length = n)
SE[4] = Tester(EigL4, n, Gamma)
CN[4] = max(EigL4) / min(EigL4)

# Fifth test: 1 to 10000
EigL5 <- seq(1, 10000, length = n)
SE[5] = Tester(EigL5, n, Gamma)
CN[5] = max(EigL5) / min(EigL5)

# Sixth test: 1 to 1e5
EigL6 <- seq(1, 1e5, length = n)
SE[6] = Tester(EigL6, n, Gamma)
CN[6] = max(EigL6) / min(EigL6)

# Seventh test: 1 to 1e6
EigL7 <- seq(1, 1e6, length = n)
SE[7] = Tester(EigL7, n, Gamma)
CN[7] = max(EigL7) / min(EigL7)

# Eight test: 1 to 1e16
EigL8 <- seq(1, 1e16, length = n)
SE[8] = Tester(EigL8, n, Gamma)
CN[8] = max(EigL8) / min(EigL8)

# Ninth test: 0.001 to 1e16
EigL9 <- seq(1e-3, 1e16, length = n)
SE[9] = Tester(EigL9, n, Gamma)
CN[9] = max(EigL9) / min(EigL9)

# Tenth test: 1e-6 to 1e16
EigL10 <- seq(1e-6, 1e16, length = n)
SE[10] = Tester(EigL10, n, Gamma)
CN[10] = max(EigL10) / min(EigL10)

# Eleventh test: 1e-12 to 1e16
EigL11 <- seq(1e-12, 1e16, length = n)
SE[11] = Tester(EigL11, n, Gamma)
CN[11] = max(EigL11) / min(EigL11)

# Twelth test: 1e-16 to 1e16
EigL12 <- seq(1e-16, 1e16, length = n)
SE[12] = Tester(EigL12, n, Gamma)
CN[12] = max(EigL12) / min(EigL12)

# Thirteenth test: 1e-16 to 1e18
EigL13 <- seq(1e-16, 1e18, length = n)
SE[13] = Tester(EigL13, n, Gamma)
CN[13] = max(EigL13) / min(EigL13)

# Fourteenth test: 1e-16 to 1e24
EigL14 <- seq(1e-16, 1e24, length = n)
SE[14] = Tester(EigL14, n, Gamma)
CN[14] = max(EigL14) / min(EigL14)

# Fifteenth test: 1e-16 to 1e32
EigL15 <- seq(1e-16, 1e32, length = n)
SE[15] = Tester(EigL15, n, Gamma)
CN[15] = max(EigL15) / min(EigL15)

# Sixteenth test: 1e-32 to 1e32
EigL16 <- seq(1e-32, 1e32, length = n)
SE[16] = Tester(EigL16, n, Gamma)
CN[16] = max(EigL16) / min(EigL16)

# Seventeenth test: 1e-64 to 1e64
EigL17 <- seq(1e-64, 1e64, length = n)
SE[17] = Tester(EigL17, n, Gamma)
CN[17] = max(EigL17) / min(EigL17)

plot(CN[10:14],SE[10:14])



## No sequences
# First test: 0 to 1
EigL1 <- runif(n, 0, 1)
Tester(EigL1, n, Gamma)

# Second test: 0 to 10
EigL2 <- runif(n, 0, 10)
Tester(EigL2, n, Gamma)

# Third test: 0 to 100
EigL3 <- runif(n, 0, 100)
Tester(EigL3, n, Gamma)

# Fourth test: 04 to 1e4
EigL4 <- runif(n, 0, 1e4)
Tester(EigL4, n, Gamma)

# Fifth test: 0 to 1e5
EigL5 <- runif(n, 0, 1e5)
Tester(EigL5, n, Gamma)

# Sixth test: 0 to 1e6
EigL6 <- runif(n, 0, 1e6)
Tester(EigL6, n, Gamma)

# Seventh test: 0 to 1e6
EigL7 <- runif(n, 0, 1e8)
Tester(EigL7, n, Gamma)

# Eight test: 0 to 1e6
EigL8 <- runif(n, 0, 1e10)
Tester(EigL8, n, Gamma)

# Ninth test: 0 to 1e6
EigL9 <- runif(n, 0, 1e12)
Tester(EigL9, n, Gamma)

# Tenth test: 0 to 1e6
EigL10 <- runif(n, 0, 1e16)
Tester(EigL10, n, Gamma)

# Eleventh test: 0 to 1e24
EigL11 <- runif(n, 0, 1e24)
Tester(EigL11, n, Gamma)

# Tenth test: 0 to 1e32
EigL12 <- runif(n, 0, 1e32)
Tester(EigL12, n, Gamma)

# Thirteenth test: 0 to 1e64
EigL13 <- runif(n, 0, 1e64)
Tester(EigL13, n, Gamma)

# Fourteenth test: 0 to 1e128
EigL14 <- runif(n, 0, 1e128)
Tester(EigL14, n, Gamma)

# Fifteenth test: 0 to 1e256
EigL15 <- runif(n, 0, 1e256)
Tester(EigL15, n, Gamma)


## Special test
# Test 1
EigL1 <- seq(0.5,1,length = n)
Tester(EigL1, n, Gamma)
