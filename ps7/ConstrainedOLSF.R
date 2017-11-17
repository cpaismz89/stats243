library(microbenchmark)

# Constrained OLS function
Constrained_OLS <- function(Model, n, m, p){
  # Random seed for reproducibility
  .Random.seed = 0.0
  
  # Data
  A = matrix(as.integer(runif(n = m * p, 0, 100)), m, p)
  b = as.integer(runif(n = m, 0, 100))
  X = matrix(as.integer(runif(n * p, 0, 100)), n, p)
  Y = as.integer(runif(n = n, 0, 100))
  
  if (Model == "Naive1"){
    # Naive implementation
    C = t(X) %*% X
    Cinv = solve(C)
    d = t(X) %*% Y
    
    Beta_Naive1 = Cinv %*% d
    Beta_Naive2_1 = Cinv %*% t(A) 
    Beta_Naive2_2 = solve(A %*% Cinv %*% t(A)) 
    Beta_Naive2_3 = (-A %*% Cinv %*% d + b)
    
    FinalBeta = Beta_Naive1 + Beta_Naive2_1 %*% Beta_Naive2_2 %*% Beta_Naive2_3
  }
  
  if (Model == "Chol1"){
    # Cholesky implementation
    # C^{-1} %*% d
    U1 = chol(crossprod(X))
    R1 = backsolve(U1, backsolve(U1, d, transpose = TRUE))
    
    # C^{-1} t(A)
    R2 = backsolve(U1, backsolve(U1, t(A), transpose = TRUE))
    
    # (AC^{-1}t(A)) %*% (-AC^{-1}d + b)
    U2 = chol(A %*% R2)
    R3 = backsolve(U2, backsolve(U2, -A %*% R1 + b, transpose = TRUE))
    
    FinalBeta = R1 + R2 %*% R3
    
  }
  
  
  return(FinalBeta)
}


# Generate data function
GenerateData <- function(n, m, p){
  # Data
  A = matrix(as.integer(runif(n = m * p, 0, 100)), m, p)
  b = as.integer(runif(n = m, 0, 100))
  X = matrix(as.integer(runif(n * p, 0, 100)), n, p)
  Y = as.integer(runif(n = n, 0, 100))
  
  return(list(A, b, X, Y))
}


# Naive first implementation
NaiveOLS <- function(A, b, X, Y){
  # Naive implementation
  # C = X^{t} X
  C = t(X) %*% X
  
  # Explicit inverse
  Cinv = solve(C)
  
  # d = X^{t} Y
  d = t(X) %*% Y
    
  # Compute relevant parts of Beta
  Beta_Naive1 = Cinv %*% d
  Beta_Naive2_1 = Cinv %*% t(A) 
  Beta_Naive2_2 = solve(A %*% Cinv %*% t(A)) 
  Beta_Naive2_3 = (-A %*% Cinv %*% d + b)
    
  # Compute final beta expression
  FinalBeta = Beta_Naive1 + Beta_Naive2_1 %*% Beta_Naive2_2 %*% Beta_Naive2_3
  return(FinalBeta)
}
  
# Naive second implementation
Naive2OLS <- function(A, b, X, Y){
  # Compute final beta expression
  FinalBeta = solve(t(X) %*% X) %*% t(X) %*% Y + 
              solve(t(X) %*% X) %*% t(A) %*% solve(A %*% solve(t(X) %*% X) %*% 
              t(A)) %*% (-A %*% solve(t(X) %*% X) %*% t(X) %*% Y + b)
  
  return(FinalBeta)
}



# Cholesky decomposition implementation
CholOLS <- function(A, b, X, Y){
  # Cholesky implementation
  # d = X^{t} Y
  d = crossprod(X, Y)
  
  # C^{-1} %*% d
  U1 = chol(crossprod(X))
  R1 = backsolve(U1, backsolve(U1, d, transpose = TRUE))
    
  # C^{-1} t(A)
  R2 = backsolve(U1, backsolve(U1, t(A), transpose = TRUE))
    
  # (AC^{-1}t(A)) %*% (-AC^{-1}d + b)
  U2 = chol(A %*% R2)
  R3 = backsolve(U2, backsolve(U2, -A %*% R1 + b, transpose = TRUE))
  
  # Compute final beta    
  FinalBeta = R1 + R2 %*% R3
  return(FinalBeta)
}

# Cholesky decomposition second implementation
Chol2OLS <- function(A, b, X, Y){
  # Cholesky second implementation
  # d = X^{t} Y
  d = crossprod(X, Y)
  
  # C
  C = crossprod(X)
  Uc = chol(C)
  Cinv = chol2inv(Uc)
  
  # R1 (right side expression): A^{t} (A C^{-1} A^{t})^{-1} (-AC^{-1}d + b)
  U1 = chol(A %*% Cinv %*% t(A))
  R0 = -A %*% (Cinv %*% d) + b
  R1 = backsolve(U1, backsolve(U1, R0, transpose = TRUE))
  
  # Compute final beta    
  R2 = d + t(A) %*% R1
  FinalBeta = backsolve(Uc, backsolve(Uc, R2))
  return(FinalBeta)
}


# Main Script
# Loading libraries
library(microbenchmark)

# Dimensions
n = 800*2
p = 600*2
m = 400*2

# Generate data
Data = GenerateData(n, m, p)

# Naive approaches
NaiveOLS(Data[[1]], Data[[2]], Data[[3]], Data[[4]])
Naive2OLS(Data[[1]], Data[[2]], Data[[3]], Data[[4]])


# Cholesky approaches
CholOLS(Data[[1]], Data[[2]], Data[[3]], Data[[4]])
Chol2OLS(Data[[1]], Data[[2]], Data[[3]], Data[[4]])

microbenchmark(NaiveOLS(Data[[1]], Data[[2]], Data[[3]], Data[[4]]),
               Naive2OLS(Data[[1]], Data[[2]], Data[[3]], Data[[4]]),
               CholOLS(Data[[1]], Data[[2]], Data[[3]], Data[[4]]),
               times = 100)