# P5: Sparse matrices and 2SOLS
library(spam)
library(dplyr)
library(pryr)
library(profvis)
library(profmem)


# Matrices
profmem({
  # X matrix
  X = matrix(sample(c(0,1,5,2,6), size = 10 * 1e6, 
                    replace = TRUE, 
                    prob = c(0.9,0.05,0.02,0.02,0.01)),  
             nrow = 1e6, ncol = 10)
  dim(X)
  object_size(X)
  
  # Convert to sparse
  Xsparse = as.spam(X)
  object_size(Xsparse)
  
  
  # Z matrix
  Z = matrix(sample(c(0,1,5,2,6), size = 11 * 1e6, 
                    replace = TRUE, 
                    prob = c(0.9,0.05,0.02,0.02,0.01)),  
             nrow = 1e6, ncol = 11)
  dim(Z)
  object_size(Z)
  
  # Convert to sparse
  Zsparse = as.spam(Z)
  object_size(Zsparse)
  
  
  # y vector
  y = as.integer(runif(1e6, 0, 100))
  dim(y)
  object_size(y)
  
  # Convert to sparse
  ysparse = as.spam(y)
  object_size(ysparse)
})


# 2-stage OLS calculations
# Original approach
profmem({
  X_hat = Z %*% solve(t(Z) %*% Z) %*% t(Z) %*% X
  object_size(X_hat)
  
  Beta_hat = solve(t(X_hat) %*% X_hat) %*% t(X_hat) %*% y
  object_size(Beta_hat)
})

profmem({  
  # Original approach SPARSE
  X_hat = Zsparse %*% solve.spam(t(Zsparse) %*% Zsparse) %*% t.spam(Zsparse) %*% Xsparse
  object_size(X_hat)
  
  Beta_hat = solve(t.spam(X_hat) %*% X_hat) %*% t.spam(X_hat) %*% y
  object_size(Beta_hat)
})


# Original approach improved 1
# Unused objects are removed every step
profmem({
  ## First Stage
  # (Z^{t} Z)^{-1}
  R0 = solve(crossprod(Z))
  object_size(R0)
  
  # Z (Z^{t} Z)^{-1}
  R01 = Z %*% R0
  rm(R0)
  object_size(R01)
  
  # Z^{t} X
  R02 = t(Z) %*% X
  object_size(R02)
  
  # Final value X_hat
  X_hat = R01 %*% R02 
  rm(R01)
  rm(R02)
  object_size(X_hat)
  
  ## Second stage
  # (X^{t} X) ^{-1}
  R1 = solve(crossprod(X_hat))
  
  # X_hat^{t} y
  R12 = crossprod(X_hat, y)
  
  # Beta_hat final value
  Beta_hat = R1 %*% R12
  rm(R1)
  rm(R12)
  object_size(Beta_hat)
  
  # Display the vector
  Beta_hat
})

profmem({
  # Original approach improved 1: SPARSE
  ## First Stage
  # (Z^{t} Z)^{-1}
  R0 = solve(crossprod.spam(Zsparse))
  object_size(R0)
  
  # Z (Z^{t} Z)^{-1}
  R01 = Zsparse %*% R0
  rm(R0)
  object_size(R01)
  
  # Z^{t} X
  R02 = t.spam(Zsparse) %*% Xsparse
  object_size(R02)
  
  # Final value X_hat
  X_hat = R01 %*% R02 
  rm(R01)
  rm(R02)
  object_size(X_hat)
  
  ## Second stage
  # (X^{t} X) ^{-1}
  R1 = solve.spam(crossprod.spam(X_hat))
  
  # X_hat^{t} y
  R12 = crossprod.spam(X_hat, ysparse)
  
  # Beta_hat final value
  Beta_hat = R1 %*% R12
  rm(R1)
  rm(R12)
  object_size(Beta_hat)
  
  # Display the vector
  Beta_hat
})  



profmem({
  # Original improved 2
  ## First stage
  # Cholesky decomposition of Z^{t} Z
  U1 = chol(crossprod(Z))
  object_size(U1)
  
  # Compute (Z^{t}Z)^{-1} Z^{t}X via Cholesky backsolve
  R1 = backsolve(U1, backsolve(U1, t(Z) %*% X, transpose = TRUE))
  rm(U1)
  object_size(R1)
  
  # Compute X_hat
  X_hat = Z %*% R1
  rm(R1)
  object_size(X_hat)
  
  ## Second stage
  # Cholesky decomposition of X_hat^{t} X_hat
  U2 = chol(crossprod(X_hat))
  object_size(U2)
  
  # Compute Beta_hat via Cholesky backsolve
  Beta_hat = backsolve(U2, backsolve(U2, crossprod(X_hat, y), transpose = TRUE))
  rm(U2)
  object_size(Beta_hat) 
  
  # Display the vector
  Beta_hat
})  


profmem({
  # Original improved 2: SPARSE
  ## First stage
  # Cholesky decomposition of Z^{t} Z
  U1 = chol.spam(crossprod.spam(Zsparse), pivot = FALSE)
  object_size(U1)
  
  # Compute (Z^{t}Z)^{-1} Z^{t}X via Cholesky backsolve and forwardsolve (spam package difference)
  R1 = backsolve.spam(U1, forwardsolve.spam(U1, t.spam(Zsparse) %*% Xsparse, transpose = T))
  rm(U1)
  object_size(R1)
  
  # Compute X_hat
  X_hat = as.spam(Zsparse %*% R1)
  rm(R1)
  object_size(X_hat)
  
  ## Second stage
  # Cholesky decomposition of X_hat^{t} X_hat
  U2 = chol.spam(crossprod.spam(X_hat), pivot = FALSE)
  object_size(U2)
  
  # Compute Beta_hat via Cholesky backsolve and forwardsolve (spam package difference)
  Beta_hat = backsolve.spam(U2, forwardsolve.spam(U2, 
                                                  crossprod.spam(X_hat, ysparse), 
                                                  transpose = T)
  )
  rm(U2)
  object_size(Beta_hat) 
  
  # Display the vector
  Beta_hat
})  
  