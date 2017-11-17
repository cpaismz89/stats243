# P5: Sparse matrices and 2SOLS
library(spam)
library(dplyr)
library(pryr)
library(profvis)


# Matrices
r = 1e6  # (60e6)
m = 55   # (630)
p = 50   # (600)
 
# X matrix
X = matrix(sample(c(0,1,5,2,6), size = p * r, 
                  replace = TRUE, 
                  prob = c(0.9,0.05,0.02,0.02,0.01)),  
           nrow = r, ncol = p)
dim(X)
object_size(X)

# Convert to sparse
Xsparse = as.spam(X)
object_size(Xsparse)


# Z matrix
Z = matrix(sample(c(0,1,5,2,6), size = m * r, 
                  replace = TRUE, 
                  prob = c(0.9,0.05,0.02,0.02,0.01)),  
           nrow = r, ncol = m)
dim(Z)
object_size(Z)

# Convert to sparse
Zsparse = as.spam(Z)
object_size(Zsparse)


# y vector
y = as.integer(runif(r, 0, 100))
dim(y)
object_size(y)

# Convert to sparse
ysparse = as.spam(y)
object_size(ysparse)



# 2-stage OLS calculations
# Original approach
X_hat = Z %*% solve(t(Z) %*% Z) %*% t(Z) %*% X
object_size(X_hat)

Beta_hat = solve(t(X_hat) %*% X_hat) %*% t(X_hat) %*% y
object_size(Beta_hat)

# Original approach SPARSE
X_hat = Zsparse %*% solve.spam(t(Zsparse) %*% Zsparse) %*% t.spam(Zsparse) %*% Xsparse
object_size(X_hat)

Beta_hat = solve(t.spam(X_hat) %*% X_hat) %*% t.spam(X_hat) %*% y
object_size(Beta_hat)



# Auxiliary vector for recording sizes
Sizes1 = integer(7)
MSizes1 = integer(3)
MSizes1[1] = object_size(X)
MSizes1[2] = object_size(Z)
MSizes1[3] = object_size(y)

# Original approach improved 1
# Unused objects are removed every step
## First Stage
# (Z^{t} Z)^{-1}
R0 = solve(crossprod(Z))
Sizes1[1] = object_size(R0)
object_size(R0)

# Z (Z^{t} Z)^{-1}
R01 = Z %*% R0
rm(R0)
object_size(R01)
Sizes1[2] = object_size(R01)

# Z^{t} X
R02 = t(Z) %*% X
object_size(R02)
Sizes1[3] = object_size(R02)

# Final value X_hat
X_hat = R01 %*% R02 
rm(R01)
rm(R02)
object_size(X_hat)
Sizes1[4] = object_size(X_hat)

## Second stage
# (X^{t} X) ^{-1}
R1 = solve(crossprod(X_hat))
object_size(R1)
Sizes1[5] = object_size(R1)

# X_hat^{t} y
R12 = crossprod(X_hat, y)
object_size(R12)
Sizes1[6] = object_size(R12)

# Beta_hat final value
Beta_hat = R1 %*% R12
rm(R1)
rm(R12)
object_size(Beta_hat)
Sizes1[7] = object_size(Beta_hat)

# Display the vector
Beta_hat

# Sizes statistics
cat("Minimum object size (computed): \t", min(Sizes1), "[bytes]")
cat("Maximum object size (computed): \t ", max(Sizes1), "[bytes]")
cat("Total memory used: \t\t\t", sum(Sizes1), "[bytes]")
cat("Total memory used including matrices: \t", sum(Sizes1) + sum(MSizes1), "[bytes]")



# Auxiliary vector for recording sizes
Sizes2 = integer(7)
MSizes2 = integer(3)
MSizes2[1] = object_size(Xsparse)
MSizes2[2] = object_size(Zsparse)
MSizes2[3] = object_size(ysparse)

# Original approach improved 1: SPARSE
## First Stage
# (Z^{t} Z)^{-1}
R0 = solve(crossprod.spam(Zsparse))
Sizes2[1] = object_size(R0)
object_size(R0)

# Z (Z^{t} Z)^{-1}
R01 = Zsparse %*% R0
rm(R0)
Sizes2[2] = object_size(R01)
object_size(R01)

# Z^{t} X
R02 = t.spam(Zsparse) %*% Xsparse
Sizes2[3] = object_size(R02)
object_size(R02)

# Final value X_hat
X_hat = R01 %*% R02 
rm(R01)
rm(R02)
object_size(X_hat)
Sizes2[4] = object_size(X_hat)

## Second stage
# (X^{t} X) ^{-1}
R1 = solve.spam(crossprod.spam(X_hat))
object_size(R1)
Sizes2[5] = object_size(R1)

# X_hat^{t} y
R12 = crossprod.spam(X_hat, ysparse)
object_size(R12)
Sizes2[6] = object_size(R12)

# Beta_hat final value
Beta_hat = R1 %*% R12
rm(R1)
rm(R12)
object_size(Beta_hat)
Sizes2[7] = object_size(Beta_hat)

# Display the vector
Beta_hat

# Sizes statistics
cat("Minimum object size (computed): \t", min(Sizes2), "[bytes]")
cat("Maximum object size (computed): \t ", max(Sizes2), "[bytes]")
cat("Total memory used: \t\t\t", sum(Sizes2), "[bytes]")
cat("Total memory used including matrices: \t", sum(Sizes2) + sum(MSizes2), "[bytes]")



# Auxiliary vector for recording sizes
Sizes22 = integer(7)

# Original approach improved 1.2: SPARSE with different order
## First Stage
# (Z^{t} Z)^{-1}
R0 = solve(crossprod.spam(Zsparse))
Sizes22[1] = object_size(R0)
object_size(R0)

# Z^{t} X
R01 = t.spam(Zsparse) %*% Xsparse
Sizes22[2] = object_size(R01)
object_size(R01)

# (Z^{t} Z)^{-1} Z^{t} X
R02 = R0 %*% R01
rm(R0)
rm(R01)
Sizes22[3] = object_size(R02)
object_size(R02)

# Final value X_hat
X_hat = Z %*% R02 
rm(R02)
object_size(X_hat)
Sizes22[4] = object_size(X_hat)

## Second stage
# (X^{t} X) ^{-1}
R1 = solve.spam(crossprod.spam(X_hat))
object_size(R1)
Sizes22[5] = object_size(R1)

# X_hat^{t} y
R12 = crossprod.spam(X_hat, ysparse)
object_size(R12)
Sizes22[6] = object_size(R12)

# Beta_hat final value
Beta_hat = R1 %*% R12
rm(R1)
rm(R12)
object_size(Beta_hat)
Sizes22[7] = object_size(Beta_hat)

# Display the vector
Beta_hat

# Sizes statistics
cat("Minimum object size (computed): \t", min(Sizes22), "[bytes]")
cat("Maximum object size (computed): \t ", max(Sizes22), "[bytes]")
cat("Total memory used: \t\t\t", sum(Sizes22), "[bytes]")
cat("Total memory used including matrices: \t", sum(Sizes22) + sum(MSizes2), "[bytes]")



# Auxiliary vector for recording sizes
Sizes3 = integer(5)

# Original improved 2
## First stage
# Cholesky decomposition of Z^{t} Z
U1 = chol(crossprod(Z))
object_size(U1)
Sizes3[1] = object_size(U1)

# Compute (Z^{t}Z)^{-1} Z^{t}X via Cholesky backsolve
R1 = backsolve(U1, backsolve(U1, t(Z) %*% X, transpose = TRUE))
rm(U1)
object_size(R1)
Sizes3[2] = object_size(R1)

# Compute X_hat
X_hat = Z %*% R1
rm(R1)
object_size(X_hat)
Sizes3[3] = object_size(X_hat)

## Second stage
# Cholesky decomposition of X_hat^{t} X_hat
U2 = chol(crossprod(X_hat))
object_size(U2)
Sizes3[4] = object_size(U2)

# Compute Beta_hat via Cholesky backsolve
Beta_hat = backsolve(U2, backsolve(U2, crossprod(X_hat, y), transpose = TRUE))
rm(U2)
object_size(Beta_hat) 
Sizes3[5] = object_size(Beta_hat)

# Display the vector
Beta_hat

# Sizes statistics
cat("Minimum object size (computed): \t", min(Sizes3), "[bytes]")
cat("Maximum object size (computed): \t ", max(Sizes3), "[bytes]")
cat("Total memory used: \t\t\t", sum(Sizes3), "[bytes]")
cat("Total memory used including matrices: \t", sum(Sizes3) + sum(MSizes1), "[bytes]")



# Auxiliary vector for recording sizes
Sizes4 = integer(5)

# Original improved 2: SPARSE
## First stage
# Cholesky decomposition of Z^{t} Z
U1 = chol.spam(crossprod.spam(Zsparse), pivot = FALSE)
object_size(U1)
Sizes4[1] = object_size(U1)

# Compute (Z^{t}Z)^{-1} Z^{t}X via Cholesky backsolve and forwardsolve (spam package difference)
R1 = backsolve.spam(U1, forwardsolve.spam(U1, t.spam(Zsparse) %*% Xsparse, transpose = T))
rm(U1)
object_size(R1)
Sizes4[2] = object_size(R1)

# Compute X_hat
X_hat = as.spam(Zsparse %*% R1)
rm(R1)
object_size(X_hat)
Sizes4[3] = object_size(X_hat)

## Second stage
# Cholesky decomposition of X_hat^{t} X_hat
U2 = chol.spam(crossprod.spam(X_hat), pivot = FALSE)
object_size(U2)
Sizes4[4] = object_size(U2)

# Compute Beta_hat via Cholesky backsolve and forwardsolve (spam package difference)
Beta_hat = backsolve.spam(U2, forwardsolve.spam(U2, 
                                             crossprod.spam(X_hat, ysparse), 
                                             transpose = T)
                          )
rm(U2)
object_size(Beta_hat) 
Sizes4[5] = object_size(Beta_hat)

# Display the vector
Beta_hat

# Sizes statistics
cat("Minimum object size (computed): \t", min(Sizes4), "[bytes]")
cat("Maximum object size (computed): \t ", max(Sizes4), "[bytes]")
cat("Total memory used: \t\t\t", sum(Sizes4), "[bytes]")
cat("Total memory used including matrices: \t", sum(Sizes4) + sum(MSizes2), "[bytes]")



# Auxiliary vector for recording sizes
Sizes4 = integer(5)

# Original improved Aproach 2: SPARSE
## PZ matrix calculation
U0 = chol(crossprod(X,Z))
R1 = backsolve(U0, backsolve(U0, t(Z), transpose = TRUE))
Pz = Zsparse %*% solve(crossprod.spam(Zsparse)) %*% t.spam(Zsparse)

U1 = chol(t(X) %*% (Pz %*% X))
Beta_hat = backsolve(U1, backsolve(t(X) %*% (Pz %*% y)))

# Display the vector
Beta_hat

# Sizes statistics
cat("Minimum object size (computed): \t", min(Sizes4), "[bytes]")
cat("Maximum object size (computed): \t ", max(Sizes4), "[bytes]")
cat("Total memory used: \t\t\t", sum(Sizes4), "[bytes]")
cat("Total memory used including matrices: \t", sum(Sizes4) + sum(MSizes2), "[bytes]")



# Final comparison
FComp = data.frame("min" = c(min(Sizes1), min(Sizes2), min(Sizes22), 
                             min(Sizes3), min(Sizes4)),
                   "max" = c(max(Sizes1), max(Sizes2), max(Sizes22),
                             max(Sizes3), max(Sizes4)),
                   "Total" = c(sum(Sizes1), sum(Sizes2), sum(Sizes22),
                               sum(Sizes3), sum(Sizes4)),                    
                   "Total_with_Matrices" = c(sum(Sizes1) + sum(MSizes1), 
                                             sum(Sizes2) + sum(MSizes2),  
                                             sum(Sizes22) + sum(MSizes2),  
                                             sum(Sizes3) + sum(MSizes1),
                                             sum(Sizes4) + sum(MSizes2)),
                   row.names = c("Method 1", "Method 1 SP", "Method 1 SP+OR", 
                                 "Method 2", "Method 2 SP")
                   )

# Display final comparison table
FComp