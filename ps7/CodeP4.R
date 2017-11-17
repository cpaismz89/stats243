# Dimensions
n = 4
p = 3
m = 2

# Data
A = matrix(as.integer(runif(n = m * p, 0, 100)), m, p)
b = as.integer(runif(n = m, 0, 100))
X = matrix(as.integer(runif(n * p, 0, 100)), n, p)
Y = as.integer(runif(n = n, 0, 100))

# Naive implementation
C = t(X) %*% X
Cinv = solve(C)
d = t(X) %*% Y

Beta_Naive1 = Cinv %*% d
Beta_Naive2_1 = Cinv %*% t(A) 
Beta_Naive2_2 = solve(A %*% Cinv %*% t(A)) 
Beta_Naive2_3 = (-A %*% Cinv %*% d + b)

FinalBeta_Naive = Beta_Naive1 + Beta_Naive2_1 %*% Beta_Naive2_2 %*% Beta_Naive2_3


# Cholesky implementation
# C^{-1} %*% d
U1 = chol(crossprod(X))
R1 = backsolve(U1, backsolve(U1, d, transpose = TRUE))

# C^{-1} t(A)
R2 = backsolve(U1, backsolve(U1, t(A), transpose = TRUE))

# (AC^{-1}t(A)) %*% (-AC^{-1}d + b)
U2 = chol(A %*% R2)
R3 = backsolve(U2, backsolve(U2, -A %*% R1 + b, transpose = TRUE))

Final_Beta_chol = R1 + R2 %*% R3




