# This script is an implementation of using the power matrix to find n-th Fibonacci
# number. The complexity of the algorithm is O(nlogn), which is significantly better
# than the naive recursive algorithm (O(2^n)), or the iterative algorithm (O(n))
################################################################################
# This part is an implementation that fully depends on the matrix computation
# Define the matrix A and u0
a <- matrix(data = c(1, 1, 1, 0), nrow = 2, ncol = 2, byrow = T)
u0 <- matrix(data = c(1, 0), nrow = 2, ncol = 1, byrow = T)

# Define the order of the Fibonacci number we want to find
n <- 20 

# Find eigenvectors and eigenvalues of A
e <- eigen(a)
e_vects <- e$vectors
e_vals <- e$values

# Define the eigenvector matrix S and the eigenvalue matrix \Lambda
S <- e_vects
Lambda <- matrix(data = c(e_vals[1], 0, 0, e_vals[2]), nrow = 2, ncol = 2,
                     byrow = T)

# Find the inverse matrix of S
S_inv <- solve(S)

# Find n-th Fibonacci number
un <- S %*% Lambda^n %*% S_inv %*% u0
f_n <- un[2, 1]
f_n

################################################################################
# This part is an implementation that soly depends on the derived formula for
# finding n-th Fibonacci number. There is no matrix computation.

fibonacci_cal <- function(n){
  lambda1 <- (1 + sqrt(5))/2
  lambda2 <- (1 - sqrt(5))/2
  return ((lambda1^n - lambda2^n)/(lambda1 - lambda2))
}
fibonacci_cal(10)
