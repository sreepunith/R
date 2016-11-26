################################################################################
# GENERATE DATA
################################################################################
# Manually generate x1 AND x2 = y dataset 
# x0 is a bias unit and equals to 1
dt <- data.frame(x0 = c(1, 1, 1, 1),
                 x1 = c(1, 1, 0, 0), 
                 x2 = c(1, 0, 1, 0), 
                 y = c(1, 0, 0, 0))

################################################################################
# FEED FORWARD
################################################################################
# Input
x <- as.matrix(dt[,1:3])

# Output
y <- as.matrix(dt[, 4])

# Theta with values [-30, 20, 20]
theta <- matrix(c(-8.873071, 5.728994, 5.788528), nrow = 1, ncol = 3)

# Define sigmoid function
g <- function(z) {
  return (1/(1 + exp(-z)))
}

# Hidden layer
l1 <- theta%*%t(x)

# Predicted values
pred <- g(l1)
pred









