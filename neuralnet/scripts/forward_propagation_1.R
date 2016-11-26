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
theta_1 <- matrix(c(0.3861975, 8.5775116, 0.4667447, -6.3141772, 2.010665, -5.724167), 
                     ncol = 3)
theta_2 <- matrix(c(6.370675, -12.93248), ncol = 2)

# Define sigmoid function
g <- function(z) {
  return (1/(1 + exp(-z)))
}

# Hidden layer 1
a1 <- theta_1%*%t(x)

# Hidden layer 1
a2 <- theta_2%*%a1

# Predicted values
pred <- g(a2)
pred









