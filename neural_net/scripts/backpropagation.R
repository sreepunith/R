  ################################################################################
# GENERATE DATA
################################################################################
# Manually generate x1 AND x2 = y dataset 
# x0 is a bias unit and equals to 1
dt <- data.frame(x0 = c(1, 1, 1, 1),
                 x1 = c(1, 1, 0, 0), 
                 x2 = c(1, 0, 1, 0), 
                 y = c(1, 0, 0, 0))
# Input
x <- as.matrix(dt[,1:3])

# Output
y <- as.matrix(dt[, 4])
################################################################################
# BACKPROPAGATION
################################################################################
# Initial theta values
theta_init <- matrix(c(1, 2, 3), ncol = 1)
theta <- theta_init

# Define sigmoid function
g <- function(z) {
  return (1/(1 + exp(-z)))
}

error_log <- data.frame("iter" = numeric(), "val" = numeric())
for (j in 1:1000) {
  e <- 0;
  for (i in 1:length(x[1,])) {
    a_2 <- g(t(theta)%*%x[i,])
    
    # Define delta_2
    delta_2 <- a_2 - y[i,]
    
    # Mean square error 
    e <- e + (delta_2)^2
    
    
    print(as.character(signif(delta_2)*100), 2)
    
    theta <- theta - delta_2*x[i,]
  }
  e <- sqrt(e/4)
  error_log[j, ] <-c(j, e)
}
error_log

