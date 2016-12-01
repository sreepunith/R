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
theta_1 <- matrix(c(1, 1, 1, 1, 1, 0), 
                  ncol = 3)
theta_2 <- matrix(c(1, 1), ncol = 2)

# Define sigmoid function
g <- function(z) {
  return (1/(1 + exp(-z)))
}

error_log <- data.frame("iter" = numeric(), "val" = numeric())
for (j in 1:1000) {
  e <- 0;
  for (i in 1:length(x[1,])) {
    a_2 <- g(theta_1%*%x[i,])
    a_3 <- g(theta_2%*%a_2)
    
    # Define delta_3
    delta_3 <- a_3 - y[i,]
    
    # delta_2
    delta_2 <- (t(theta_2)%*%delta_3)*(a_2*(1-a_2))
    
    # Mean square error 
    e <- e + (delta_3)^2
    
    print(as.character(signif(delta_3)*100), 2)
    
    theta_1 <- theta_1 - delta_2%*%x[i,]
    theta_2 <- theta_2 - t(a_2%*%t(delta_3))
  }
  e <- sqrt(e/4)
  error_log[j, ] <-c(j, e)
}
error_log
# i <- 1


