# This script is an implementation of perceptron learning proposed by Rosenblatt
# in 1950s. In detail, Rosenblatt used Hebbian learning (Donald Hebb, 1949) to 
# make threshold neuron of McCulloch-Pitts learn.
# In detail, read: section 2.5. of Neural Networks for Applied Sciences and 
# Engineering

################################################################################
# LIBRARY
################################################################################
library(rrcov) #salmon data
library(ggplot2) #plot

################################################################################
# DATA
################################################################################
x1 <- runif(30,-1,1)
x2 <- runif(30,-1,1)
x <- cbind(x1,x2)
Y <- ifelse(x2>0.5+x1,+1,-1)
plot(x,pch=ifelse(Y>0,"+","-"), xlim=c(-1,1),ylim=c(-1,1),cex=2)


################################################################################
# LEARNING ALGORITHM
################################################################################
# Learning function of the perceptron
# x : input matrix
# y : ouput vector
# w : weight vector
# beta : learning rate
learning_func <- function(x, y, w, w0, beta) {
  R <- max(apply(x, 1, function(x) {sqrt(sum(x * x))}))
  making_mistake <- TRUE
  while(making_mistake) {
    making_mistake <- FALSE
    for (idx in 1:nrow(x)) {
      # Net input
      u <- sum(x[idx, ]*w) + w0
      
      # Predicted output
      p <- as.numeric(ifelse(u < 0, -1, 1))
      
      # Adjust weights
      if (y[idx] != p) {
        w <- w + beta * y[idx] * x[idx,]
        w0 <- w0 + beta * y[idx] * R^2
        making_mistake <- TRUE
      }
    }
  }
  return (list(w = w, w0 = w0))
}
################################################################################
# MODELING
################################################################################
# Weight vector with initial weights
w <- vector(length = ncol(x))

# Learning rate beta
beta <- 1

# Iteration
iters <- 1000

# Bias unit
w0 <- 0

# Learning iteration starts here
param <- learning_func(x, Y, w, w0, beta)
w <- param$w
w0 <- param$w0  

# Separate line
boundary_func <-function(x, w, w0) {
  return ((-w[1]*x - w0) / w[2])
}
boundary_line <- data.frame(x = x[,1], y = boundary_func(x[,1], w, w0))

# Plot
ggplot() +
  geom_point(data = data.frame(x = x[,1], y = x[,2], cat = as.factor(Y)), 
             mapping = aes(x = x, y = y, colour = cat, shape = cat), 
             size = 2.5) +
  geom_line(data = boundary_line, mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(-1,1), ylim = c(-1, 1))
param
