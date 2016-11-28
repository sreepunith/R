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
# Load data to working env
data(salmon)

# Store dat in temp var
dt <- data.frame(x = c(0.3, -0.6, -0.1, 1, -0.25, -0.30), y = c(0.7, 0.3, -0.1, 1, 0.25, 0.11), cat = as.factor(c(1, 0, 0, 1, 1, 0)))

# Plotting
ggplot(data = dt, mapping = aes(x = x, y = y, colour = cat, 
                                shape = cat)) +
  geom_point(size = 5) +
  coord_cartesian(xlim = c(-1:1), ylim = c(-1:1)) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0))

################################################################################
# LEARNING ALGORITHM
################################################################################
# Learning function of the perceptron
# x : input matrix
# y : ouput vector
# w : weight vector
# beta : learning rate
learning_func <- function(x, y, w, beta, iters) {
  for (idx in 1:iters) {
    for (idx in 1:3) {
      # Net input
      u <- x[idx, ]%*%w
      
      # Predicted output
      p <- ifelse(u >= 0, 1, 0)
      
      # Error
      E <- y[idx] - p
      
      # Adjust weights
      w <- w + as.matrix(as.numeric(beta*x[idx, ]*E))
    }
  }
  return (w)
}
################################################################################
# MODELING
################################################################################
# Input matrix x
x <- as.matrix(dt[, 1:2]) 
x[, 3] <- cbind(x, c(1,1,1,1,1,1))
x

# Actual output
y <- dt$cat
y <- as.character(y)
y <- as.numeric(y)

# Weight vector with initial weights
w <- matrix(data = c(1, 1), ncol = 1, nrow = 2)

# Learning rate beta
beta <- 0.000001

# Iteration
iters <- 10000

# Learning iteration starts here
w <- learning_func(x, y, w, beta, iters)
w

# Separate line
boundary_func <-function(x1, w) {
  return (-(w[1,1]/w[2,1]) * x1)
}
boundary_line <- data.frame(x = x[,1], y = boundary_func(x[,1], w))

new_dt <- data.frame(x = x[,1], y = x[, 2])

# Plot
ggplot() +
  geom_point(data = dt, mapping = aes(x = x, y = y, colour=cat, 
                                      shape = cat), size = 2.5) +
  geom_line(data = boundary_line, mapping = aes(x = x, y = y) , 
            colour = "darkgreen") +
  coord_cartesian(xlim = c(-0.8:1), ylim = c(-0.25:1)) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0))

 

