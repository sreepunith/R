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
dt <- salmon

# Plotting
ggplot(data = dt, mapping = aes(x = Freshwater, y = Marine, colour=Origin, 
                                shape = Origin)) +
  geom_point(size = 2.5)

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
    for (idx in 1:100) {
      # Net input
      u <- x[idx, ]%*%w
      
      # Predicted output
      p <- ifelse(u >= 0, 1, 0)
      
      # Error
      E <- y[idx] - p
      
      # Adjust weights
      w <- w + beta*x[idx, ]*E
      print(w)
    }
  }
  return (w)
}
################################################################################
# MODELING
################################################################################
# Input matrix x
dt["bias"] <- rep(x = 1, times = length(dt[,1]))

x <- as.matrix(dt[,c(2,3, 5)]) 

# Actual output
y <- dt$Origin
y <- as.character(y)
y[which(y == "Canadian")] <- 0
y[which(y == "Alaskan")] <- 1
y <- as.numeric(y)

# Weight vector with initial weights
w <- matrix(data = c(-10, 0, 1), ncol = 1, nrow = 3)

# Learning rate beta
beta <- 0.1

# Iteration
iters <- 100

# Learning iteration starts here
w <- learning_func(x, y, w, beta, iters)
w

# Separate line
boundary_func <-function(x1, w) {
  return (-(w[1,1] * x1 - w[3,1]) / w[2,1])
}
boundary_line <- data.frame(x = x[,1], y = boundary_func(x[,1], w))

# Plot
ggplot() +
  geom_point(data = dt, mapping = aes(x = Freshwater, y = Marine, colour=Origin,
                                      shape = Origin), size = 2.5) +
  geom_line(data = boundary_line, mapping = aes(x = x, y = y))



