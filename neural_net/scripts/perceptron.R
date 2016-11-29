# This script is an implementation of perceptron learning proposed by Rosenblatt
# in 1950s. In detail, Rosenblatt used Hebbian learning (Donald Hebb, 1949) to 
# make threshold neuron of McCulloch-Pitts learn.
# In detail, read: section 2.5. of Neural Networks for Applied Sciences and 
# Engineering
################################################################################
# CLEAN WORKING ENV
################################################################################
rm(list=ls())

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
  geom_point(size = 2.5) +
  coord_cartesian(xlim = c(50, 200), ylim = c(200, 550))

################################################################################
# LEARNING ALGORITHM
################################################################################
# Learning function of the perceptron
# x : input matrix
# y : ouput vector
# w : weight vector
# beta : learning rate
# iters : number of iterations
learning_func <- function(x, y, w, beta, iters) {
  for (j in 1:iters) {
    for (idx in 1:nrow(x)) {
      # Net input
      u <- x[idx, ]%*%w
      
      # Predicted output
      p <- ifelse(u >= 0, 1, 0)
      
      # Error
      e <- y[idx] - p
      
      # Adjust weights
      w <- w + beta*e*x[idx,]
    }
  }
  return (list(w = w))
}
################################################################################
# MODELING
################################################################################
# Input matrix x
dt <- dt[sample(nrow(dt)),]
x <- as.matrix(dt[, 2:3]) 
x <- matrix(x, ncol = ncol(x), dimnames = NULL)
x <- cbind(x, rep(1, nrow(x)))
x <- x[, c(3, 1, 2)]

# Actual output
y <- dt$Origin
y <- as.character(y)
y[which(y == "Canadian")] <- 1
y[which(y == "Alaskan")] <- 0
y <- as.numeric(y)

# Weight vector with initial weights
w <- vector(length = ncol(x))

# Learning rate beta
beta <- 0.1

# Iteration
iters <- 100

# Learning iteration starts here
results <- learning_func(x, y, w, beta, iters)
w <- results$w
results

# Boundary line
boundary_func <-function(x, w, w0) {
  return ((-w[2]*x - w[1]) / w[3])
}
boundary_line <- data.frame(x = x[,2], y = boundary_func(x[,2], w))

# Plot
ggplot() +
  geom_point(data = dt, mapping = aes(x = Freshwater, y = Marine, colour=Origin,
                                      shape = Origin), size = 2.5) +
  geom_line(data = boundary_line, mapping = aes(x = x, y = y), 
            colour="black", linetype="dashed") +
  coord_cartesian(xlim = c(50, 200), ylim = c(200, 550))

