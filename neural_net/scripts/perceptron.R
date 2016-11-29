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
library(animation) #gif
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
  return (w)
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
iters <- 10

# Learning iteration starts here
w <- learning_func(x, y, w, beta, iters)

# Boundary line
boundary_func <-function(x, w) {
  return (as.numeric((-w[2]*x - w[1]) / w[3]))
}
boundary_line <- data.frame("x" = x[,2], "y" = boundary_func(x[,2], w))

# Plot
ggplot() +
  geom_point(data = dt, mapping = aes(x = Freshwater, y = Marine, colour = Origin,
                                      shape = Origin), size = 2.5) +
  geom_line(data = boundary_line, mapping = aes(x = x, y = y), 
            colour="black", linetype="dashed") +
  coord_cartesian(xlim = c(50, 200), ylim = c(200, 550)) +
  labs(title = "The growth-ring diameter of salmon in
       freshwater and saltwater for Canadian and Alaskan fish")

################################################################################
# TRAINING REPORT
################################################################################
# Weight vector with initial weights
w <- vector(length = ncol(x))

# test() surveys on how many items are correctly classified
# x: input matrix
# y: output vector
# w: weight vector
test <- function(x, y, w) {
  correctly_classified <- 0
  for (idx in 1:nrow(x)) {
    # Net input
    u <- x[idx, ]%*%w
    
    # Predicted output
    p <- ifelse(u >= 0, 1, 0)
    
    # Error
    e <- y[idx] - p
    
    # Cumulate correctly classified item
    correctly_classified <- ifelse(e == 0, 
                                   correctly_classified + 1, 
                                   correctly_classified)
  }
  return (as.numeric(correctly_classified))
}
# report() cumulates all possible iterations
# x: input
# y: output
# w: inital weight
# epochs: vector of the number of iterations
report <- function(x, y, beta, epochs){
  correctly_classified <- vector(length = 0)
  for (idx in epochs) {
    w <- vector(length = ncol(x))
    w <- learning_func(x, y, w, beta, idx)
    correctly_classified <- append(correctly_classified, test(x, y, w))
  }
  return (data.frame(epoch = epochs, corrected = correctly_classified))
}


# Vector of iterations
epochs <- seq(0, 100, 10)

report_detail <- report(x, y, beta, epochs)
report_detail

ggplot(data = report_detail, mapping = aes(x = epoch, y = corrected)) +
  geom_point(shape = 15, colour = "red", size = 3) +
  geom_path(colour = "red") +
  coord_cartesian(xlim = c(0, 100), ylim = c(60, 100)) +
  theme_bw() +
  scale_x_continuous(breaks = round(seq(0, 100, by = 10), 10)) +
  geom_hline(yintercept=seq(60, 100, 10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Epoch", y = "The number of correctly classified items", 
       title = "The number of correctly classified items by Epoch")
################################################################################
# ANIMATION OF HOW BOUNDARY LINE EVOLVES IN 1 SINGLE EPOCH
################################################################################
# Cumulate changes of weight vector in a single epoch
# x : input matrix
# y : ouput vector
cumulative_learning_func <- function(x, y) {
  #cumulate weight
  weight_df <- data.frame("w1" = numeric(), #bias
                          "w2" = numeric(), 
                          "w3" = numeric(),
                          "x1" = numeric(),
                          "x2" = numeric(),
                          "x3" = numeric(),
                          "y" = numeric()) 
  beta <- 0.1 #set beta as 0.1
  w <- vector(length = ncol(x)) #weight vector
  
  for (idx in 1:nrow(x)) {
    # Net input
    u <- x[idx, ]%*%w
    
    # Predicted output
    p <- ifelse(u >= 0, 1, 0)
    
    # Error
    e <- y[idx] - p
    
    # Adjust weights
    w <- w + beta*e*x[idx,]
    cum_vec <- append(w, c(x[idx,], y[idx]))
    weight_df[idx, ] <- cum_vec
  }
  return (weight_df)
}

weight_df <- cumulative_learning_func(x, y)
weight_df$origin <- dt$Origin

draw.curve<-function(idx){
  boundary_line <- data.frame(
    "x" = x[, 2], "y" = boundary_func(x[, 2], as.numeric(weight_df[idx, 1:3])))
  print(boundary_line)
  # Plot
  p <- ggplot() +
    geom_point(data = dt, 
               mapping = aes(x = Freshwater, y = Marine, 
                             colour = Origin, shape = Origin), 
               size = 2.5) +
    geom_point(data = weight_df[idx, 5:8], 
               aes(x = x2, y = x3, shape = origin, colour = origin), 
               size = 10, fill = "white") +
    geom_line(data = boundary_line, mapping = aes(x = x, y = y), 
              colour="black", linetype="dashed") +
    coord_cartesian(xlim = c(50, 200), ylim = c(200, 550)) +
    labs(title = "The growth-ring diameter of salmon in
         freshwater and saltwater for Canadian and Alaskan fish") +
    geom_label(data = dt, size = 10, x = 150, y = 250, 
               label = paste("# of items: ", idx))
  print(p)
}

#function to iterate over the full span of w-values
trace.animate <- function() {
  lapply(seq(1, nrow(weight_df), 1), function(i) {
    draw.curve(i)
  })
}

#save all iterations into one GIF
saveGIF(trace.animate(), loop = TRUE, ani.width = 600, interval = .9, 
        movie.name="linear_neuron.gif")
