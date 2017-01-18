################################################################################
# LOAD LIBRARY
################################################################################
library(mlbench) #PimaIndiansDiabetes

################################################################################
# UTILITIES 
################################################################################
# Define sigmoid function
sigmoid <- function(z) {
  return (1/(1 + exp(-z)))
}

# Define softmax function
softmax <- function(z) {
  shift_z <- z - max(z)
  exp_score <- exp(shift_z)
  return (exp_score / sum(exp_score))
}

range01 <- function(z){
  return ((z - min(z)) / (max(z) - min(z)))
}
################################################################################
# LOAD DATA
################################################################################
data(iris)
dt <- iris
dt$Species <- as.character(dt$Species)
dt$Species[which(dt$Species =="setosa")] <- 1
dt$Species[which(dt$Species =="versicolor")] <- 2
dt$Species[which(dt$Species =="virginica")] <- 3
dt$Species <- as.factor(dt$Species)
summary(dt)

################################################################################
# BACKPROPAGATION
################################################################################
set.seed(123)
samp <- sample(1:nrow(dt), nrow(dt))

dt <- dt[samp,]

learning_rate <- 0.01

# Initial weight matrices 
w1 <- matrix(rnorm(25, mean = 1, sd = 1), nrow = 5, ncol = 5)
w2 <- matrix(rnorm(15, mean = 1, sd = 1), nrow = 5, ncol = 3)

# Indices
N <- nrow(dt)
x_idx <- 1:4
y_idx <- 5

# Input
x <- unname(data.matrix(dt[x_idx]))
x <- cbind(1, x) # Add bias
x <- t(x) # Return to column vector form

# Output
classes <- unique(dt[,y_idx])
for(level in classes){ # Generate dummy columns
  dt[paste("class", level, sep = "")] <- ifelse(dt[, y_idx] == level, 1, 0)
}
y <- dt[, -(1:y_idx)] # Get the last dummy columns
y <- unname(data.matrix(y)) # Convert to matrix
y <- t(y) # Return to column vector form

# In the end we have
x

y
w1
w2

x <- apply(x, 2, range01)
x

#############################################################
# x <- range01(x)
# 
# # Forward
# ## Hidden layer
# s1 <- t(w1) %*% x
# s1
# a1 <- sigmoid(s1) # Sigmoid
# a1
# 
# ## Output layer
# s2 <- t(w2) %*% a1
# s2
# a2 <- apply(s2, 2, softmax)
# a2
# 
# # Backward
# ## w2
# dw2 <- (-1) * a1 %*% t(y - a2) # Gradient
# dw2
# w2 <- w2 - learning_rate * dw2 #  Update
# 
# ## w1
# dw1 <- (w2 %*% (y - a2) * (a1*(1-a1))) %*% t(x) # Gradient
# dw1
# w1 <- w1 - learning_rate * dw1 # Update

#############################################################
# error_log <- data.frame("iter" = numeric(), "val" = numeric())
for (j in 1:8000) {
  # Forward
  ## Hidden layer
  s1 <- t(w1) %*% x
  s1
  a1 <- sigmoid(s1) # Sigmoid
  a1
  
  ## Output layer
  s2 <- t(w2) %*% a1
  s2
  a2 <- apply(s2, 2, softmax)
  a2
  
  # Backward
  ## w2
  dw2 <- (-1) * a1 %*% t(y - a2) # Gradient
  dw2
  w2 <- w2 - learning_rate * dw2 # Update
  
  ## w1
  dw1 <- (w2 %*% (y - a2) * (a1*(1-a1))) %*% t(x) # Gradient
  dw1
  w1 <- w1 - learning_rate * dw1 # Update
  
}
w1
w2

########################################################
# Forward
## Hidden layer
s1 <- t(w1) %*% x
s1
a1 <- sigmoid(s1) # Sigmoid
a1

## Output layer
s2 <- t(w2) %*% a1
s2
a2 <- apply(s2, 2, softmax)
a2
