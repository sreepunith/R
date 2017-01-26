# This code is the implementation of using softmax and cross-entropy to perform
# multi-class classification task on the Iris flower dataset
################################################################################
# LOAD LIBRARY
################################################################################
library(mlbench)
library(ggplot2)
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
softmax_1 <- function(z) {
  exp_score <- exp(z*1)
  return (exp_score / sum(exp_score))
}
# Normalise to (0,1)
range01 <- function(z){
  return ((z - min(z)) / (max(z) - min(z)))
}
################################################################################
# LOAD DATA
################################################################################
# Load data
data(iris)
dt <- iris

dt$Species <- as.character(dt$Species)
dt$Species[which(dt$Species =="setosa")] <- 1
dt$Species[which(dt$Species =="versicolor")] <- 2
dt$Species[which(dt$Species =="virginica")] <- 3
dt$Species <- as.factor(dt$Species)
summary(dt)

################################################################################
# PLOT 
################################################################################
# Box plot of 4 features of the data
dt_reshaped <- melt(iris, id.var = "Species")

ggplot(data = dt_reshaped, 
       mapping = aes(x = variable, y = value, fill = Species)) +
  geom_boxplot() +
  facet_grid(.~variable, scales="free_x") +
  xlab("") + ylab("") + ggtitle("Iris flower dataset description") +
  theme(legend.position="bottom", legend.title=element_blank())

ggsave("figs/iris_features_boxplot.png")
################################################################################
# BACKPROPAGATION
################################################################################
##############################
# INITIALISATION
##############################
set.seed(65432)

# Shuffle the data
rand <- sample(1:nrow(dt), nrow(dt))
dt <- dt[rand,]

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
classes <- sort(unique(dt[,y_idx]))
for(level in classes){ # Generate dummy columns
  dt[paste("class", level, sep = "")] <- ifelse(dt[, y_idx] == level, 1, 0)
}
y <- dt[, -(1:y_idx)] # Get the last dummy columns
y <- unname(data.matrix(y)) # Convert to matrix
y <- t(y) # Return to column vector form

# Misc
learning_rate <- 1e-3 #learning rate
reg_rate <- 1e-12 #regularization rate
epoches <- 1e4 #the number of epoches

##############################
# DATA NORMALISATION
##############################
x <- apply(x, 2, range01)

##############################
# DATA SPLITING
##############################
# Split data for testing and training
training_size <- 120 # testing size
samp <- sample(1:training_size, training_size)

# Training data
training_x <- x[, samp] #input training 5x120
training_y <- y[, samp] #output training 3x120
y_single <- dt[,y_idx][samp] #output training 1x120

# Testing data
testing_x <- x[, -samp] #input testing 5x30
desire_result <- dt$Species[-samp] #output testing 1x30

##############################
# TRAINING
##############################
# Store logs for reporting
accuracy <- numeric() #accuracy 
loss <- numeric() #loss

for (j in 1:epoches) {
  ##############################
  # FORWARD
  ##############################
  ## Hidden layer
  s1 <- t(w1) %*% training_x
  s1
  a1 <- sigmoid(s1) # Sigmoid
  a1
  
  ## Output layer
  s2 <- t(w2) %*% a1
  s2
  a2 <- apply(s2, 2, softmax) #Softmax
  a2
  
  ## Loss
  matching_probs <- sapply(1:ncol(a2), function(i) return (a2[y_single[i], i]))
  log_prob <- -log(matching_probs)
  cross_entropy_loss  <- sum(log_prob)/training_size
  reg_loss <- 0.5*reg_rate* (sum(w1*w1) + sum(w2*w2))
  loss <- c(loss, cross_entropy_loss + reg_loss)
  
  ##############################
  # TESTING
  ##############################
  # Hidden layer
  s1_test <- t(w1) %*% testing_x
  s1_test
  a1_test <- sigmoid(s1_test) # Sigmoid
  a1_test
  
  # Output layer
  s2_test <- t(w2) %*% a1_test
  s2_test
  a2_test <- apply(s2_test, 2, softmax)
  a2_test
  
  # Result
  result <- max.col(t(a2_test))
  accuracy <- c(accuracy, (mean(as.integer(desire_result) == result)))
  
  ##############################
  # BACKWARD
  ##############################
  ## w2
  dw2 <- (-1) * a1 %*% t(training_y - a2) # Gradient
  dw2
  dw2 <- dw2 + reg_rate*w2
  w2 <- w2 - learning_rate * dw2 #  Update
  
  ## w1
  dw1 <- (w2 %*% (training_y - a2) * (a1*(1-a1))) %*% t(training_x) # Gradient
  dw1
  dw1 <- dw1 + reg_rate*w1
  w1 <- w1 - learning_rate * dw1 # Update
}
################################################################################
# REPORT
################################################################################
# Collect data for report
report <- data.frame(accuracy = accuracy, epoch = 1:epoches, loss = loss)
report <- report[which((report$epoch %% 200) == 0),] #keep less epoches

# Plot
ggplot(data = report, mapping = aes(x = epoch)) +
  geom_path(aes(y = accuracy, colour = "Accuracy")) +
  geom_path(aes(y = loss, colour = "Loss")) +
  geom_point(aes(y = loss, colour = "Loss"), shape = 0) + 
  geom_point(aes(y = accuracy, colour = "Accuracy"), shape = 21) +
  xlab("Epoch") + ylab("") +
  theme(legend.title=element_blank())

ggsave("figs/iris_accuracy_vs_loss.png")