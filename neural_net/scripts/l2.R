# This code is the implementation of using softmax and cross-entropy to perform
# multi-class classification task on the Iris flower dataset
# + Structure: 5 input layers (4 inputs + 1 bias), 5 hidden layers, 3 outputs
# + Activation function: the output layer uses softmax function to output posterior 
# probabilities, while the hidden layer uses sigmoid function
# + Cost function: The cost function is cross-entropy
# + Regularaisation: L2 regularisation
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
# BACKPROPAGATION ALGORITHM IMPLEMENTATION
################################################################################
set.seed(65432)

nn_training <- function(x_idx, #column indexes of input vectors 
                                y_idx, #column indexes of output vectors
                                data = NULL,
                                training_percent = 0.8, #portion of data for training
                                learning_rate = 1e-3, #learning rate
                                regularization_rate = NULL, #a vector of regularisation rates
                                iters = 5000 # number of iterations
                                ) {
  N <- nrow(data)
  ##############################
  # Data spliting
  ##############################
  # Shuffle the data
  smpl <- sample(1:N, N) 
  data <- data[smpl,]
  
  # Input and output
  ## Input
  x <- data[, x_idx]
  x <- unname(data.matrix(data[x_idx]))
  x <- cbind(1, x) # Add bias
  x <- t(x) # Return to column vector form
  x <- apply(x, 2, range01) #data normalisation
  
  ## Output
  y <- data[, y_idx]
  classes <- sort(unique(data[,y_idx]))
  for(level in classes){ # Generate dummy columns
    data[paste("class", level, sep = "")] <- ifelse(data[, y_idx] == level, 1, 0)
  }
  y <- data[, -(1:y_idx)] # Get dummy columns
  y <- unname(data.matrix(y)) # Convert to matrix
  y <- t(y) # Return to column vector form
  
  # Split data for testing and training
  training_size <- N*training_percent
  testing_size <- N - training_size
  
  # Training data
  training_x <- x[, 1:training_size]
  training_y <- y[, 1:training_size] #output training 3x120
  y_single <- data[,y_idx][1:training_size] #output training 1x120
  
  # Testing data
  testing_x <- x[, (training_size+1):N] #input testing 5x30
  desire_result <- data[, y_idx][(training_size+1):N] #output testing 1x30
  
  ##############################
  # Initial weight matrices 
  ##############################
  w1 <- matrix(rnorm(25, mean = 1, sd = 1), nrow = 5, ncol = 5)
  w2 <- matrix(rnorm(15, mean = 1, sd = 1), nrow = 5, ncol = 3)
  
  ##############################
  # Store logs for reporting
  ##############################
  training_accuracy <- numeric() #accuracy of training dataset
  testing_accuracy <- numeric() #accuracy of testing dataset 
  loss <- numeric() #loss
  
  for (j in 1:iters) {
    ##############################
    # Shuffle data for every epoch
    ##############################
    smpl_training <- sample(1:training_size, training_size)
    smpl_testing <- sample(1:testing_size, testing_size)
    
    training_x <- training_x[, smpl_training]
    training_y <- training_y[, smpl_training]
    y_single <- y_single[smpl_training]
    
    testing_x <- testing_x[, smpl_testing]
    desire_result <- desire_result[smpl_testing]
    
    ##############################
    # Forward
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
    reg_loss <- 0.5*regularization_rate* (sum(w1*w1) + sum(w2*w2))
    loss <- c(loss, cross_entropy_loss + reg_loss)
    
    ## Accuracy 
    pred1 <- max.col(t(a2))
    training_accuracy <- c(training_accuracy, (mean(as.integer(y_single) == pred1)))
    ##############################
    # Tesing
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
    pred2 <- max.col(t(a2_test))
    testing_accuracy <- c(testing_accuracy, (mean(as.integer(desire_result) == pred2)))
    ##############################
    # BACKWARD
    ##############################
    ## w2
    dw2 <- (-1) * a1 %*% t(training_y - a2) # Gradient
    dw2
    dw2 <- dw2 + regularization_rate*w2 # Add regularisation term
    w2 <- w2 - learning_rate * dw2 #  Update
    
    ## w1
    dw1 <- (w2 %*% (training_y - a2) * (a1*(1-a1))) %*% t(training_x) # Gradient
    dw1
    dw1 <- dw1 + regularization_rate*w1 # Add regularisation term
    w1 <- w1 - learning_rate * dw1 # Update
  }
  
  return (list(training_accuracy = training_accuracy, 
               testing_accuracy = testing_accuracy, 
               loss = loss))
}

################################################################################
# EXPERIMENT & REPORT
################################################################################
# Training
x_idx = 1:4
y_idx = 5
data = dt
training_percent = 0.8
learning_rate = 1e-3
regularization_rate = seq(-10, 10, 1)
iters = 4e4

pred <- nn_training(x_idx = x_idx, y_idx = y_idx, data = data,
                            training_percent = training_percent,
                            learning_rate = learning_rate,
                            regularization_rate = regularization_rate,
                            iters = iters)
# Collect data for report
report <- data.frame(training_accuracy = pred$training_accuracy,
                     testing_accuracy = pred$testing_accuracy,
                     loss = pred$loss,
                     epoch = 1:iters)
report <- report[which((report$epoch %% 500) == 0),] #keep less epoches

# Plot
ggplot(data = report, mapping = aes(x = epoch)) +
  geom_path(aes(y = training_accuracy, colour = "Training accuracy")) +
  geom_path(aes(y = loss, colour = "Training loss")) +
  geom_point(aes(y = loss, colour = "Training loss"), shape = 0) + 
  geom_point(aes(y = training_accuracy, colour = "Training accuracy"), shape = 21) +
  xlab("Epoch") + ylab("") +
  ggtitle("Training accuracy vs Training loss \nin the classification model of Iris data") + 
  theme(legend.title = element_blank(), legend.position="bottom")

ggsave("figs/iris_accuracy_vs_loss.png")

ggplot(data = report, mapping = aes(x = epoch)) +
  geom_path(aes(y = testing_accuracy, colour = "Testing")) +
  geom_path(aes(y = training_accuracy, colour = "Training")) +
  geom_point(aes(y = training_accuracy, colour = "Training"), shape = 0) + 
  geom_point(aes(y = testing_accuracy, colour = "Testing"), shape = 21) +
  xlab("Epoch") + ylab("Accuracy") +
  ggtitle("Testing accuracy vs Training accuracy \n in the classification model of Iris data") + 
  theme(legend.title = element_blank(), legend.position="bottom")

ggsave("figs/iris_testing_vs_training.png")