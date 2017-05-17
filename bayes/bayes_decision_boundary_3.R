################################################################################
# Load library
################################################################################
library(MASS) #mvrnorm
library(ggplot2) #plot
library(corrplot) #corrplot
library(caret)

set.seed(1)
################################################################################
# Simulate data
################################################################################
# D : number of input variables
# C : number of output classes
# N : number of observations
gen_data <- function(D, C, N){
  # Means and covariance
  n_mu <-  D*10 #number of means needs to be drawed
  sigma <- diag(D) #set covariance matrix as the I matrix
  sigma_scale <- 5 #the scale between the sigma of the class mean dist and the sigma of the obs dist
  
  dt <- data.frame() #store generated data
  cm <- list() #store class means
  for (c in 1:C){
    #randomly draw a mean vector for the class mean
    # mu <- rbinom (D, size = 1, prob = 0.5)
    mu <- runif(D, 0, 2)
    
    #randomly generate class means from a Gaussian distribution with the generated mean vector
    class_means <- mvrnorm(n = n_mu, mu = mu, Sigma = sigma)
    
    #store class_means in the cm list with a flexible name
    flex_name <- paste("class_means", c, sep = "") #create flexible name
    cm[[flex_name]] <- class_means
    
    # After getting class means, randomly pick them N (number of obs) with replacement
    sample_idx <- sample(1:n_mu, size = N, replace = T)
    sample_means <- class_means[sample_idx, ]
    
    # For N means, generate N observations
    obs <- t(apply(sample_means, 1,
                   function(x) mvrnorm(n = 1, mu = x, Sigma = sigma/sigma_scale)))
    
    # Put them in the data frame
    class <- c
    obs <- cbind(obs, class)
    obs <- as.data.frame(obs)
    dt <- rbind(dt, obs)
  }
  dt$class <- as.factor(dt$class)
  return (list(dt = dt, cm = cm))
}
# Define variables
D <- 15 #number of input variables
C <- 2 #number of output classes
N <- 1000 #number of observations

# Generate data
gen <- gen_data(D, C, N)
dt <- gen$dt
cm <- gen$cm

# Plot
ggplot(data = dt, mapping = aes(x = V1, y = V4, colour = class, shape = class)) +
  geom_point(size = 1.5)

# Correlation plots
correlations <- cor(dt[,1:10], dt[,1:10])
corrplot(correlations)

################################################################################
# Measure the Bayes error rate
################################################################################
# Function to compute the predicted prob and predicted class
pred <- function(dt, cm){
  dt <- as.matrix(dt)
  pred_prob <- vector() #store pred prob of the whole process
  pred_class <- vector() #store pred class of the whole process
  
  for (i in 1:nrow(dt)) {
    x <- as.numeric(dt[i, -ncol(dt)]) #get observation
    pred_prob_x <- 0 #for each obs, we have a corresponding predicted probability
    pred_class_x <- 0 #and predicted class
    
    #find probability w.r.t to each class
    prob_class <- vector() #to store prob of each class
    for (c in 1:length(cm)) { 
      x_prob_c <- 0 #P(x_i|y = c)
      class_means <- cm[[c]]
      
      #find class prob
      for (j in 1:nrow(class_means)) { 
        m <- class_means[j, ]
        x_prob_c <- x_prob_c + exp(-0.5 * t(x - m) %*% solve(diag(ncol(dt)-1)/5) %*% (x - m))
      }
      prob_class <- c(prob_class, x_prob_c) #add to prob vector
    }
    
    pred_class_x <- which.max(prob_class) #classify to the class with highest prob
    pred_prob_x <- max(prob_class) #store the predicted prob
    
    pred_prob <- c(pred_prob, pred_prob_x)
    pred_class <- c(pred_class, pred_class_x)
  }
  return (list(pred_prob = pred_prob, pred_class = pred_class))
}

pred <- pred(dt, cm)
dt$prob <- pred$pred_prob
dt$pred_class <- pred$pred_class
bayes_err <- mean(dt$pred_class != dt$class)
paste("Bayes error rate: ", bayes_err)

################################################################################
# Try to model with NN
################################################################################
# dt$class <- factor(dt$class, labels = make.names(unique(dt$class)))
# 
# # Split data
# in_training <- createDataPartition(dt$class, p = .75, list = FALSE)
# training <- dt[in_training,]
# testing  <- dt[-in_training,]
# 
# # Train control
# fit_control <- trainControl(method = "cv", number = 10,
#                             classProbs = T, savePredictions = T)
# 
# # Model of NN
# nn_grid <-  expand.grid(size = c(10, 20, 1))
# nn_fit <- train(class ~ ., 
#                 data = training,
#                 method = "mlp",
#                 tuneGrid = nn_grid,
#                 trControl = fit_control, tuneLength = 10)
# plot(nn_fit)

