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
  
  dt <- data.frame()
  for (c in 1:C){
    #randomly draw a mean vector for the class mean
    # mu <- rbinom (D, size = 1, prob = 0.5)
    mu <- runif(D, 0, 1)
    
    #randomly generate class means from a Gaussian distribution with the generated mean vector
    class_means <- mvrnorm(n = n_mu, mu = mu, Sigma = sigma)
    
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
  return (dt)
}
# Define variables
D <- 15 #number of input variables
C <- 3 #number of output classes
N <- 1000 #number of observations

dt <- gen_data(D, C, N)
dt$class <- as.factor(dt$class)

# Plot
ggplot(data = dt, mapping = aes(x = V1, y = V4, colour = class, shape = class)) +
  geom_point(size = 1.5)

# Correlation plots
correlations <- cor(dt[,1:10], dt[,1:10])
corrplot(correlations)

################################################################################
# Try to model with NN
################################################################################
dt$class <- factor(dt$class, labels = make.names(unique(dt$class)))

# Split data
in_training <- createDataPartition(dt$class, p = .75, list = FALSE)
training <- dt[in_training,]
testing  <- dt[-in_training,]

# Train control
fit_control <- trainControl(method = "cv", number = 10,
                            classProbs = T, savePredictions = T)

# Model of NN
nn_grid <-  expand.grid(size = c(10, 20, 1))
nn_fit <- train(class ~ ., 
                data = training,
                method = "mlp",
                tuneGrid = nn_grid,
                trControl = fit_control, tuneLength = 10)
plot(nn_fit)
