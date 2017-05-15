################################################################################
# Load library
################################################################################
library(MASS) #mvrnorm
library(ggplot2) #plot

set.seed(123)
################################################################################
# Simulate data
################################################################################
# Draw 10 means for each class from a bivariate Gaussian distribution with 
# mu_0 = (0, 1) and mu_1 = (1, 0), and sigma_0 = sigma_1 = I (2x2)
sigma <- diag(2) #identity matrix 2x2
mu_sample_0 <- mvrnorm(n = 10, mu = c(0, 1), Sigma = sigma) #class 0
mu_sample_1 <- mvrnorm(n = 10, mu = c(1, 0), Sigma = sigma) #class 1 

# For each class, we draw 100 observations by (1) randomly selecting 1 in 10 above 
# means, and (2) generate a bivariate Gaussian distribution with the selected mean 
# and the covariance matrix I/5 (2x2).
generate_obs <- function(class_means, class_name, size, sigma) {
  sample_idx <- sample(1:10, size = size, replace = T) #randomly select the index
  sample_means <- class_means[sample_idx, ] #get the corresponding means by index
  obs <- t(apply(sample_means, 1, function(x) mvrnorm(n = 1, mu = x, Sigma = sigma)))
  obs <- cbind(obs, c(class_name)) #add class name to df
  obs <- as.data.frame(obs)
  names(obs) <- c("x1", "x2", "class")
  return (obs)
}

data_sample_0 <- generate_obs(mu_sample_0, class_name = 0, size = 500, #class 0
                              sigma = sigma/5)
data_sample_1 <- generate_obs(mu_sample_1, class_name = 1, size = 500, #class 1
                              sigma = sigma/5)
dt <- rbind(data_sample_0, data_sample_1) 
dt$class <- as.factor(as.character(dt$class))

# Plot
ggplot(data = dt, mapping = aes(x = x1, y = x2, colour = class, shape = class)) +
  geom_point(size = 1.0) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

################################################################################
# Construct the Bayes decision boundary
################################################################################
# Calculate P(x_i|y = 1) - P(x_i|y = 0)
range_of_x <- c(min(dt[, 1:2]), max(dt[, 1:2]))
points <- seq(range_of_x[1], range_of_x[2], 0.05)
grid <- expand.grid(points, points) # define a grid that covers the plot
err_vect <- vector() # vector that accumulates P(x_i|y = 1) - P(x_i|y = 0)

grid <- as.matrix(grid) 
for (i in 1:nrow(grid)) {
  x <- grid[i, ]
  x_err_0 <- 0
  x_err_1 <- 0
  for (j in 1:10) {
    m_0 <- mu_sample_0[j, ]
    x_err_0 <- x_err_0 + exp(-0.5 * t(x - m_0) %*% solve(sigma/5) %*% (x - m_0))
  }
  for (j in 1:10) {
    m_1 <- mu_sample_1[j, ]
    x_err_1 <- x_err_1 + exp(-0.5 * t(x - m_1) %*% solve(sigma/5) %*% (x - m_1))
  }
  x_err <- x_err_0 - x_err_1 
  err_vect <- c(err_vect, x_err)
}

grid <- as.data.frame(grid) #convert to df to store results
grid$prob <- err_vect # P(x_i|y = 1) - P(x_i|y = 0)
pred <- ifelse(grid$prob >= 0, 1, 0) # predict by following bayesian decision rule
grid$pred <- pred # add predicted class to df

ggplot() +
  geom_point(data = dt, mapping = aes(x = x1, y = x2, colour = class, shape = class), size = 1.2) +
  geom_contour(data = grid, mapping = aes(x = Var1, y = Var2, z = pred), size = 0.6) +
  geom_contour(binwidth = 0.0001)

################################################################################
# Compute the Bayes error rate
################################################################################
