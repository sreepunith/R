# This experiment aims at demonstrating how we can plot the Bayes decision boundary
# for a two-category two-dimensional data. The data is sampled from two bivariate
# Gaussian distributions with different means and identical covariance matrix.
# 
# To plot the decision boundary, we need to calculate the probability P(x | y = 0) 
# and P(x | y =1) by using the bivariate probability density function. It can be 
# observed that the decision boundary is the contour where two distributions
# intersect each other. All points along the contour have P(x | y = 0) - P(x | y = 1) = 0
################################################################################
# Load library
################################################################################
library(MASS) #mvrnorm
library(ggplot2) #plot

set.seed(123456)
################################################################################
# Simulate data
################################################################################
# Means
mu_0 <- c(3, 6)
mu_1 <- c(3, -2)

# Covariance matrices
sigma_1 <- matrix(c(1/2, 0, 0, 2), nrow = 2, ncol = 2, byrow = 2)
sigma_2 <- matrix(c(2, 0, 0, 2), nrow = 2, ncol = 2, byrow = 2)

# Sample 10 points from two distributions
mu_sample_0 <- mvrnorm(n = 10, mu = mu_0, Sigma = sigma_1) #class 0
mu_sample_1 <- mvrnorm(n = 10, mu = mu_1, Sigma = sigma_2) #class 1 

# Combine sampling data points
dt <- rbind(mu_sample_0, mu_sample_1) 
dt <- as.data.frame(dt)
names(dt) <- c("x1", "x2")
class <- rep(c(0, 1), each = 10)
dt <- cbind(dt, class)
dt$class <- as.factor(dt$class)

# Point plot
ggplot() +
  geom_point(dt, mapping = aes(x = x1, y = x2, colour = class, shape = class), size = 2.5)

################################################################################
# Bayes decision boundary
################################################################################
# Calculate P(x | y = 0) - P(x | y = 1)
range_of_x <- c(min(mu_sample_0, mu_sample_1), max(mu_sample_0, mu_sample_1)) # get
                            # min and max of the data points
points <- seq(range_of_x[1], range_of_x[2], by = 0.001) #generate points 
                            #that belong to the range
grid <- expand.grid(points, points) #create a grid of the corresponding plot 
                                    #and find points where P(x | y = 0) - P(x | y = 1) = 0
grid <- as.matrix(grid) #conver to matrix for math manipulation
mu_0 <- matrix(mu_0, nrow = 2)
mu_1 <- matrix(mu_1, nrow = 2)

prob <- vector()
for (i in 1:nrow(grid)) {
  x <- grid[i, ] #get point x
  x_prob_0 <- exp(-0.5 * t(x - mu_0) %*% solve(sigma_1) %*% (x - mu_0)) #P(x|y = 0)
  x_prob_1 <- exp(-0.5 * t(x - mu_1) %*% solve(sigma_2) %*% (x - mu_1)) #P(x|y = 1)
  prob <- c(prob, x_prob_0 - x_prob_1) ##P(x|y = 0) - #P(x|y = 1)
}

grid <- as.data.frame(grid)
grid$prob <- prob #store prob in dt
grid$pred <- ifelse(prob >= 0, 0, 1) #prediction by minimizing the probability of error 
                                    #(i.e., Bayes decision rule)

# Plot
ggplot() +
  geom_point(dt, mapping = aes(x = x1, y = x2, colour = class, shape = class), size = 2.5) +
  geom_contour(data = grid, mapping = aes(x = Var1, y = Var2, z = pred), size = 0.6)

