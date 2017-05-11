# Load library
library(MASS) #mvrnorm
library(ggplot2) #plot

# Set seed
set.seed(123456)

# Draw 10 means for each class from a bivariate Gaussian distribution with 
# mu_0 = (0, 1) and mu_1 = (1, 0), and sigma_0 = sigma_1 = I (2x2)
sigma <- diag(2) #identity matrix 2x2
mu_sample_0 <- mvrnorm(n = 10, mu = c(0, 1), Sigma = sigma) #class 0
mu_sample_1 <- mvrnorm(n = 10, mu = c(1, 0), Sigma = sigma) #class 1 

# For each class, we draw 100 observations by (1) randomly selecting 1 in 10 above 
# means, and (2) generate a bivariate Gaussian distribution with the selected mean 
# and the covariance matrix I/5 (2x2).
generate_obs <- function(class_means, size, sigma) {
  sample_idx <- sample(1:10, size = size, replace = T) #randomly select the index
  sample_means <- class_means[sample_idx, ] #get the corresponding means by index
  obs <- t(apply(sample_means, 1, function(x) mvrnorm(n = 1, mu = x, Sigma = sigma)))
  colnames(obs) <- c('x1','x2')
  return (obs)
}

obs100_1 <- generate_obs(mu_sample_0, size = 100, sigma = sigma/5)
obs100_2 <- generate_obs(mu_sample_1, size = 100, sigma = sigma/5)
y <- rep(c(0,1), each = 100)
trainMat <- as.data.frame(cbind(y, rbind(obs100_1, obs100_2)))

# Plot

ggplot(data = trainMat, mapping = aes(x = x1, y = x2, colour = as.factor(y), shape = as.factor(y))) +
  geom_point(size = 2.5) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

