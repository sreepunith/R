################################################################################
# Load library
################################################################################
library(MASS) #mvrnorm
library(ggplot2) #plot

set.seed(123456)
################################################################################
# Simulate data
################################################################################
sigma_1 <- matrix(c(1/2, 0, 0, 2), nrow = 2, ncol = 2, byrow = 2)
sigma_2 <- matrix(c(2, 0, 0, 2), nrow = 2, ncol = 2, byrow = 2)

mu_sample_0 <- mvrnorm(n = 10, mu = c(3, 6), Sigma = sigma_1) #class 0
mu_sample_1 <- mvrnorm(n = 10, mu = c(3, -2), Sigma = sigma_2) #class 1 

dt <- rbind(mu_sample_0, mu_sample_1) 
dt <- as.data.frame(dt)
names(dt) <- c("x", "y")
class <- rep(c(0, 1), each = 10)
dt <- cbind(dt, class)
dt$class <- as.factor(dt$class)

# Point plot
ggplot() +
  geom_point(dt, mapping = aes(x = x, y = y, colour = class))