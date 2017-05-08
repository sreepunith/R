# Implementations of concepts introduced in "Introducing Monte Carlo methods with R"

set.seed(123)
################################################################################
# Among dnorm, pnorm, qnorm, rnorm, the function we should care about is rnorm.
# rnorm can be used to generate random normal distribution with mean and sd.

# The default value of rnorm is mean = 0, and sd = 1
x <- rnorm(25) # produce a N(0, 1) sample of size 25

# test whether or not the sample mean is 0, the p-value should be > 0.05, which 
# means that we fail to reject the null hypothesis that the sample mean is equal
# to 0.
t.test(x, mu = 0) 
################################################################################
# Bootstrap
# Create a bootstrap sample y* from the data y
y <- c(4.313, 4.513, 5.489, 4.265, 3.641, 5.106, 8.006, 5.087) #note that size = 8 is rather small for CLT
ystar <- sample(y, replace = T) #bootstrap resampling
b_mean <- c()
for (i in 1:2500) { #2500 bootstrap
  b_mean[i] <-  mean(sample(y, replace = T))
}
hist(b_mean, prob=TRUE)
################################################################################
# Generate number uniformly using uniform distribution.
# Uniform distribution is a ON-OFF distribution such as this(http://www.statisticshowto.com/wp-content/uploads/2013/09/uniform-distribution.gif)
# where every number has an equal chance.
n <-  10^4
x <-  runif(n) # r-uni-form
hist(x) # histogram plot to see how numbers distribute across the interval (0,1)

################################################################################
# mlbench simulated data
library(mlbench)
p <- mlbench.hypercube()
plot(p)
library("lattice")
cloud(x.3~x.1+x.2, groups=classes, data=as.data.frame(p))

p<-mlbench.shapes()
plot(p)

################################################################################
library(bnlearn)
net <- model2network("[A][B|A]")
yn <- c("yes", "no")
cptA <- matrix(c(0.3, 0.7), ncol=2, dimnames=list(NULL, yn))
cptB <- matrix(c(0.01, 0.99, 0.3, 0.7), ncol=2, dimnames=list("B" = yn, "A" = yn))
net.disc <- custom.fit(net, dist=list(A = cptA, B = cptB))
graphviz.plot(net.disc)


