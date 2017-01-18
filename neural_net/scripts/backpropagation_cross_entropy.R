################################################################################
# LOAD LIBRARY
################################################################################
library(mlbench) #PimaIndiansDiabetes

################################################################################
# LOAD DATA
################################################################################
data(PimaIndiansDiabetes)
dt <- PimaIndiansDiabetes
dt$diabetes <- as.character(dt$diabetes)
dt$diabetes[which(dt$diabetes =="neg")] <- 0
dt$diabetes[which(dt$diabetes =="pos")] <- 1
dt$diabetes <- as.factor(dt$diabetes)
summary(dt)

################################################################################
# BACKPROPAGATION
################################################################################
# Initial theta values
theta_1 <- matrix(rep(1, 64), nrow = 8)
theta_2 <- matrix(rep(1, 8), nrow = 8)
theta_2

# Define sigmoid function
g <- function(z) {
  return (1/(1 + exp(-z)))
}

x <- dt[1:8]
y <- dt[9]

i <- 1
row <- as.matrix(x[i,])
d <- as.numeric(y[i, ])

a_2 <- g(theta_1%*%t(row))
a_3 <- g(t(theta_2)%*%a_2)

theta_2 <- theta_2 - -as.numeric((d - a_3))*a_2

theta_1 <- theta_1 - t(delta_3*theta_2)%*%((1-a_2)%*%row)

error_log <- data.frame("iter" = numeric(), "val" = numeric())
for (j in 1:1) {
  e <- 0;
  for (i in 1:length(x[1,])) {
    a_2 <- g(as.matrix(x[i,])%*%theta_1)
    a_3 <- g(a_2%*%theta_2)
    
    # Define delta_3
    delta_3 <- (a_3 - y[i,])
    
    # delta_2
    delta_2 <- delta_3*a_2*(1-a_2)
    
    # Mean square error 
    # e <- e + (delta_3)^2
    
    # print(as.character(signif(delta_3)*100), 2)
    
    theta_1 <- theta_1 - delta_2%*%(as.matrix(x[i,]))
    theta_2 <- theta_2 - t(a_2%*%t(delta_3))
  }
  e <- sqrt(e/4)
  error_log[j, ] <-c(j, e)
}
error_log
