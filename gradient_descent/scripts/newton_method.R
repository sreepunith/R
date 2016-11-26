################################################################
# LIBRARY
################################################################
library(ggplot2) #graphs
# library(polynom) #generate polynomial functions
source("scripts/multiplot.R") #plot multiple at once

################################################################
# GENERATE DATA
################################################################
# x is a sequence from [-45; 50]
x <- seq(-5, 1, .05)

poly_func <- function(x){
  return (-120*x - 154*x^2 + 49*x^3 + 140*x^4 + 70*x^5 + 14*x^6 + x^7 );
}

# polynomial function
# calculated_poly <- poly.calc(-5:1) #polynom
# calculated_poly
# poly_func <- as.function(calculated_poly)

# Aggreate as a df
y <- sapply(x, poly_func)
df <- data.frame(x, y)
# df <- data.frame(x, y = poly_func)

# Plot the function
ggplot(data = df, mapping = aes(x = x, y = y)) +
  stat_function(fun=poly_func, colour="red") +
  geom_point(shape=21, size=2, fill="blue", stroke = .1)

################################################################
# GRADIENT DESCENT
################################################################
m <- 5 # iterations
eta <- c(0.01, 0.02, 0.005, 0.007) # learning rate
x_init <- -4.16 # initial value

# 1st derivative w.r.t. the poly func
first_order_derivative <- function(x) {
  return (-120 - 308*x + 147*x^2 + 560*x^3 + 350*x^4 + 84*x^5 + 7*x^6)
}
# 2nd derivative w.r.t. the poly func
second_order_derivative <- function(x){
  return (-308 + 294*x + 1680*x^2 + 1400*x^3 + 420*x^4 + 42*x^5)
}
# Gradient descent 
grad_desc <- function(x_init, eta, m) {
  # data frame to store the result
  result <- data.frame("id" = numeric(), "x" = numeric(), "y" = numeric())
  y_init <- poly_func(x_init)
  # store the initial point as a result
  result[1,] <- c(1, x_init, y_init)
  
  x <- x_init
  for(indx in 2:m) {
    #compute new x
    x <- x - eta * first_order_derivative(x)
    #compute new value of y by new x
    y <- poly_func(x)
    # update the result
    result[indx,] <- c(indx, x, y)
  }
  return (result)
}

# Newton's method
newton_opt <- function(x_init){
  # data frame to store the result
  result <- data.frame("id" = numeric(), "x" = numeric(), "y" = numeric())
  y_init <- poly_func(x_init)
  # store the initial point as a result
  result[1,] <- c(1, x_init, y_init)
  
  x <- x_init
  for(indx in 2:m) {
    #compute new x
    x <- x - first_order_derivative(x) / second_order_derivative(x)
    #compute new value of y by new x
    y <- poly_func(x)
    # update the result
    result[indx,] <- c(indx, x, y)
  }
  return (result)
}
################################################################
# EXPERIMENT
################################################################
# In this experiment, we have multiple plots according to eta values
grad_result_1 <- grad_desc(x_init, eta[1], m)
grad_result_2 <- grad_desc(x_init, eta[2], m)
grad_result_3 <- grad_desc(x_init, eta[3], m)
grad_result_4 <- grad_desc(x_init, eta[4], m)

newton_result <- newton_opt(x_init)

p1 <- ggplot(data = df, mapping = aes(x = x, y = y)) +
  stat_function(data = df, fun=poly_func, colour="red", linetype="dashed") +
  geom_path(data = data.frame(grad_result_1), aes(x = x, y = y), colour = "blue") +
  geom_point(data = data.frame(grad_result_1), aes(x = x, y = y)) +
  geom_path(data = data.frame(newton_result), aes(x = x, y = y), colour = "green") +
  geom_point(data = data.frame(newton_result), aes(x = x, y = y)) +
  ggtitle(expression(paste(eta, " = 0.01"))) +
  coord_cartesian(xlim = c(-5:1), ylim = c(-100, 100))
 
p2 <- ggplot(data = df, mapping = aes(x = x, y = y)) +
  stat_function(data = df, fun=poly_func, colour="red", geom = "line", linetype="dashed") +
  geom_path(data = data.frame(grad_result_2), aes(x = x, y = y), colour = "blue") +
  geom_point(data = data.frame(grad_result_2), aes(x = x, y = y)) +
  geom_path(data = data.frame(newton_result), aes(x = x, y = y), colour = "green") +
  geom_point(data = data.frame(newton_result), aes(x = x, y = y)) +
  ggtitle(expression(paste(eta, " = 0.02"))) +
  coord_cartesian(xlim = c(-5:1), ylim = c(-100, 100))

p3 <- ggplot(data = df, mapping = aes(x = x, y = y)) +
  stat_function(data = df, fun=poly_func, colour="red", linetype="dashed") +
  geom_path(data = data.frame(grad_result_3), aes(x = x, y = y), colour = "blue") +
  geom_point(data = data.frame(grad_result_3), aes(x = x, y = y)) +
  geom_path(data = data.frame(newton_result), aes(x = x, y = y), colour = "green") +
  geom_point(data = data.frame(newton_result), aes(x = x, y = y)) +
  ggtitle(expression(paste(eta, " = 0.005"))) +
  coord_cartesian(xlim = c(-5:1), ylim = c(-100, 100))

p4 <- ggplot(data = df, mapping = aes(x = x, y = y)) +
  stat_function(data = df, fun=poly_func, colour="red", linetype="dashed") +
  geom_path(data = data.frame(grad_result_4), aes(x = x, y = y), colour = "blue") +
  geom_point(data = data.frame(grad_result_4), aes(x = x, y = y)) +
  geom_path(data = data.frame(newton_result), aes(x = x, y = y), colour = "green") +
  geom_point(data = data.frame(newton_result), aes(x = x, y = y)) +
  ggtitle(expression(paste(eta, " = 0.007"))) +
  coord_cartesian(xlim = c(-5:1), ylim = c(-100, 100))

# Use extra function for multiple plot
multiplot(p1, p3, p2, p4, cols = 2)
dev.copy(png,filename="figs/newton_vs_grad_desc_poly_func.png")
dev.off ()
