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
m <- 10 # iterations
eta <- c(0.01, 0.02, 0.005, 0.007) # learning rate
x_init <- -4.5 # initial value

# Gradient descent w.r.t. quadractic function ax^2+bx+c
gradient <- function(x) {
  return (-120 - 308*x + 147*x^2 + 560*x^3 + 350*x^4 + 84*x^5 + 7*x^6)
}
grad_desc <- function(x_init, eta, m) {
  # data frame to store the result
  result <- data.frame("id" = numeric(), "x" = numeric(), "y" = numeric())
  y_init <- poly_func(x_init)
  # store the initial point as a result
  result[1,] <- c(1, x_init, y_init)
  
  x <- x_init
  for(indx in 2:m) {
    #compute new x
    x <- x - eta * gradient(x)
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
grad_result_3 <- grad_desc(x_init, eta[2], m)
grad_result_5 <- grad_desc(x_init, eta[3], m)
grad_result_7 <- grad_desc(x_init, eta[4], m)

p1 <- ggplot(data = df, mapping = aes(x = x, y = y)) +
  stat_function(data = df, fun=poly_func, colour="red") +
  geom_path(data = data.frame(grad_result_1), aes(x = x, y = y), colour = "blue") +
  geom_point(data = data.frame(grad_result_1), aes(x = x, y = y)) +
  ggtitle(expression(paste(eta, " = 0.01"))) +
  coord_cartesian(xlim = c(-5:1), ylim = c(-100, 100))

p2 <- ggplot(data = df, mapping = aes(x = x, y = y)) +
  stat_function(data = df, fun=poly_func, colour="red", geom = "line") +
  geom_path(data = data.frame(grad_result_3), aes(x = x, y = y), colour = "blue") +
  geom_point(data = data.frame(grad_result_3), aes(x = x, y = y)) +
  ggtitle(expression(paste(eta, " = 0.02"))) +
  coord_cartesian(xlim = c(-5:1), ylim = c(-100, 100))

p3 <- ggplot(data = df, mapping = aes(x = x, y = y)) +
  stat_function(data = df, fun=poly_func, colour="red") +
  geom_path(data = data.frame(grad_result_5), aes(x = x, y = y), colour = "blue") +
  geom_point(data = data.frame(grad_result_5), aes(x = x, y = y)) +
  ggtitle(expression(paste(eta, " = 0.005"))) +
  coord_cartesian(xlim = c(-5:1), ylim = c(-100, 100))

p4 <- ggplot(data = df, mapping = aes(x = x, y = y)) +
  stat_function(data = df, fun=poly_func, colour="red") +
  geom_path(data = data.frame(grad_result_7), aes(x = x, y = y), colour = "blue") +
  geom_point(data = data.frame(grad_result_7), aes(x = x, y = y)) +
  ggtitle(expression(paste(eta, " = 0.007"))) +
  coord_cartesian(xlim = c(-5:1), ylim = c(-100, 100))

# Use extra function for multiple plot
multiplot(p1, p3, p2, p4, cols = 2)
dev.copy(png,filename="figs/gradient_desc_poly_func.png")
dev.off ()
