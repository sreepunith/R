################################################################
# LIBRARY
################################################################
library(ggplot2)
library(animation)
source("scripts/multiplot.R")

################################################################
# GENERATE DATA
################################################################
# x is a sequence from [-45; 50]
x <- seq(-45, 50, by = 5)

# Define a quadratic function ax^2 + bx + c
a <- 1
b <- 2
c <- 3
quad_func <- function(x, a, b, c){
  return (a*(x^2) + b*x + c);
}

# Generate y values 
y <- sapply(x, quad_func, a, b, c)

# Plot the function
df <- data.frame(x, y)
ggplot(data = df, mapping = aes(x = x, y = y)) +
  geom_line(linetype="dashed", colour = "red") +
  geom_point(shape=21, size=2, fill="blue", stroke = .1)

################################################################
# GRADIENT DESCENT
################################################################
m <- 20 # iterations
eta <- c(0.1, 0.3, 0.5, 0.7) # learning rate
x_init <- 45 # initial value

# Gradient descent w.r.t. quadractic function ax^2+bx+c
grad_desc <- function(x_init, eta, m) {
  # data frame to store the result
  result <- data.frame("id" = numeric(), "x" = numeric(), "y" = numeric())
  y_init <- quad_func(x_init, a, b, c)
  # store the initial point as a result
  result[1,] <- c(1, x_init, y_init)
  
  x <- x_init
  for(indx in 2:m) {
    #gradient
    grad <- 2*a*x+b
    #compute new x
    x <- x - eta * grad
    #compute new value of y by new x
    y <- quad_func(x, a, b, c)
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

draw.curve<-function(indx){
  p1 <- ggplot() +
    geom_line(data = df, mapping = aes(x = x, y = y), linetype="dashed", colour = "red") +
    geom_path(data = data.frame(grad_result_1[1:indx,]), aes(x = x, y = y), colour = "blue") +
    geom_point(data = data.frame(grad_result_1[1:indx,]), aes(x = x, y = y)) +
    geom_text(data = data.frame(grad_result_1[1:indx,]), aes(x = x, y = y, label = id), vjust=-1, colour = "blue") +
    ggtitle(expression(paste("Gradient descent with ", eta, " = 0.1")))
  
  p2 <- ggplot() +
    geom_line(data = df, mapping = aes(x = x, y = y), linetype="dashed", colour = "red") +
    geom_path(data = data.frame(grad_result_2[1:indx,]), aes(x = x, y = y), colour = "blue") +
    geom_point(data = data.frame(grad_result_2[1:indx,]), aes(x = x, y = y)) +
    geom_text(data = data.frame(grad_result_2[1:indx,]), aes(x = x, y = y, label = id), vjust=-1, colour = "blue") +
    ggtitle(expression(paste("Gradient descent with ", eta, " = 0.3")))
  
  p3 <- ggplot() +
    geom_line(data = df, mapping = aes(x = x, y = y), linetype="dashed", colour = "red") +
    geom_path(data = data.frame(grad_result_3[1:indx,]), aes(x = x, y = y), colour = "blue") +
    geom_point(data = data.frame(grad_result_3[1:indx,]), aes(x = x, y = y)) +
    geom_text(data = data.frame(grad_result_3[1:indx,]), aes(x = x, y = y, label = id), vjust=-1, colour = "blue") +
    ggtitle(expression(paste("Gradient descent with ", eta, " = 0.5")))
  
  p4 <- ggplot() +
    geom_line(data = df, mapping = aes(x = x, y = y), linetype="dashed", colour = "red") +
    geom_path(data = data.frame(grad_result_4[1:indx,]), aes(x = x, y = y), colour = "blue") +
    geom_point(data = data.frame(grad_result_4[1:indx,]), aes(x = x, y = y)) +
    geom_text(data = data.frame(grad_result_4[1:indx,]), aes(x = x, y = y, label = id), vjust=-1, colour = "blue") +
    ggtitle(expression(paste("Gradient descent with ", eta, " = 0.7")))
  
  # Use extra function for multiple plot
  multiplot(p1, p3, p2, p4, cols = 2)
}

#function to iterate over the full span of x-values
trace.animate <- function() {
  lapply(seq(1, m, 1), function(i) {
    draw.curve(i)
  })
}

#save all iterations into one GIF
saveGIF(trace.animate(), loop = TRUE, ani.width = 600, interval = .8, movie.name="grad_desc_quad_func.gif")
