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
x <- seq(-100,100, by = 5)

# Define a quadratic function ax^2 + bx + c
a <- 1
b <- 2
c <- 3

# Generate y values 
y <- function(x) {
  return (a*(x^2) + b*x + c)
}
df <- data.frame("x" = x, "y" = y(x))

# Plot the function
ggplot(data = df, mapping = aes(x = x, y = y)) +
  geom_line(linetype="dashed", colour = "red") +
  geom_point(shape=21, size=2, fill="blue", stroke = .1)

################################################################
# GAUSS NEWTON
################################################################
# Hypothesis function h(x)
h <- function(x, theta) {
  return (theta[1]*x^2 + theta[2]*x + theta[3])
}

# Residual function
r <- function(x, theta) {
  return (matrix(h(x, theta) - y(x), ncol = 1))
}

# Jacobian matrix
create_jacobian_matrix <- function(x) {
  v <- c()
  for (i in 1:length(x)) {
    grad_r <- c(x[i]^2, x[i], 1)
    v <- append(v, grad_r)
  }
  j <- matrix(v, ncol = 3, byrow = TRUE)
  return (j)
}

# Gauss Newton method
gauss_newton <- function(x, theta) {
  j <- create_jacobian_matrix(x)
  theta <- theta - solve(t(j)%*%j)%*%t(j)%*%r(x, theta)
  return (signif(theta, digits = 2))
}

################################################################
# EXPERIMENT
################################################################
# Iterations
iters <- 2

# Coefficients
theta_initial <- c(27, 1, 5)

# Outputs container
theta_vals <- data.frame("theta_1" = numeric(), 
                         "theta_2" = numeric(), 
                         "theta_2" = numeric())

# Experiment carrier
exprmt <- function(x, theta_initial, iters) {
  theta <- theta_initial
  theta_vals[1, ] <- theta
  for (i in 2:iters) {
    theta <- gauss_newton(x, theta)
    print(theta)
    theta_vals[i, ] <- theta
  }
  return (theta_vals)
}
theta_vals <- exprmt(x, theta_initial, iters)
theta_vals

# Plot approximation
draw.curve<-function(indx){
  # need to use as.numeric and as.vector to ensure
  theta <- as.numeric(as.vector(theta_vals[indx,]))
  y_pred <- h(x, theta)
  p1 <- ggplot() +
    geom_point(data = df, mapping = aes(x = x, y = y), shape = 21, size = 2,
               fill = "blue", stroke = .1) +
    geom_line(data = data.frame(x, y_pred), mapping = aes(x = x, y = y_pred), 
              linetype = "dashed", colour = "red") +
    coord_cartesian(xlim = c(-100, 100), ylim = c(0, 10000)) +
    annotate("text", x=-Inf, y=Inf, hjust=-.2, vjust=2, 
             label = paste("Iteration ", indx ,"( eta = [", paste(theta, collapse = ","), "])"))
  
  print(p1)  
}

# function to iterate over the full span of x-values
trace.animate <- function() {
  lapply(seq(1, iters,1), function(i) {
    draw.curve(i)
  })
}

# save all iterations into one GIF
saveGIF(trace.animate(), interval = 1.5, movie.name="gauss_newton_approximation.gif")
