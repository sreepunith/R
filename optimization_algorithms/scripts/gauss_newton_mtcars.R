################################################################
# LIBRARY
################################################################
library(ggplot2)
library(animation) #gif

################################################################
# SAMPLE DATA
################################################################
# Plot
ggplot(data = cars, mapping = aes(x = speed, y = dist)) +
  geom_point(shape=21, size=2, fill="blue", stroke = .1)

################################################################
# GAUSS NEWTON
################################################################
# Hypothesis function h(x)
h <- function(x, theta) {
  return (theta[1]*x^2 + theta[2]*x + theta[3])
}

# Residual function
r <- function(x, theta, y) {
  return (matrix(h(x, theta) - y, ncol = 1))
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
gauss_newton <- function(x, theta, y) {
  j <- create_jacobian_matrix(x)
  theta <- theta - solve(t(j)%*%j)%*%t(j)%*%r(x, theta, y)
  return (signif(theta, digits = 2))
}
################################################################
# EXPERIMENT
################################################################
# Iterations
iters <- 4

# Coefficients
theta_initial <- c(1, 0, 0)

# Outputs container
theta_vals <- data.frame("theta_1" = numeric(), 
                         "theta_2" = numeric(), 
                         "theta_3" = numeric())

# Experiment carrier
exprmt <- function(x, theta_initial, iters, y) {
  theta <- theta_initial
  theta_vals[1, ] <- theta
  for (i in 2:iters) {
    theta <- gauss_newton(x, theta, y)
    print(theta)
    theta_vals[i, ] <- theta
  }
  return (theta_vals)
}
theta_vals <- exprmt(cars$speed, theta_initial, iters, cars$dist)
theta_vals

# Plot approximation
draw.curve<-function(indx){
  # need to use as.numeric and as.vector to ensure
  theta <- as.numeric(as.vector(theta_vals[indx,]))
  y_pred <- h(cars$speed, theta)
  p1 <- ggplot() +
    geom_point(data = cars, mapping = aes(x = speed, y = dist), shape = 21, size = 2,
               fill = "blue", stroke = .1) +
    geom_line(data = data.frame(speed = cars$speed, y_pred), 
              mapping = aes(x = speed, y = y_pred), colour = "red") +
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 125)) +
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
saveGIF(trace.animate(), loop = TRUE, ani.width = 600, interval = .5, movie.name="gauss_newton_mtcars.gif")
