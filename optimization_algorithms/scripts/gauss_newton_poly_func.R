################################################################
# LIBRARY
################################################################
library(ggplot2)
library(animation)

################################################################
# GENERATE DATA
################################################################
# x is a sequence from [-45; 50]
x <- seq(-5,1, by = 0.1)

# Generate y values 
y <- function(x) {
  return (-120*x - 154*x^2 + 49*x^3 + 140*x^4 + 70*x^5 + 14*x^6 + x^7)
}
df <- data.frame("x" = x, "y" = y(x))

# Plot the function
ggplot(data = df, mapping = aes(x = x, y = y)) +
  stat_function(fun = y, colour="red") +
  geom_point(shape=21, size=2, fill="blue", stroke = .1) +
  coord_cartesian(xlim = c(-5:1), ylim = c(-100, 100))

################################################################
# GAUSS NEWTON
################################################################
# Hypothesis function h(x)
h <- function(x, theta) {
  return (theta[1]*x^7 + theta[2]*x^6 + theta[3]*x^5 + theta[4]*x^4 
          + theta[5]*x^3 + theta[6]*x^2 + theta[7]*x + theta[8])
}

# Residual function
r <- function(x, theta) {
  return (matrix(h(x, theta) - y(x), ncol = 1))
}

# Jacobian matrix
create_jacobian_matrix <- function(x) {
  v <- c()
  for (i in 1:length(x)) {
    grad_r <- c(x[i]^7, x[i]^6, x[i]^5, x[i]^4, x[i]^3, x[i]^2, x[i], 1)
    v <- append(v, grad_r)
  }
  j <- matrix(v, ncol = 8, byrow = TRUE)
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
iters <- 4

# Coefficients
# -120*x - 154*x^2 + 49*x^3 + 140*x^4 + 70*x^5 + 14*x^6 + x^7
theta_initial <- c(1, 1, 1, 1, 1, 1, 1, 1)

# Outputs container
theta_vals <- data.frame("theta_1" = numeric(), 
                         "theta_2" = numeric(), 
                         "theta_3" = numeric(),
                         "theta_4" = numeric(),
                         "theta_5" = numeric(),
                         "theta_6" = numeric(),
                         "theta_7" = numeric(),
                         "theta_8" = numeric())

# Experiment carrier
exprmt <- function(x, theta_initial, iters) {
  theta <- theta_initial
  theta_vals[1, ] <- theta
  for (i in 2:iters) {
    theta <- gauss_newton(x, theta)
    print(theta)
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
  p1 <- ggplot(data = data.frame(x, y_pred), mapping = aes(x = x, y = y_pred)) +
    geom_point(data = df, mapping = aes(x = x, y = y), shape = 21, size = 2,
               fill = "blue", stroke = .1) +
    geom_line(data = data.frame(x, y_pred), mapping = aes(x = x, y = y_pred), 
              linetype = "dashed", colour = "red") +
    coord_cartesian(xlim = c(-5:1), ylim = c(-100, 100)) +
    annotate("text", x=-Inf, y=Inf, hjust=-.2, vjust=2, 
             label = paste("Iteration ", indx ,"( theta = [", paste(theta, collapse = ","), "])"))
  
  print(p1)  
}

# function to iterate over the full span of x-values
trace.animate <- function() {
  lapply(seq(1, iters,1), function(i) {
    draw.curve(i)
  })
}

# save all iterations into one GIF
saveGIF(trace.animate(), loop = TRUE, ani.width = 600, interval = 1, movie.name="gauss_newton_poly_func.gif")
