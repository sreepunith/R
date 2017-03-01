################################################################################
# LOAD LIBRARY
################################################################################
library(ggplot2) # ggplot
library(MVN) # normality testing
library(dummies) # create dummies columns
library(caret) # neural net

set.seed(456)
################################################################################
# LOAD DATA
################################################################################
# Load data
data(iris)
dt <- iris

################################################################################
# NORMALITY TESTING
################################################################################
# Setosa
setosa_x <- dt[dt$Species == "setosa", 1:4]
mardiaTest(setosa_x, cov = T, qqplot = T)
hzTest(setosa_x, cov = T, qqplot = T)
roystonTest(setosa_x, qqplot = T)

# Versicolor
setosa_x <- dt[dt$Species == "versicolor", 1:4]
mardiaTest(setosa_x, cov = T, qqplot = T)
hzTest(setosa_x, cov = T, qqplot = T)
roystonTest(setosa_x, qqplot = T)

# Virginica
setosa_x <- dt[dt$Species == "virginica", 1:4]
mardiaTest(setosa_x, cov = T, qqplot = T)
hzTest(setosa_x, cov = T, qqplot = T)
roystonTest(setosa_x, qqplot = T)
################################################################################
# SPLITTING THE DATA
################################################################################
# Split data into training and testing
N <- nrow(dt)
training_percent <- 0.8
smpl <- sample(1:N, N) 

dt <- dt[smpl,]
x <- dt[1:4]
y <- dt[5]

training_size <- N*training_percent
testing_size <- N - training_size

# Training x
training_x <- x[1:training_size, ]
testing_x <- x[(training_size+1):N,]

# Training y
training_y <- y[1:training_size,]
testing_y <- y[(training_size+1):N,]

################################################################################
# PCA
################################################################################
prin_comp <- prcomp(training_x, scale. = T, center = T)
biplot(prin_comp, scale = 0) # plot

# Compute variance for each components
pr_var <- (prin_comp$sdev)^2

# Find how many percent of variance explained by each component
pr_var_ex <- (pr_var/sum(pr_var))*100
# [1] 6.198359e+01 2.518308e+01 9.658769e+00 2.448550e+00 4.872580e-01 2.387557e-01 2.151244e-30

plot(cumsum(pr_var_ex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
# It is cleary see that the first 3 components explain nearly 97% variance of the data
# Thus, we would use only 3 first components

# Combine the transformed data with training Y to creat a new training data
training_pca <- data.frame(Species = training_y, prin_comp$x[,1:3]) #1:3 because we only use 3 first components
str(training_pca)

# Using the same PCA to transform test data as well
testing_pca <- predict(prin_comp, newdata = testing_x)
testing_pca <- data.frame(Species = testing_y, testing_pca[,1:3])
str(testing_pca)

# Save both training and testing as csv for further modeling
write.csv(training_pca, "iris_training_pca.csv", row.names = F)
write.csv(testing_pca, "iris_testing_pca.csv", row.names = F)

################################################################################
# Traning and Testing
################################################################################
# Train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# Model tuning
rpart_grid <-  expand.grid(cp = c(1:10)*0.05)

# Model
rpart_model_pca <- train(Species ~ ., 
                       data = training_pca, 
                       method = "rpart",
                       trControl = fitControl,
                       tuneGrid = rpart_grid,
                       metric = "Accuracy")
rpart_model
plot(rpart_model_pca)

# Testing
pred <- predict(rpart_model_pca, newdata = testing_pca)
rpart_pca_error <- mean(pred != testing_pca$Species)
print(paste("Accuracy ", 1 - rpart_pca_error))

################################################################################
# Traning and Testing for non PCA
################################################################################
training <- data.frame(Species = training_y, training_x)
testing <- data.frame(Species = testing_y, testing_x)

# Training
rpart_model <- train(Species ~ ., 
                     data = training, 
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = rpart_grid,
                     metric = "Accuracy")

# Testing
pred <- predict(rpart_model, newdata = testing)
rpart_error <- mean(pred != testing_pca$Species)
print(paste("Accuracy ", 1 - rpart_error))