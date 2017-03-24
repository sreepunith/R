# This is an implementation of using ROC
# Dataset: Iris
################################################################################
# LOADING LIBRARY
################################################################################
library(ROCR) # ROC
library(nnet) # Neural nets with softmax
library(caret) # Create stratified partitions for k-folds
library(rrcov) # Salmon dataset
library(ggplot2)
################################################################################
# LOADING DATA
################################################################################
data(salmon)
dt_raw <- salmon
dt <- dt_raw #backup raw data
summary(dt)

################################################################################
# DATA PREPROCESSING
################################################################################
# Change Species values to 1, 2, 3
# dt$Origin <- as.character(dt$Origin)
# dt$Origin[dt$Origin == "Alaskan"] <- A
# dt$Origin[dt$Origin == "Canadian"] <- -1
# dt$Origin <- as.factor(dt$Origin)

# Standardize
dt[, -ncol(dt)] <- scale(dt[, -ncol(dt)], center = T, scale = T)

summary(dt)
################################################################################
# DATA SPLITING
################################################################################
set.seed(123) # Set seed for experiment reproduction

N <- nrow(dt)
smpl <- sample(N)
percentage <- 0.8

# Shuffle data
dt <- dt[smpl, ]
  
# Create training and testing sets
training_size <- floor(N*percentage)
training <- dt[1:training_size, ]  
testing <- dt[(training_size+1):N, ]

# Summary
summary(training)
summary(testing)

################################################################################
# MODELING
################################################################################
set.seed(123)

# train control
fitControl <- trainControl(method="repeatedcv", number = 5, repeats = 10,
                           classProbs=T, savePredictions = T)

# Model
nn_fit <- train(Origin ~ ., 
                data = training,
                method = "mlp",
                trControl = fitControl)

filtered_output <- nn_fit$pred[nn_fit$pred$size == 3,]

filtered_output$pred <- as.character(filtered_output$pred)
filtered_output$obs <- as.character(filtered_output$obs)

filtered_output$pred[filtered_output$pred == "Alaskan"] <- 1
filtered_output$pred[filtered_output$pred == "Canadian"] <- 0
filtered_output$obs[filtered_output$obs == "Alaskan"] <- 1
filtered_output$obs[filtered_output$obs == "Canadian"] <- 0

filtered_output$pred <- as.factor(filtered_output$pred)
filtered_output$obs <- as.factor(filtered_output$obs)

roc_dt <- list()
k <- 1
for (resample in unique(filtered_output$Resample)) {
  roc_dt$predictions[[k]] <- apply(filtered_output[filtered_output$Resample == resample, 3:4], 1, max)
  roc_dt$labels[[k]] <- filtered_output$obs[filtered_output$Resample == resample]
  k <- k + 1
}

pred <- prediction(roc_dt$predictions, roc_dt$labels)
perf <- performance(pred, "tpr", "fpr")
plot(perf, avg = "vertical", spread.estimate = "stderror", lwd = 1, lty = 3)

