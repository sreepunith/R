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
##############################################
# RANDOM FOREST
##############################################
set.seed(123)

# train control
fitControl <- trainControl(method="repeatedcv", number = 10, repeats = 10,
                           classProbs=T, savePredictions = T)

# Model tuning
grid <-  expand.grid(mtry = seq(1, 7, 1))

# Model
rf_fit <- train(Origin ~ ., 
                data = training,
                method = "rf",
                tuneGrid = grid,
                trControl = fitControl,
                preProc = c("center", "scale"),
                metric = "Kappa")

plot(rf_fit)
##############################################
# NEURAL NET
##############################################
set.seed(123)

# train control
fitControl <- trainControl(method="repeatedcv", number = 10, repeats = 10,
                           classProbs=T, savePredictions = T)

# Model tuning
grid <-  expand.grid(size = c(3:5))

# Model
nn_fit <- train(Origin ~ ., 
                data = training,
                method = "mlp",
                tuneGrid = grid,
                trControl = fitControl,
                preProc = c("center", "scale"),
                metric = "Kappa")

plot(nn_fit)
################################################################################
# ROC
################################################################################
library(ngoctranUtils)
roc_dt_1 <- extract(fit = rf_fit, field = "mtry", field_idx = 1)
roc_dt_2 <- extract(fit = nn_fit, field = "size", field_idx = 3)

pred_1 <- prediction(roc_dt_1$predictions, roc_dt_1$labels, label.ordering = c("Canadian", "Alaskan"))
perf_1 <- performance(pred_1, "tpr", "fpr")
pred_2 <- prediction(roc_dt_2$predictions, roc_dt_2$labels, label.ordering = c("Canadian", "Alaskan"))
perf_2 <- performance(pred_2, "tpr", "fpr")
plot(perf_1, avg='vertical', spread.estimate = "stderror", lwd = 2, lty = 3,
     legacy.axes = TRUE, col = 'red')
abline(a=0, b= 1)
plot(perf_2, avg='vertical', spread.estimate = "stderror", lwd = 2, lty = 3,
     legacy.axes = TRUE, add = T, col = 'blue')
legend(0.6, 0.2, c('RF', 'NN'), col = c('red', 'blue'), lwd = 3, lty = 3)

auc <- roc(testing$Origin[], as.numeric(pred))

# Extract single data by field and field index from the caret training
# extract <- function(fit = NULL, field = NULL, field_idx = NULL) {
#   filtered_output <- fit$pred[fit$pred[field] == field_idx,]
#   
#   roc_dt <- list()
#   k <- 1
#   for (resample in unique(filtered_output$Resample)) {
#     roc_dt$predictions[[k]] <- apply(filtered_output[filtered_output$Resample == resample, 3:4], 1, max)
#     roc_dt$labels[[k]] <- filtered_output$obs[filtered_output$Resample == resample]
#     k <- k + 1
#   }
#   
#   return (roc_dt)
# }

# plot_tuning <- function(fit = NULL, field = NULL) {
#   perf <- list()
#   k <- 1
#   for (fi in 1:7) {
#     print(paste("The year is", fi))
#     print("abc")
#     roc_dt <- extract(fit = fit, field = field, field_idx = fi)
#     pred <- prediction(roc_dt$predictions, roc_dt$labels)
#     perf[[k]] <- performance(pred, "tpr", "fpr")
#     if (k == 1) {
#       plot(perf[[k]], avg='vertical', spread.estimate = "stderror", col = k, lwd = 1, lty = 3,
#            legacy.axes = TRUE)
#     }
#     plot(perf[[k]], avg='vertical', spread.estimate = "stderror", col = k, lwd = 1, lty = 3,
#          legacy.axes = TRUE, add = T)
#     # print(str(perf))
#     k <- k + 1
#   }
#   legend(0.9, 0.5, c(1:7), col = c(1:7))
#   # return (perf)
# }
# 
# perf <- plot_tuning(fit = rf_fit, field = "mtry")
# plot(perf[[1]], avg='vertical', spread.estimate = "stderror", lwd = 2, lty = 3,
#      legacy.axes = TRUE, add = T)
# plot(perf[[2]], avg='vertical', spread.estimate = "stderror", lwd = 2, lty = 3,
#      legacy.axes = TRUE, add = T)
# 
# 
# roc_dt <- extract(fit = rf_fit, field = "mtry", field_idx = 2)
# 
# 

