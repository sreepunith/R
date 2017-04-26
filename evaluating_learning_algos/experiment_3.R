# PURPOSE: This is an experiment of comparing 2 classifiers on a single domain
# DATASET: Iris
# ALGORITHMS: CART, NN
# ERROR ESTIMATION: 5x2-fold
# SIGNIFICANCE STAT TESTS: t-test, Wilcoxon's signed-rank test
################################################################################
# Loading library
################################################################################
library(mlbench)
library(Amelia)
library(mice)
library(caret)
library(pROC)
################################################################################
# Loading data
################################################################################
data(iris)
dt_raw <- iris
dt <- dt_raw

################################################################################
# Preprocessing
################################################################################
str(dt)

missmap(dt)
################################################################################
# Modeling
################################################################################
set.seed(123)

# Split data
inTraining <- createDataPartition(dt$Species, p = .80, list = FALSE)
training <- dt[inTraining,]
testing  <- dt[-inTraining,]

# Train control
fitControl <- trainControl(method="repeatedcv", number = 2, repeats = 5,
                           classProbs=T, savePredictions = T)

# Model of NN
nn_fit <- train(Species ~ ., 
                data = training,
                method = "mlp",
                trControl = fitControl)

# Model of CART
cart_fit <- train(Species ~ .,
                  data = training,
                  method = "rpart",
                  trControl = fitControl)
plot(cart_fit)
plot(nn_fit)

################################################################################
# Testing
################################################################################
# Prediction with NN
pred <- predict(nn_fit, newdata = testing)
error <- mean(pred != testing$Species)
print(paste("Accuracy ", 1 - error))

# Prediction with CART
pred <- predict(cart_fit, newdata = testing)
error <- mean(pred != testing$Species)
print(paste("Accuracy ", 1 - error))
################################################################################
# Extracting results of each folds
################################################################################
# Extract the best size of nn & best cp of cart
best_size <- nn_fit$bestTune$size
best_cp <- cart_fit$bestTune$cp

# Extracting all fold results according to best size and best cp
nn_fold_result <- nn_fit$pred[nn_fit$pred$size == best_size, ]
cart_fold_result <- cart_fit$pred[cart_fit$pred$cp == best_cp, ]

# Function for extracting accuracy by folds of caret
accuracy_by_fold <- function(fit_fold_result) {
  accuracy <- c()
  for (fld in unique(fit_fold_result$Resample)) {
    pred_by_fold <- fit_fold_result$pred[fit_fold_result$Resample == fld]
    obs_by_fold <- fit_fold_result$obs[fit_fold_result$Resample == fld]
    err <- mean(pred_by_fold != obs_by_fold)
    accuracy <- append(accuracy, 1 - err)
  }
  return (accuracy)
}

nn_accuracy_by_fold <- accuracy_by_fold(nn_fold_result)
cart_accuracy_by_fold <- accuracy_by_fold(cart_fold_result)

################################################################################
# Performance measures
################################################################################
rocobj_1 <- roc(obs ~ setosa, nn_fold_result, ret = c("tp", "fp"))
rocobj_2 <- roc(obs ~ setosa, cart_fold_result, ret = c("tp", "fp"))
plot(rocobj_1, print.thres="best", col = "blue", print.auc=TRUE)
plot(rocobj_2, print.thres="best", col = "red", add=TRUE)
legend("bottomright", legend=c("NN", "CART"),
       col=c("blue", "red"), lwd=2)

################################################################################
# Statistical significance test
################################################################################
# Paired t-test
t.test(nn_accuracy_by_fold, cart_accuracy_by_fold, paired = T)

# Wilcoxon's signed-rank test
wilcox.test(nn_accuracy_by_fold, cart_accuracy_by_fold, paired = T, alternative = "two.sided", correct = F)
