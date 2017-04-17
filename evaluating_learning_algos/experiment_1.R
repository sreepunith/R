# PURPOSE: This is an experiment of comparing 2 classifiers on a single domain
# DATASET: Wisconsin Breast Cancer Database
# ALGORITHMS: CART, NN
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
data(BreastCancer)
dt_raw <- BreastCancer
dt <- dt_raw

################################################################################
# Preprocessing
################################################################################
str(dt)

# Imputation
set.seed(123)

imp <- mice(dt[,-1], m = 1)
dt <- complete(imp, 1) # Extract the 1st version of the imputed data using complete()
missmap(dt)

################################################################################
# Modeling
################################################################################
set.seed(123)

# Split data
inTraining <- createDataPartition(dt$Class, p = .75, list = FALSE)
training <- dt[inTraining,]
testing  <- dt[-inTraining,]

# Train control
fitControl <- trainControl(method="cv", number = 10,
                           classProbs=T, savePredictions = T)

# Model of NN
nn_fit <- train(Class ~ ., 
                data = training,
                method = "mlp",
                trControl = fitControl)

# Model of CART
cart_fit <- train(Class ~ .,
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
error <- mean(pred != testing$Class)
print(paste("Accuracy ", 1 - error))

# Prediction with CART
pred <- predict(cart_fit, newdata = testing)
error <- mean(pred != testing$Class)
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

# Extracting correctly classified and incorrectly classified by models to build 
# a McNemar's 2x2 contigency table
get_correctly_classified_item <- function(fit_fold_result){
  return (fit_fold_result$rowIndex[fit_fold_result$pred == fit_fold_result$obs])
}
get_incorrectly_classified_item <- function(fit_fold_result){
  return (fit_fold_result$rowIndex[fit_fold_result$pred != fit_fold_result$obs])
}

nn_correct <- get_correctly_classified_item(nn_fold_result)
cart_correct <- get_correctly_classified_item(cart_fold_result)

nn_incorrect <- get_incorrectly_classified_item(nn_fold_result)
cart_incorrect <- get_incorrectly_classified_item(cart_fold_result)

c00 <- length(intersect(nn_incorrect, cart_incorrect))
c01 <- length(intersect(nn_incorrect, cart_correct))
c10 <- length(intersect(cart_incorrect, nn_correct))
c11 <- length(intersect(nn_correct, cart_correct))

mcnemar_mtrx <- matrix(c(c00, c01, c10, c11), byrow = T, nrow = 2, ncol = 2)
colnames(mcnemar_mtrx) <- c("No", "Yes")
rownames(mcnemar_mtrx) <- c("No", "Yes")
mcnemar_tbl <- as.table(mcnemar_mtrx)
################################################################################
# Performance measures
################################################################################
rocobj_1 <- roc(obs ~ malignant, nn_fold_result, ret = c("tp", "fp"))
rocobj_2 <- roc(obs ~ malignant, cart_fold_result, ret = c("tp", "fp"))
plot(rocobj_1, print.thres="best", col = "blue", print.auc=TRUE)
plot(rocobj_2, print.thres="best", col = "red", add=TRUE)
legend("bottomright", legend=c("NN", "CART"),
       col=c("blue", "red"), lwd=2)

################################################################################
# Statistical significance test
################################################################################
# Paired t-test
t.test(nn_accuracy_by_fold, cart_accuracy_by_fold, paired = T)

# McNemar's test
mcnemar.test(mcnemar_tbl, correct = F)

# Wilcoxon's signed-rank test
wilcox.test(nn_accuracy_by_fold, cart_accuracy_by_fold, paired = T, alternative = "two.sided", correct = F)
