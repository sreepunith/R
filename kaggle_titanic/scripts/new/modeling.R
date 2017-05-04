# Description: the goal of this script is to buid classifiers for predicting 
# survivors of Titanic accident by using 3 learning algorithms Neural Networks 
# (NN), CART, Random Forest (RF)
# Algorithms: NN, CART, RF
# Evaluation method: Accuracy, ROC, Statistical tests (Wilcoxon's signed-rank)..
################################################################################
# Load libraries
################################################################################
library(VIM) #aggr
library(mice) #missing
library(caret)
library(corrplot) #corrplot
library(pROC)
library(moments) #skewness, kurtosis
library(MASS) #boxcox
library(rcompanion) #plotNormalHistogram
set.seed(123)
################################################################################
# Load dataset
################################################################################
train_raw <- read.csv("data/train.csv", na.strings = c("NA", ""))
train <- train_raw

test_raw <- read.csv("data/test.csv", na.strings = c("NA", ""))
test <- test_raw #backup

################################################################################
# Preprocessing (both train and test data at the same time)
################################################################################
# Combind train and test
train$Survived <- NULL #temporarily remove Survived from train
combined_dt <- rbind(train, test)

# Identify missing values
# Fare == 0 doesnt make sense, thus should be missing values
combined_dt$Fare[which(combined_dt$Fare == 0)] <- NA

# More than 70% of Cabin is missing, thus we have to remove
combined_dt$Cabin <- NULL

# Ticket makes no sense
combined_dt$Ticket <- NULL

# Observe the missing data again
missing_plot <- aggr(combined_dt, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))

# Impute Embarked
combined_dt$Embarked[which(is.na(combined_dt$Embarked))] <- "S"
missing_plot <- aggr(combined_dt, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))

# Extract Title from Name
get_title_from_name <- function(x) {
  index_of_comma <- regexpr("\\, [A-Z][a-z]+\\.", x)
  index_of_dot <- regexpr("\\. ", x)
  return (substr(x, index_of_comma + 2, index_of_dot - 1))
}

combined_dt["Title"] <- get_title_from_name(combined_dt$Name)
combined_dt$Title <- as.factor(combined_dt$Title)
summary(combined_dt$Title)

# Impute age & fare
imp <- mice(combined_dt[, -c(1, 3)], maxit = 20, m = 6)
combined_dt_imp <- complete(imp, "long", inc = TRUE)

col <- rep(c("blue", "red")[1+as.numeric(is.na(imp$data$Age))], 6)
stripplot(Age~.imp, data = combined_dt_imp, jit = TRUE, fac = 0.8, col = col, 
          xlab = "Imputation Number") #explore the distribution of each imputed version

combined_dt <- complete(imp, 1) # select the 1st imputed version
missing_plot <- aggr(combined_dt, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(combined_dt), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern")) #observe missing data again

# Too many factors in Title, we need to merge them
combined_dt$Title <- as.character(combined_dt$Title) 
combined_dt$Title[which(combined_dt$Title == "Capt")] <- "Miltary"
combined_dt$Title[which(combined_dt$Title == "Col")] <- "Miltary"
combined_dt$Title[which(combined_dt$Title == "Major")] <- "Miltary"

combined_dt$Title[which(combined_dt$Title == "Don")] <- "Men_Honorable"
combined_dt$Title[which(combined_dt$Title == "Dr")] <- "Men_Honorable"
combined_dt$Title[which(combined_dt$Title == "Jonkheer")] <- "Men_Honorable"
combined_dt$Title[which(combined_dt$Title == "Rev")] <- "Men_Honorable"
combined_dt$Title[which(combined_dt$Title == "Sir")] <- "Men_Honorable"

combined_dt$Title[which(combined_dt$Title == "Mme")] <- "Mr"

combined_dt$Title[which(combined_dt$Title == "Lady")] <- "Women_Honorable"
combined_dt$Title[which(combined_dt$Title == "Rothes, the Countess")] <- "Women_Honorable"
combined_dt$Title[which(combined_dt$Title == "Dona")] <- "Women_Honorable"

combined_dt$Title[which(combined_dt$Title == "Ms")] <- "Miss"
combined_dt$Title[which(combined_dt$Title == "Mlle")] <- "Miss"

combined_dt$Title <- as.factor(combined_dt$Title) 
summary(combined_dt$Title)

# Create dummy variables for feature selection
# for(level in unique(combined_dt$SibSp)) { #SibSp
#   combined_dt[paste("SibSp", level, sep = "_")] <- ifelse(combined_dt$SibSp == level, 1, 0)
# }
# 
# for(level in unique(combined_dt$Parch)) { #Parch
#   combined_dt[paste("Parch", level, sep = "_")] <- ifelse(combined_dt$Parch == level, 1, 0)
# }
# 
# for(level in unique(combined_dt$Embarked)) { #Embarked
#   combined_dt[paste("Embarked", level, sep = "_")] <- ifelse(combined_dt$Embarked == level, 1, 0)
# }
# 
# for(level in unique(combined_dt$Title)) { #Title
#   combined_dt[paste("Title", level, sep = "_")] <- ifelse(combined_dt$Title == level, 1, 0)
# }
# 
# for(level in unique(combined_dt$Pclass)) { #Pclass
#   combined_dt[paste("Pclass", level, sep = "_")] <- ifelse(combined_dt$Pclass == level, 1, 0)
# }
# 
# cnames <- colnames(combined_dt[,9:ncol(combined_dt)]) 
# combined_dt[,9:ncol(combined_dt)] <- lapply(cnames, #change to factor
#                                             function(x) as.factor(as.character(combined_dt[,x])))
# combined_dt$SibSp <- NULL
# combined_dt$Parch <- NULL
# combined_dt$Embarked <- NULL
# combined_dt$Title <- NULL
# combined_dt$Pclass <- NULL
# summary(combined_dt)

# Scale and center
# combined_dt$Fare <- scale(combined_dt$Fare, scale = T, center = T)
# combined_dt$Age <- scale(combined_dt$Age, scale = T, center = T)

# Measure skewness and kurtosis
skewness(combined_dt$Age)
skewness(combined_dt$Fare)
kurtosis(combined_dt$Age)
kurtosis(combined_dt$Fare)
plotNormalHistogram(combined_dt$Fare)
plotNormalHistogram(combined_dt$Age)

bc <- BoxCoxTrans(combined_dt$Fare)
combined_dt$Fare <- ((combined_dt$Fare ^ bc$lambda) - 1)/bc$lambda #boxcox transform with lambda = -0.5
plotNormalHistogram(combined_dt$Fare) #http://rcompanion.org/handbook/I_12.html

# After preprocessing both, separate the data back to train and test
combined_dt$Pclass <- as.factor(combined_dt$Pclass)
combined_dt$SibSp <- as.factor(combined_dt$SibSp)
combined_dt$Parch <- as.factor(combined_dt$Parch)
summary(combined_dt)

train <- combined_dt[1:nrow(train),]
train["Survived"] <- train_raw$Survived #add reponse variable back after imputation
train$Survived <- as.factor(train$Survived)
test <- combined_dt[(nrow(train)+1):nrow(combined_dt), ]

# Survived
train$Survived <- factor(train$Survived, labels = make.names(unique(train$Survived)))

# Feature selection
in_feature_selection <- createDataPartition(train$Survived, p = .80, list = FALSE)
building <- train[in_feature_selection,]
feature_selection  <- train[-in_feature_selection,]

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 100,
                           classProbs = T, savePredictions = T)
rf_grid <-  expand.grid(mtry = seq(1, 8, 1))
rf_fs <- train(Survived ~ .,
                data = feature_selection, 
                method = "RRFglobal",
                trControl = fitControl,
                tuneLength = 10)
importance <- varImp(rf_fs, scale=FALSE) #feature ranking
importance
plot(importance) #plot ranking of features

################################################################################
# Building models
################################################################################
# Split data
in_training <- createDataPartition(building$Survived, p = .75, list = FALSE)
training <- train[in_training,]
testing  <- train[-in_training,]

# Train control
fit_control <- trainControl(method = "repeatedcv", number = 10, repeats = 100,
                           classProbs = T, savePredictions = T)

# Model of NN
nn_grid <-  expand.grid(size = c(4, 14, 1))
nn_fit <- train(Survived ~ Title_Mr + Sex + Fare + Age + Title_Mrs + 
                  Title_Miss + Pclass_3 + SibSp_0 + Title_Master + Pclass_1 +
                  Parch_0 + SibSp_1 + Pclass_2 + Embarked_S + SibSp_2 + SibSp_3 + Embarked_Q, 
                data = training,
                method = "mlp",
                tuneGrid = nn_grid,
                trControl = fit_control, tuneLength = 10)

# Model of CART
cart_grid <-  expand.grid(cp = seq(1,10, 2)*0.1)
cart_fit <- train(Survived ~ Title_Mr + Sex + Fare + Age + Title_Mrs + 
                    Title_Miss + Pclass_3 + SibSp_0 + Title_Master + Pclass_1 +
                    Parch_0 + SibSp_1 + Pclass_2 + Embarked_S + SibSp_2 + SibSp_3 + Embarked_Q,
                  data = training,
                  method = "rpart",
                  tuneGrid = cart_grid,
                  trControl = fit_control, tuneLength = 10)

# Model
rf_grid <-  expand.grid(mtry = seq(1, 20, 2))
rf_fit <- train(Survived ~ Title_Mr + Sex + Fare + Age + Title_Mrs + 
                  Title_Miss + Pclass_3 + SibSp_0 + Title_Master + Pclass_1 +
                  Parch_0 + SibSp_1 + Pclass_2 + Embarked_S + SibSp_2 + SibSp_3 + Embarked_Q,
               data = training, 
               method = "parRF",
               trControl = fit_control,
               tuneGrid = rf_grid, tuneLength = 10)

plot(cart_fit)
plot(nn_fit)
plot(rf_fit)

################################################################################
# Measure performance using ROC
################################################################################
# Extract the best size of nn & best cp of cart
best_size <- nn_fit$bestTune$size
best_cp <- cart_fit$bestTune$cp
best_mtry <- rf_fit$bestTune$mtry

# Extracting all fold results according to best size and best cp
nn_fold_result <- nn_fit$pred[nn_fit$pred$size == best_size, ]
cart_fold_result <- cart_fit$pred[cart_fit$pred$cp == best_cp, ]
rf_fold_result <- rf_fit$pred[rf_fit$pred$mtry == best_mtry, ]

rocobj_1 <- roc(obs ~ X1, nn_fold_result, ret = c("tp", "fp"))
rocobj_2 <- roc(obs ~ X1, cart_fold_result, ret = c("tp", "fp"))
rocobj_3 <- roc(obs ~ X1, rf_fold_result, ret = c("tp", "fp"))
plot(rocobj_1, print.thres="best", col = "blue", print.auc=TRUE)
plot(rocobj_2, print.thres="best", col = "red", add=TRUE)
plot(rocobj_3, print.thres="best", col = "green", add=TRUE)
legend("bottomright", legend=c("NN", "CART", "RF"),
       col=c("blue", "red", "green"), lwd=2)

################################################################################
# Statistical test
################################################################################
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
rf_accuracy_by_fold <- accuracy_by_fold(rf_fold_result)

# paired t-test for NN and RF because they seems equivalent
t.test(nn_accuracy_by_fold, rf_accuracy_by_fold, paired = T)

# ANOVA test for 3 classifiers
accuracy <- c(nn_accuracy_by_fold, cart_accuracy_by_fold, rf_accuracy_by_fold)
classifier <- c(rep("NN", 10), rep("CART", 10), rep("RF", 10))
dt <- data.frame(accuracy, classifier)
anova_result <- aov(accuracy ~ classifier, data = dt)
summary(anova_result)

################################################################################
# Testing on the holdout set
################################################################################
# Generate probabilities for test set
nn_pred <- predict(nn_fit, newdata = testing, type = "prob")
cart_pred <- predict(cart_fit, newdata = testing, type = "prob")
rf_pred <- predict(rf_fit, newdata = testing, type = "prob")

# ROC
rf_holdout_result <- data.frame(X1 = rf_pred$X1, obs = testing$Survived)
rocobj <- roc(obs ~ X1, rf_holdout_result, ret = c("tp", "fp"))
rets <- c("threshold", "specificity", "sensitivity", "accuracy", "tn", "tp", "fn", "fp", "npv", 
          "ppv", "1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv")
ci.coords(rocobj, x="best", input = "threshold", ret=rets)
plot(rocobj, col = "blue", print.auc = TRUE)


cart_holdout_result <- data.frame(X1 = cart_pred$X1, obs = testing$Survived)
rocobj <- roc(obs ~ X1, cart_holdout_result, ret = c("tp", "fp"))
rets <- c("threshold", "specificity", "sensitivity", "accuracy", "tn", "tp", "fn", "fp", "npv", 
          "ppv", "1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv")
ci.coords(rocobj, x="best", input = "threshold", ret=rets)
plot(rocobj, print.thres="best", col = "red", add = T)

nn_holdout_result <- data.frame(X1 = nn_pred$X1, obs = testing$Survived)
rocobj <- roc(obs ~ X1, nn_holdout_result, ret = c("tp", "fp"))
rets <- c("threshold", "specificity", "sensitivity", "accuracy", "tn", "tp", "fn", "fp", "npv", 
          "ppv", "1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv")
ci.coords(rocobj, x="best", input = "threshold", ret=rets)
plot(rocobj, print.thres="best", col = "green", add = T)

legend("bottomright", legend=c("NN", "CART", "RF"),
       col=c("green", "red", "blue"), lwd=2)

# Testing
testing_survived <- ifelse(rf_pred$X1 > 0.5320, "X1", "X0")
error <- mean(testing_survived != testing$Survived)
print(paste("Accuracy ", 1 - error))

################################################################################
# Predict
################################################################################
# Generate probabilities for test set
test_prediction <- predict(rf_fit, newdata = test, type = "prob")
survived <- ifelse(test_prediction$X1 > 0.5320, "1", "0")
survived <- as.factor(survived)

# Attach prediction results to test set
test["Survived"] <- survived
test["PassengerId"] <- test_raw$PassengerId

# Create submission file
test_submit <- test[c("PassengerId", "Survived")]
write.csv(test_submit, "results/randomforest_test_result.csv", 
          quote = F, row.names=F)
