# Description: the goal of this script is to buid classifiers for predicting 
# survivors of Titanic accident by using 3 learning algorithms Neural Networks 
# (NN), CART, Random Forest (RF)
# Algorithms: NN, CART, RF
# Evaluation method: Accuracy, ROC, Statistical tests (Wilcoxon's signed-rank).
################################################################################
# Load libraries
################################################################################
library(VIM) #aggr
library(mice) #missing
library(caret)
library(corrplot) #corrplot
library(pROC)

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
combined_dt$Fare[which(train$Fare == 0)] <- NA

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

# Scale and center
combined_dt$Fare <- scale(combined_dt$Fare, scale = T, center = T)
combined_dt$Age <- scale(combined_dt$Age, scale = T, center = T)

# After preprocessing both, separate the data back to train and test
train <- combined_dt[1:nrow(train),]
train["Survived"] <- train_raw$Survived #add reponse variable back after imputation
train$Survived <- as.factor(train$Survived)

test <- combined_dt[(nrow(train)+1):nrow(combined_dt), ]

# Survived
train$Survived <- factor(train$Survived, labels = make.names(unique(train$Survived)))
# Create dummy variables for correlation testing
for(level in unique(train$SibSp)) { #SibSp
  train[paste("SibSp", level, sep = "_")] <- ifelse(train$SibSp == level, 1, 0)
}

for(level in unique(train$Parch)) { #Parch
  train[paste("Parch", level, sep = "_")] <- ifelse(train$Parch == level, 1, 0)
}

for(level in unique(train$Embarked)) { #Embarked
  train[paste("Embarked", level, sep = "_")] <- ifelse(train$Embarked == level, 1, 0)
}

for(level in unique(train$Title)) { #Embarked
  train[paste("Title", level, sep = "_")] <- ifelse(train$Title == level, 1, 0)
}

# Fare
train$Fare_factor <- as.factor(findInterval(train$Fare, c(32)))

for(level in unique(train$Fare_factor)){
  train[paste("Fare_factor", level, sep = "_")] <- ifelse(train$Fare_factor == level, 1, 0)
}

# Correlation testing
chisq.test(train$Parch_0, train$Survived, correct = F) #1.082e-05
chisq.test(train$Parch_1, train$Survived, correct = F) #6.2e-05
chisq.test(train$Parch_2, train$Survived, correct = F) #0.02514
chisq.test(train$Parch_3, train$Survived, correct = F) #0.3189
chisq.test(train$Parch_4, train$Survived, correct = F) #0.1136
chisq.test(train$Parch_5, train$Survived, correct = F) #0.3966
chisq.test(train$Parch_6, train$Survived, correct = F) #0.4297

chisq.test(train$SibSp_0, train$Survived, correct = F) #0.000543
chisq.test(train$SibSp_1, train$Survived, correct = F) #2.388e-07
chisq.test(train$SibSp_2, train$Survived, correct = F) #0.3738
chisq.test(train$SibSp_3, train$Survived, correct = F) #0.2666
chisq.test(train$SibSp_4, train$Survived, correct = F) #0.05562
chisq.test(train$SibSp_5, train$Survived, correct = F) #0.07675
chisq.test(train$SibSp_8, train$Survived, correct = F) #0.03604

chisq.test(train$Embarked_S, train$Survived, correct = F) #7.896e-06
chisq.test(train$Embarked_C, train$Survived, correct = F) #5.116e-07
chisq.test(train$Embarked_Q, train$Survived, correct = F) #0.9132

chisq.test(train$Title_Master, train$Survived, correct = F) #0.01097
chisq.test(train$Title_Men_Honorable, train$Survived, correct = F) #0.2666
chisq.test(train$Title_Women_Honorable, train$Survived, correct = F) #0.07284
chisq.test(train$Title_Miltary, train$Survived, correct = F) #0.9406
chisq.test(train$Title_Mr, train$Survived, correct = F) #2.2e-16
chisq.test(train$Title_Mrs, train$Survived, correct = F) #2.2e-16
chisq.test(train$Title_Miss, train$Survived, correct = F) #2.2e-16

# Merging Parch > 2 into 1 single feature and test whether the new feature is sensitive
for(level in unique(train$Parch)) { #Pclass
  train[paste("Parch", level, sep = "_")] <- NULL
}
train$Parch <- findInterval(train$Parch, c(0, 1, 2, 3))
for(level in unique(train$Parch)) { #Pclass
  train[paste("Parch", level, sep = "_")] <- ifelse(train$Parch == level, 1, 0)
}
chisq.test(train$Parch_4, train$Survived, correct = F) #0.3467 -> remove this feature

# Merging 8 > SibSp > 1 into 1 single feature and test whether the new feature is sensitive
for(level in unique(train$SibSp)) { #SibSp
  train[paste("SibSp", level, sep = "_")] <- NULL
}
train$SibSp <- findInterval(train$SibSp, c(0, 1, 2, 8))
for(level in unique(train$SibSp)) { #SibSp
  train[paste("SibSp", level, sep = "_")] <- ifelse(train$SibSp == level, 1, 0)
}
chisq.test(train$SibSp_3, train$Survived, correct = F) #0.1353 -> remove this feature

# Remove features
train[, c("SibSp", "SibSp_3", "Parch", "Parch_4", "Embarked", "Embarked_Q", "Title", 
          "Title_Men_Honorable", "Title_Women_Honorable", "Title_Miltary")] <- NULL

################################################################################
# Building models
################################################################################
# Split data
inTraining <- createDataPartition(train$Survived, p = .75, list = FALSE)
training <- train[inTraining,]
testing  <- train[-inTraining,]

# Train control
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                           classProbs = T, savePredictions = T)

# Model of NN
nn_grid <-  expand.grid(size = c(4, 5, 6, 7, 8, 9, 10))
nn_fit <- train(Survived ~ ., 
                data = training,
                method = "mlp",
                tuneGrid = nn_grid,
                trControl = fitControl)

# Model of CART
cart_grid <-  expand.grid(cp = seq(1,10, 2)*0.1)
cart_fit <- train(Survived ~ .,
                  data = training,
                  method = "rpart",
                  tuneGrid = cart_grid,
                  trControl = fitControl)

# Model
rf_grid <-  expand.grid(mtry = seq(1, 7, 1))
rf_fit <- train(Survived ~ .,
               data = training, 
               method = "parRF",
               trControl = fitControl,
               tuneGrid = rf_grid)

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
# Fit model to an entire training
################################################################################
# Train control
fit_control_1 <- trainControl(method = "none",
                           classProbs = T, savePredictions = T)

# Model of NN
grid_1 <-  expand.grid(mtry = best_mtry)
model_1 <- train(Survived ~ ., 
                data = training,
                method = "parRF",
                tuneGrid = grid_1,
                trControl = fit_control_1)

# Generate probabilities for test set
pred <- predict(model_1, newdata = testing, type = "prob")

# Use ROC to find the best cut-off point
dt <- data.frame(X1 = pred$X1, obs = testing$Survived)
rocobj <- roc(obs ~ X1, dt, ret = c("tp", "fp"))
plot(rocobj, print.thres="best", col = "blue", print.auc=TRUE) #0.494

# Testing
testing_survived <- ifelse(pred$X1 > 0.494, "X1", "X0")
error <- mean(testing_survived != testing$Survived)
print(paste("Accuracy ", 1 - error))

################################################################################
# Fit model to an entire train data
################################################################################
# Train control
fitControl_2 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                           classProbs = T, savePredictions = T)
model_2 <- train(Survived ~ ., 
                     data = train,
                     method = "parRF",
                     tuneGrid = grid_1,
                     trControl = fitControl_2)

################################################################################
# Predict
################################################################################
# Generate probabilities for test set
test_prediction <- predict(model_2, newdata = test, type = "prob")
survived <- ifelse(test_prediction$X1 > 0.306, "1", "0")
survived <- as.factor(survived)

# Attach prediction results to test set
test["Survived"] <- survived
test["PassengerId"] <- test_raw$PassengerId

# Create submission file
test_submit <- test[c("PassengerId", "Survived")]
write.csv(test_submit, "results/randomforest_test_result.csv", 
          quote = F, row.names=F)
