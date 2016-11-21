# Load libraries -----------------------------------------------------------------------------
## missmap
require(Amelia) 

require(Hmisc)

## Boosting
require(gbm)


# Load data ----------------------------------------------------------------------------------
## Read data
train_raw <- read.csv("titanic/train.csv", na.strings = c("NA", ""))
test_raw <- read.csv("titanic/test.csv", na.strings = c("NA", ""))

test_raw["Survived"] <- rep(NA, nrow(test_raw))
## Make a copy of raw for pre-processing
train <- train_raw

# Data munging --------------------------------------------------------------------------------
## Observe missing data 
missmap(train, legend = TRUE, main = "Missing Map of Titanic Training Data")

## Learn from data
### Survived
barplot(table(train$Survived), names.arg = c("Death", "Survived"))

### Pclass
barplot(table(train$Pclass), names.arg = c("first", "second", "third"),
        main = "Pclass (Traveling class of passenger)")

mosaicplot(table(train$Pclass, train$Survived), color = TRUE,
           main = "Mosaic plot of Pclass ~ Survived")

### Fare
hist(train$Fare)

### Age
hist(train$Age)

### Sex
barplot(table(train$Sex))
plot(table(train$Sex, train$Survived), color = TRUE, main = "Sex ~ Survived")

### Embarked
plot(train$Embarked, names.arg = c("Cherbourg", "Queenstown", "Southampton"),
     main = "Embarking location")

mosaicplot(table(train$Embarked, train$Survived), color = TRUE)

### Parch
hist(train$Parch)

# Data Pre-processing -----------------------------------------------------------------------------
preprocessing <- function(df){
  ## Convert unnecessary factors to characters
  ### Sex
  levels(train$Sex) <- c(0, 1)
  train$Sex <- as.numeric(train$Sex)
  
  ### Embarked
  levels(train$Embarked) <- c(1, 2, 3)
  train$Embarked <- as.numeric(train$Embarked)
  
  ## Impute Age according to Title
  ### Observe Age according to Title
  bystats(df$Age, df$Title, fun = function(.) c(Mean = mean(.), Median = median(.)))
  
  ### Extract Title from Name using Regex
  get_title_from_name <- function(x) {
    index_of_comma <- regexpr("\\, [A-Z][a-z]+\\.", x)
    index_of_dot <- regexpr("\\. ", x)
    substr(x, index_of_comma + 2, index_of_dot - 1)
  }
  
  df["Title"] <- get_title_from_name(df$Name)
  
  ### Impute Age according to median value of corresponding Title
  unique_titles <- unique(df$Title)
  
  impute_age <- function(unique_titles, df) {
    for (title in unique_titles) {
      df$Age[which(df$Title == title)] <- impute(df$Age[df$Title == title], fun = median)  
    }
    return(df)
  }
  
  df <- impute_age(unique_titles, df)
  
  ### Observe Age after imputation
  bystats(df$Age, df$Title, fun = function(.) c(Mean = mean(.), Median = median(.)))
  missmap(df, legend = TRUE, main = "Missing Map of Titanic Training Data")
  
  ## Feature reduction
  ### Remove unneccessary features
  df <- df[c("PassengerId", "Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked", "Title")]
  names(df)
  
  return (df)
}

train <- preprocessing(train)

# Fitting Model -----------------------------------------------------------------------------
## Validation set approach --------------------------------------------------
### Set seeds to make partition reproducible
set.seed(123)

### Separate training set to 80% training batch and 20% test batch
training_size <- floor(0.8 * nrow(train))
training_indx <- sample(nrow(train), training_size)
training_batch <- train[training_indx, ]
testing_batch <- train[-training_indx, ]

## Logistic Regression ------------------------------------------------------
### Original model ------------------------------------
logistic_fit <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked,
    data = training_batch, family = binomial)
summary(logistic_fit)

#### Predict 
logistic_fit_result <- predict(logistic_fit, newdata = testing_batch, type = "response")

#### Using ROC curve to choose the best threshold, which is 0.410
logistic_fit_roc_curve <- roc(testing_batch$Survived ~ logistic_fit_result)
plot(logistic_fit_roc_curve, print.thres="best", print.thres.best.method="closest.topleft")

#### Use .410 as cut-off point
logistic_fit_result <- ifelse(logistic_fit_result > 0.410, 1, 0)
error <- mean(logistic_fit_result != testing_batch$Survived)
print(paste("Accuracy ", 1 - error))

### Tuning 1 ------------------------------------------
### Realise that Embarked is not sensitive enough to put in the model and
### more importantly, can cause overfitting. Remove it!
logistic_fit_tune_1 <- glm(Survived ~ Pclass + Sex + Age,
                    data = training_batch, family = binomial)

summary(logistic_fit_tune_1)
#### Predict 
logistic_fit_tune_1_result <- predict(logistic_fit_tune_1, newdata = testing_batch, type = "response")

#### Using ROC curve to choose the best threshold, which is 0.443
logistic_fit_1_roc_curve <- roc(testing_batch$Survived ~ logistic_fit_tune_1_result)

plot(logistic_fit_1_roc_curve, print.thres="best", 
     print.thres.best.method="closest.topleft", 
     add = TRUE,
     col = "red")

#### Use 0.443 as cut-off point
logistic_fit_tune_1_result <- ifelse(logistic_fit_tune_1_result > 0.443, 1, 0)
error <- mean(logistic_fit_tune_1_result != testing_batch$Survived)
print(paste("Accuracy ", 1 - error))

## Boosting -------------------------------------------------------------------
### Original model ------------------------------------
#### Build model
boosting_fit <- gbm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked,
                    data = training_batch, n.trees = 5000, interaction.depth = 4,
                    distribution = "bernoulli")
#summary(boosting_fit)

boosting_result <- predict(boosting_fit, newdata = testing_batch, 
                           n.trees = 5000, type = "response")

#### Using ROC curve to choose the best threshold, which is 0.420
boosting_roc_curve <- roc(testing_batch$Survived ~ boosting_result)
plot(boosting_roc_curve, print.thres="best", 
     print.thres.best.method="closest.topleft",
     col = "blue")

boosting_result <- ifelse(boosting_result > 0.420, 1, 0)
boosting_error <- mean(boosting_result != testing_batch$Survived)
print(paste("Accuracy ", 1 - boosting_error))

### Tuned model 1 ------------------------------------
#### Build model
boosting_fit_tune_1 <- gbm(Survived ~ Pclass + Sex + Age,
                    data = training_batch, n.trees = 5000, interaction.depth = 4,
                    distribution = "bernoulli")
summary(boosting_fit_tune_1)

boosting_tune_1_result <- predict(boosting_fit_tune_1, newdata = testing_batch, 
                           n.trees = 5000, type = "response")

#### Using ROC curve to choose the best threshold, which is 0.443
boosting_tune_1_roc_curve <- roc(testing_batch$Survived ~ boosting_tune_1_result)
plot(boosting_tune_1_roc_curve, print.thres="best", 
     print.thres.best.method="closest.topleft",
     col = "blue")

boosting_result <- ifelse(boosting_tune_1_result > 0.443, 1, 0)
boosting_error <- mean(boosting_result != testing_batch$Survived)
print(paste("Accuracy ", 1 - boosting_error))

# Compare models ----------------------------------------------------------------------
plot.new()
#jpeg("titanic/roc-curves.jpeg", width = 4, height = 4, units = 'in', res = 300)
plot(logistic_fit_roc_curve, print.thres="best", print.thres.best.method="closest.topleft")
plot(logistic_fit_1_roc_curve, print.thres="best", print.thres.best.method="closest.topleft", 
     add = TRUE,
     col = "red")
plot(boosting_roc_curve, print.thres="best", 
     print.thres.best.method="closest.topleft",
     add = TRUE,
     col = "blue")
plot(boosting_tune_1_roc_curve, print.thres="best", 
     print.thres.best.method="closest.topleft",
     add = TRUE,
     col = "brown")
#dev.off()

# Testing against test.csv of Kaggle --------------------------------------------------
## Pre-process ---------------------------------------------------------
test <- preprocessing(test_raw)
missmap(test)

## Because there is a Ms, and no record of her age, 
## so we can not impute her age according to median of Title
## Therefore, we have to MANUALLY fill a predicted record for her Age
test$Age[which(test$PassengerId == 980)] <- 30

## Predict using logistic regression ------------------------------------
logistic_result_test <- predict(logistic_fit, newdata = test, type = "response")
logistic_result_test <- ifelse(logistic_result_test > 0.410, 1, 0)
test$Survived <- logistic_result_test
test_submit <- test[c("PassengerId", "Survived")]
write.csv(test_submit, "titanic/test-estimated-logistic.csv", quote=F, row.names=F)

## Predict using boosting ------------------------------------------------
boosting_result_test <- predict(boosting_fit, newdata = test, n.trees = 5000, type = "response")
boosting_result_test <- ifelse(boosting_result_test > 0.420, 1, 0)
test$Survived <- boosting_result_test

test_submit <- test[c("PassengerId", "Survived")]
write.csv(test_submit, "titanic/test-estimated-boosting.csv", quote=F, row.names=F)

## Predict using boosting tune 1------------------------------------------
boosting_tune_1_result_test <- predict(boosting_fit_tune_1, newdata = test, n.trees = 5000, type = "response")
boosting_tune_1_result_test <- ifelse(boosting_tune_1_result_test > 0.420, 1, 0)
test$Survived <- boosting_tune_1_result_test

test_submit <- test[c("PassengerId", "Survived")]
write.csv(test_submit, "titanic/test-estimated-boosting-tune-1.csv", quote=F, row.names=F)


