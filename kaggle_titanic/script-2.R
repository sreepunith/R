# Load libraries -----------------------------------------------------------------------------
## missmap
require(Amelia) 

require(Hmisc)

## Boosting
require(gbm)

require(mfp)

require(pROC)
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

## 1st cycle ---------------------------------------------------------------
glm_fit_1 <- glm(Survived ~ Pclass + factor(Sex) + Age + SibSp + Parch + Embarked,
                    data = training_batch, family = binomial("logit"))
  
### Sort by p-value
coefs <- summary(glm_fit_1)$coefficients
coefs <- coefs[order(coefs[,4]),]


## 2nd cycle ---------------------------------------------------------------
training_batch <- training_batch[!is.na(training_batch$Embarked),]
missmap(training_batch)

glm_fit_2 <- glm(Survived ~ I(Pclass == 2) + I(Pclass == 3)+ factor(Sex) + Age + SibSp + I(Embarked == "S"),
               data = training_batch, family = binomial)

## MFP
mfp_age <- mfp(Survived ~ I(Pclass == 2) + I(Pclass == 3)+ factor(Sex) + fp(Age) + SibSp + I(Embarked == "S"),
               data = training_batch)

mfp_age$pvalues # Age is non-linear

mfp_sibsp <- mfp(Survived ~ I(Pclass == 2) + I(Pclass == 3)+ factor(Sex) + log((Age/10)) + fp(SibSp) + I(Embarked == "S"),
               data = training_batch)

mfp_sibsp$pvalues # Age is non-linear

glm_fit_4 <- glm(Survived ~ I(Pclass == 2) + I(Pclass == 3)+ factor(Sex) + log((Age/10)) + I((SibSp+1)^1) + I(Embarked == "S"),
                 data = training_batch, family = binomial)

missmap(training_batch)
lowess_curve <- lowess(log(fitted(glm_fit_4)/(1-fitted(glm_fit_4))) ~ training_batch$SibSp)
plot(training_batch$SibSp, log(fitted(glm_fit_4)/(1-fitted(glm_fit_2))))
lines(lowess_curve, col = "red")



glm_fit_4_result <- predict(glm_fit_4, newdata = testing_batch, type = "response")

glm_fit_4_result_roc_curve <- roc(testing_batch$Survived ~ glm_fit_4_result)

plot(glm_fit_4_result_roc_curve, print.thres="best", print.thres.best.method="closest.topleft")

logistic_fit_result <- ifelse(glm_fit_4_result > 0.393, 1, 0)
error <- mean(logistic_fit_result != testing_batch$Survived)
print(paste("Accuracy ", 1 - error))

# Testing against test.csv of Kaggle --------------------------------------------------
## Pre-process ---------------------------------------------------------
test <- preprocessing(test_raw)
missmap(test)

## Because there is a Ms, and no record of her age,
## so we can not impute her age according to median of Title
## Therefore, we have to MANUALLY fill a predicted record for her Age
test$Age[which(test$PassengerId == 980)] <- 30

## Predict using logistic regression ------------------------------------
logistic_result_test <- predict(glm_fit_4, newdata = test, type = "response")
logistic_result_test <- ifelse(logistic_result_test > 0.393, 1, 0)
test$Survived <- logistic_result_test
test_submit <- test[c("PassengerId", "Survived")]
write.csv(test_submit, "titanic/test-estimated-logistic-FP.csv", quote=F, row.names=F)