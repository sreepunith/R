# Load libraries ------------------------------------------------------------------------------------------------
## missmap
require(Amelia) 

require(Hmisc)

## Boosting
require("randomForest")

require(ggplot2)

require(rpart)

require(pROC)

# Load data -----------------------------------------------------------------------------------------------------
## Read data
train_raw <- read.csv("titanic/train.csv", na.strings = c("NA", ""))
test_raw <- read.csv("titanic/test.csv", na.strings = c("NA", ""))

# require(foreign)
# write.arff(train_raw, "titanic/train.arff", eol = "\n", relation = deparse(substitute(x)))

## Make a copy of raw for pre-processing
train <- train_raw
train$Survived <- as.factor(train$Survived)

test <- test_raw
# test["Survived"] <- rep(NA, nrow(test))
# Data munging & Preprocessing ---------------------------------------------------------------------------------------------------
## Observe missing data 
missmap(train, legend = TRUE, main = "Missing Map of Titanic Training Data")

## PassengerId -------------------------------------------------------------------------------------------------
train$PassengerId <- NULL


## Name -------------------------------------------------------------------------------------------------
train$Name <- NULL

## Cabin -------------------------------------------------------------------------------------------------
train$Cabin <- NULL

## Ticket -------------------------------------------------------------------------------------------------
train$Ticket <- NULL

## Embarked -------------------------------------------------------------------------------------------------
embarked <- data.frame(table(train$Embarked, exclude = NULL))
names(embarked) <- c("Location", "Total")
head(embarked)
ggplot(data = embarked, aes(x = Location, y = Total, fill = Location)) +
  geom_bar(stat = "identity", width = .5) +
  geom_text(aes(label= Total), vjust=-1)


train$Embarked[which(is.na(train$Embarked))] <- "S"

missmap(train)

## Title -----------------------------------------------------------------------------------------------------
### Extract Title from Name using Regex
# get_title_from_name <- function(x) {
#   index_of_comma <- regexpr("\\, [A-Z][a-z]+\\.", x)
#   index_of_dot <- regexpr("\\. ", x)
#   return (substr(x, index_of_comma + 2, index_of_dot - 1))
# }
# 
# train["Title"] <- get_title_from_name(train$Name)
# train$Title <- as.factor(train$Title)
# 
# missmap(train)

## Parch -------------------------------------------------------------------------------------------------
parch <- data.frame(table(train$Parch, exclude = NULL))
names(parch) <- c("Parch", "Total")
head(parch)
ggplot(data = parch, aes(x = Parch, y = Total, fill = Parch)) +
  geom_bar(stat = "identity", width = .5) +
  geom_text(aes(label= Total), vjust=-1)

discretize_parch <- function(x) {
  if (isTRUE(x >1)) {
    return ("Have children")
  } else {
    return ("No Children")
  }
}

train$Parch <- sapply(train$Parch, discretize_parch)
train$Parch
train$Parch <- as.factor(train$Parch)


## SibSp -------------------------------------------------------------------------------------------------
discretize_sibsp <- function(x) {
  if (isTRUE(x >1)) {
    return ("Have Sibs")
  } else {
    return ("No Sibs")
  }
}

train$SibSp <- sapply(train$SibSp, discretize_sibsp)
train$SibSp
train$SibSp <- as.factor(train$SibSp)

## Fare -------------------------------------------------------------------------------------------------
# train <- train[-which(train$Fare == 0),] 
train$Fare[train$Fare == 0] <- NA


## Age -------------------------------------------------------------------------------------------------
ggplot(data = train, aes(x = Age)) +
  geom_histogram(binwidth = 5, colour = "black", fill = "#2ECCFA")

missmap(train)

### Impute age & fare using case deletion ---------------------------
train_1 <- train[-which(is.na(train$Age)),]
train_1 <- train_1[-which(is.na(train_1$Fare)),]
missmap(train_1)

### Impute age & fare using most common method ----------------------
age_2 <- impute(train$Age, mean)
fare_2 <- impute(train$Fare, mean)
train_2 <- train
train_2$Age <- age_2
train_2$Fare <- fare_2
missmap(train_2)

### Impute age & fare using knn --------------------------------------
require(DMwR)
train_3 <- knnImputation(train, k = 2, meth = "median")
missmap(train_3)

### Impute age & fare using weighted knn -----------------------------
require(DMwR)
train_4 <- knnImputation(train, k = 2, meth = "weighAvg")
missmap(train_4)

### Impute age & fare using random forest ---------------------------
require(missForest)
train_5 <- missForest(train)
train_5 <- train_5$ximp
missmap(train_5)

require(foreign)
write.arff(train_1, "titanic/train_1.arff", eol = "\n", relation = deparse(substitute(x)))
write.arff(train_2, "titanic/train_2.arff", eol = "\n", relation = deparse(substitute(x)))
write.arff(train_3, "titanic/train_3.arff", eol = "\n", relation = deparse(substitute(x)))
write.arff(train_4, "titanic/train_4.arff", eol = "\n", relation = deparse(substitute(x)))
write.arff(train_5, "titanic/train_5.arff", eol = "\n", relation = deparse(substitute(x)))

## Clustering ---------------------------------------------------------------
require("cluster")
clusters <- pam(train_2, 2)$clustering

rearrange_cluster_name <- function(x){
  if (x == 1) {return (0)}
  else {return (1)}
}

clusters <- sapply(clusters, rearrange_cluster_name)
clusters <- as.numeric(as.character(clusters))
error <- mean(clusters != as.numeric(as.character(train_2$Survived)))
print(paste("Accuracy ", 1 - error))

train_extracted <- train_2[clusters == as.numeric(as.character(train_2$Survived)),]

write.arff(train_extracted, "titanic/train_extracted.arff", eol = "\n", relation = deparse(substitute(x)))

## Validation set approach --------------------------------------------------

### Set seeds to make partition reproducible
set.seed(123)

### Separate training set to 80% training batch and 20% test batch
training_size <- floor(0.8 * nrow(train_extracted))
training_indx <- sample(nrow(train_extracted), training_size)
training_batch <- train_extracted[training_indx, ]
testing_batch <- train_extracted[-training_indx, ]

## Build model -----------------------------------------------------------------------------------------------------
m1 <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Fare,
                   data = training_batch, importance = TRUE, proximity=TRUE)

summary(m1)

m1_result <- predict(m1, newdata = testing_batch, type = "response")
error <- mean(m1_result != testing_batch$Survived)
print(paste("Accuracy ", 1 - error))

# Testing against test.csv of Kaggle --------------------------------------------------
missmap(test)

## PassengerId -------------------------------------------------------------------------------------------------
test$PassengerId <- NULL


## Name -------------------------------------------------------------------------------------------------
test$Name <- NULL

## Cabin -------------------------------------------------------------------------------------------------
test$Cabin <- NULL

## Ticket -------------------------------------------------------------------------------------------------
test$Ticket <- NULL

## Embarked -------------------------------------------------------------------------------------------------
missmap(test)

## Parch -------------------------------------------------------------------------------------------------
parch <- data.frame(table(test$Parch, exclude = NULL))
names(parch) <- c("Parch", "Total")
head(parch)
ggplot(data = parch, aes(x = Parch, y = Total, fill = Parch)) +
  geom_bar(stat = "identity", width = .5) +
  geom_text(aes(label= Total), vjust=-1)

discretize_parch <- function(x) {
  if (isTRUE(x >1)) {
    return ("Have children")
  } else {
    return ("No Children")
  }
}

test$Parch <- sapply(test$Parch, discretize_parch)
test$Parch
test$Parch <- as.factor(test$Parch)


## SibSp -------------------------------------------------------------------------------------------------
discretize_sibsp <- function(x) {
  if (isTRUE(x >1)) {
    return ("Have Sibs")
  } else {
    return ("No Sibs")
  }
}

test$SibSp <- sapply(test$SibSp, discretize_sibsp)
test$SibSp
test$SibSp <- as.factor(test$SibSp)

## Fare -------------------------------------------------------------------------------------------------
# train <- train[-which(train$Fare == 0),] 
test$Fare[test$Fare == 0] <- NA


## Age -------------------------------------------------------------------------------------------------
### Impute age & fare using most common method ----------------------
age_2 <- impute(test$Age, mean)
fare_2 <- impute(test$Fare, mean)
test_2 <- test
test_2$Age <- age_2
test_2$Fare <- fare_2
missmap(test_2)

test_2["Survived"] <- rep(NA, nrow(test_2))
## Predict
test_result <- predict(m1, newdata = test_2, type = "response")
test_2["Survived"] <- test_result
test_2["PassengerId"] <- test_raw["PassengerId"]

test_submit <- test_2[c("PassengerId", "Survived")]
write.csv(test_submit, "titanic/submission.csv", quote=F, row.names=F)


