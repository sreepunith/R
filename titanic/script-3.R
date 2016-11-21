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
# train$PassengerId <- NULL
# train$Pclass[train$Pclass==1] <- "firstclass"
# train$Pclass[train$Pclass==2] <- "seco"
# train$Pclass[train$Pclass==3] <- "firstclass"

## Title -----------------------------------------------------------------------------------------------------
## Extract Title from Name using Regex
get_title_from_name <- function(x) {
  index_of_comma <- regexpr("\\, [A-Z][a-z]+\\.", x)
  index_of_dot <- regexpr("\\. ", x)
  return (substr(x, index_of_comma + 2, index_of_dot - 1))
}

train["Title"] <- get_title_from_name(train$Name)
train$Title <- as.factor(train$Title) 

missmap(train)


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

## Fare -------------------------------------------------------------------------------------------------
# train <- train[-which(train$Fare == 0),] 
train$Fare[train$Fare == 0] <- NA

# ### Impute age & fare using random forest ---------------------------
# train$Age <- NULL
missing_age_index <- train$PassengerId[is.na(train$Age)]

library(mice)
post <- mice(train, maxit = 0)$post
post["Age"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(1,70))"
post["Fare"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(1,500))"
restricted <- mice(train, m = 100, post = post, seed = 85444, method = 'norm')
train_6 <- complete(restricted, 1)
train <- train_6
summary(train$Age)

fare_outliers <- which(train$Fare > 300)
train <- train[-fare_outliers,]
plot(train$Survived ~ train$Fare)


discretize_fare <- function(x) {
  if (isTRUE(x > 0 && x < 20)) {
    return ("normal")
  } else if (isTRUE(x >= 20 && x < 80)) {
    return ("regular")
  } else if (isTRUE(x >= 80)) {
    return ("expensive")
  }
}

train$Fare <- sapply(train$Fare, discretize_fare)
train$Fare <- as.factor(train$Fare)


## Parch -------------------------------------------------------------------------------------------------
parch <- data.frame(table(train$Parch, exclude = NULL))
names(parch) <- c("Parch", "Total")
head(parch)
ggplot(data = parch, aes(x = Parch, y = Total, fill = Parch)) +
  geom_bar(stat = "identity", width = .5) +
  geom_text(aes(label= Total), vjust=-1)

discretize_parch <- function(x) {
  if (isTRUE(x >1)) {
    return ("havechildren")
  } else {
    return ("nochildren")
  }
}

train$Parch <- sapply(train$Parch, discretize_parch)
train$Parch
train$Parch <- as.factor(train$Parch)


## SibSp -------------------------------------------------------------------------------------------------
discretize_sibsp <- function(x) {
  if (isTRUE(x >1)) {
    return ("havesibs")
  } else {
    return ("nosibs")
  }
}

train$SibSp <- sapply(train$SibSp, discretize_sibsp)
train$SibSp
train$SibSp <- as.factor(train$SibSp)



## Age -------------------------------------------------------------------------------------------------
# ggplot(data = train, aes(x = Age)) +
#   geom_histogram(binwidth = 5, colour = "black", fill = "#2ECCFA")
# 
# missmap(train)

# ### Impute age & fare using case deletion ---------------------------
# train_1 <- train[-which(is.na(train$Age)),]
# train_1 <- train_1[-which(is.na(train_1$Fare)),]
# missmap(train_1)
# 
# ### Impute age & fare using most common method ----------------------
# age_2 <- impute(train$Age, mean)
# fare_2 <- impute(train$Fare, mean)
# train_2 <- train
# train_2$Age <- age_2
# train_2$Fare <- fare_2
# missmap(train_2)
# 
# ### Impute age & fare using knn --------------------------------------
# require(DMwR)
# train_3 <- knnImputation(train, k = 2, meth = "median")
# missmap(train_3)
# 
# ### Impute age & fare using weighted knn -----------------------------
# require(DMwR)
# train_4 <- knnImputation(train, k = 2, meth = "weighAvg")
# missmap(train_4)
# 
# ### Impute age & fare using random forest ---------------------------
# require(missForest)
# train_5 <- missForest(train)
# train_5 <- train_5$ximp
# missmap(train_5)
# 

# require(foreign)
# write.arff(train, "titanic/train_1.arff", eol = "\n", relation = deparse(substitute(x)))
# write.arff(train_2, "titanic/train_2.arff", eol = "\n", relation = deparse(substitute(x)))
# write.arff(train_3, "titanic/train_3.arff", eol = "\n", relation = deparse(substitute(x)))
# write.arff(train_4, "titanic/train_4.arff", eol = "\n", relation = deparse(substitute(x)))
# write.arff(train_5, "titanic/train_5.arff", eol = "\n", relation = deparse(substitute(x)))

## Validation set approach --------------------------------------------------
# define training control
train$PassengerId <- NULL

library(caret)
train$Survived <- as.factor(as.character(train$Survived))
train$Pclass <- as.factor(as.character(train$Pclass))
train$Sex <- as.factor(as.character(train$Sex))
train$Embarked <- as.factor(as.character(train$Embarked))
train$Title <- as.factor(as.character(train$Title))
train$Fare <- as.factor(as.character(train$Fare))
train$SibSp <- as.factor(as.character(train$SibSp))

preProcValues <- preProcess(train, method = c("center", "scale"))
train_caret <- predict(preProcValues, train)

train_caret$Survived <- as.factor(as.character(train_caret$Survived))
train_caret$Pclass <- as.factor(as.character(train_caret$Pclass))
train_caret$Sex <- as.factor(as.character(train_caret$Sex))
train_caret$Embarked <- as.factor(as.character(train_caret$Embarked))
train_caret$Title <- as.factor(as.character(train_caret$Title))
train_caret$Fare <- as.factor(as.character(train_caret$Fare))
train_caret$SibSp <- as.factor(as.character(train_caret$SibSp))

levels(train_caret$Survived) <- make.names(levels(factor(train_caret$Survived)))
levels(train_caret$Pclass) <- make.names(levels(factor(train_caret$Pclass)))
levels(train_caret$Sex) <- make.names(levels(factor(train_caret$Sex)))
levels(train_caret$SibSp) <- make.names(levels(factor(train_caret$SibSp)))
levels(train_caret$Parch) <- make.names(levels(factor(train_caret$Parch)))
levels(train_caret$Embarked) <- make.names(levels(factor(train_caret$Embarked)))
# levels(train_caret$Fare) <- make.names(levels(factor(train_caret$Fare)))

train_caret$Survived <- factor(train_caret$Survived)
dmy <- dummyVars("~ .", data = train_caret)
train_dummy <- data.frame(predict(dmy, newdata = train_caret))

train_dummy$Survived.X0 <- NULL
train_dummy$Survived.X1 <- NULL
train_dummy["Survived"] <- train$Survived
levels(train_dummy$Survived) <- make.names(levels(factor(train_dummy$Survived)))

train_control <- trainControl(method="cv", number=10)

require(foreign)
write.arff(train_dummy, "titanic/train_dummy.arff", eol = "\n", relation = deparse(substitute(x)))

# fix the parameters of the algorithm
nnet_grid <-  expand.grid(size = c(3:9), decay = (0:50)*0.02)
randomForest_grid <- expand.grid(mtry =c(1:12))
adaboost_grid <- expand.grid(iter = c(10:100), maxdepth = 2, nu = (1:10)*0.1)

# train the model
# nnet_model <- train(Survived ~ Sex.male + Sex.female + Title.Mrs 
#                     + Title.Mr + Pclass.X1 + Pclass.X2 + Pclass.X3 
#                     + Embarked.S + Embarked.Q + Embarked.C + SibSp.nosibs + SibSp.havesibs 
#                     + Age + Fare.expensive + Fare.normal + Fare.regular,
#                data=train_dummy, trControl=train_control, method="nnet",
#                verbose = TRUE, tuneGrid = nnet_grid, metric = "Accuracy")

adaboostm1_model <- train(as.factor(Survived) ~ Sex.male + Sex.female
                            + Title.Mrs + Title.Mr + Title.Master + Title.Miss + Title.Rev + Title.Don + Title.Jonkheer
                            + Pclass.X1 + Pclass.X2 + Pclass.X3 
                            + Embarked.S + Embarked.Q + Embarked.C 
                            + SibSp.nosibs + SibSp.havesibs 
                            + Age 
                            + Fare.expensive + Fare.normal + Fare.regular,
                            data=train_dummy, trControl=train_control, method="ada", 
                            verbose = TRUE, tuneGrid = adaboost_grid, metric = "Accuracy")

# randomforest_model <- train(Survived ~ Sex.male + Sex.female + Title.Mrs 
#                             + Title.Mr + Pclass.X1 + Pclass.X2 + Pclass.X3 
#                             + Embarked.S + Embarked.Q + Embarked.C + SibSp.nosibs + SibSp.havesibs 
#                             + Age + Fare.expensive + Fare.normal + Fare.regular,
#                     data=train_dummy, trControl=train_control, method="Boruta", 
#                     verbose = TRUE, tuneGrid = randomForest_grid, metric = "Accuracy")

regularied_randomforest_model <- train(Survived ~ Sex.male + Sex.female + Title.Mrs
                            + Title.Mr + Pclass.X1 + Pclass.X2 + Pclass.X3
                            + Embarked.S + Embarked.Q + Embarked.C + SibSp.nosibs + SibSp.havesibs
                            + Age + Fare.expensive + Fare.normal + Fare.regular,
                    data=train_dummy, trControl=train_control, method="Boruta",
                    verbose = TRUE, tuneGrid = randomForest_grid, metric = "Accuracy")

# summarize results
trellis.par.set(caretTheme())
ggplot(nnet_model)

# Testing against test.csv of Kaggle --------------------------------------------------
missmap(test)

## PassengerId -------------------------------------------------------------------------------------------------
test$PassengerId <- NULL


## Name -------------------------------------------------------------------------------------------------
test["Title"] <- get_title_from_name(test$Name)
test$Title <- as.factor(test$Title)
test$Name <- NULL

## Cabin -------------------------------------------------------------------------------------------------
test$Cabin <- NULL

## Ticket -------------------------------------------------------------------------------------------------
test$Ticket <- NULL

## Embarked -------------------------------------------------------------------------------------------------
missmap(test)
test$Fare[test$Fare == 0] <- NA
missmap(test)
## Age -------------------------------------------------------------------------------------------------
### Impute age & fare using  ---------------------------
# test_mice <- mice(test, m = 9999, method = 'norm.nb')
# test <- complete(test_mice, 1)

post <- mice(test, maxit = 0)$post
post["Age"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(1,70))"
post["Fare"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(1,500))"
restricted <- mice(test, m = 100, post = post, seed = 85444, method = 'norm')
test <- complete(restricted, 1)

test$Fare <- sapply(test$Fare, discretize_fare)
test$Fare <- as.factor(test$Fare)

## Parch -------------------------------------------------------------------------------------------------
parch <- data.frame(table(test$Parch, exclude = NULL))
names(parch) <- c("Parch", "Total")
head(parch)
ggplot(data = parch, aes(x = Parch, y = Total, fill = Parch)) +
  geom_bar(stat = "identity", width = .5) +
  geom_text(aes(label= Total), vjust=-1)


test$Parch <- sapply(test$Parch, discretize_parch)
test$Parch
test$Parch <- as.factor(test$Parch)


## SibSp -------------------------------------------------------------------------------------------------
test$SibSp <- sapply(test$SibSp, discretize_sibsp)
test$SibSp
test$SibSp <- as.factor(test$SibSp)

## Fare -------------------------------------------------------------------------------------------------
# train <- train[-which(train$Fare == 0),] 


require(caret)
# test$Survived <- as.factor(as.character(test$Survived))
test$Pclass <- as.factor(as.character(test$Pclass))

preProcValues <- preProcess(test, method = c("center", "scale"))
test_caret <- predict(preProcValues, test)

# levels(test_caret$Survived) <- make.names(levels(factor(test_caret$Survived)))
levels(test_caret$Pclass) <- make.names(levels(factor(test_caret$Pclass)))
levels(test_caret$Sex) <- make.names(levels(factor(test_caret$Sex)))
levels(test_caret$SibSp) <- make.names(levels(factor(test_caret$SibSp)))
levels(test_caret$Parch) <- make.names(levels(factor(test_caret$Parch)))
levels(test_caret$Embarked) <- make.names(levels(factor(test_caret$Embarked)))

dmy <- dummyVars("~ .", data = test_caret)
test_caret <- data.frame(predict(dmy, newdata = test_caret))


test_caret["Survived"] <- rep(NA, nrow(test_caret))
## Predict
# nnet_test_result <- predict(nnet_model, newdata = test_caret)
# randomforest_test_result <- predict(randomforest_model, newdata = test_caret)

test_caret$Title.Col <- NULL
# test_caret$Title.Rev <- NULL
test_caret$Title.Dona <- NULL
test_caret$Title.Don <- 0
test_caret$Title.Jonkheer <- 0

ada_test_result <- predict(adaboostm1_model, newdata = test_caret)

test_caret["PassengerId"] <- test_raw["PassengerId"]

## nnet
# test_caret["Survived"] <- nnet_test_result
# test_submit <- test_caret[c("PassengerId", "Survived")]
# write.csv(test_submit, "titanic/nnet_submission.csv", quote=F, row.names=F)

## randomforest
# test_caret["Survived"] <- randomforest_test_result
# test_submit <- test_caret[c("PassengerId", "Survived")]
# write.csv(test_submit, "titanic/randomforest_submission.csv", quote=F, row.names=F)

## ada
test_caret["Survived"] <- ada_test_result
test_submit <- test_caret[c("PassengerId", "Survived")]
write.csv(test_submit, "titanic/nnet_submission.csv", quote=F, row.names=F)

