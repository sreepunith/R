################################################################################
# LOAD LIBRARY
################################################################################
library(VIM) #aggr
library(mice) #missing
library(caret)
################################################################################
# LOAD DATA
################################################################################
train_raw <- read.csv("data/train.csv", na.strings = c("NA", ""))

# Back up
train <- train_raw
################################################################################
# DATA TYPES
################################################################################
str(train)

train$Survived <- as.factor(train$Survived)

## Convert Name, Ticket, Cabin to character
indx <- c("Name", "Ticket", "Cabin")
train[indx] <- lapply(train[indx], as.character)

str(train)

################################################################################
# MISSING DATA HANDLING
################################################################################
# More than 70% of Cabin is missing, thus we have to remove
train$Cabin <- NULL

# Observe the missing data again
missing_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))

#########################################
# EMBARKED
#########################################
# Impute Embarked
train$Embarked[which(is.na(train$Embarked))] <- "S"

#########################################
# AGE
#########################################
# 19% of Age is missing. We'll find a way to impute the data

# Extract Title from Name
get_title_from_name <- function(x) {
  index_of_comma <- regexpr("\\, [A-Z][a-z]+\\.", x)
  index_of_dot <- regexpr("\\. ", x)
  return (substr(x, index_of_comma + 2, index_of_dot - 1))
}

train["Title"] <- get_title_from_name(train$Name)
train$Title <- as.character(train$Title) 
summary(train$Title)

# Too many factors in Title, we need to merge them
train$Title[which(train$Title == "Capt")] <- "Miltary"
train$Title[which(train$Title == "Col")] <- "Miltary"
train$Title[which(train$Title == "Major")] <- "Miltary"

train$Title[which(train$Title == "Don")] <- "Men_Honorable"
train$Title[which(train$Title == "Dr")] <- "Men_Honorable"
train$Title[which(train$Title == "Jonkheer")] <- "Men_Honorable"
train$Title[which(train$Title == "Rev")] <- "Men_Honorable"
train$Title[which(train$Title == "Sir")] <- "Men_Honorable"

train$Title[which(train$Title == "Mme")] <- "Mr"

train$Title[which(train$Title == "Lady")] <- "Women_Honorable"
train$Title[which(train$Title == "Rothes, the Countess")] <- "Women_Honorable"

train$Title[which(train$Title == "Ms")] <- "Miss"
train$Title[which(train$Title == "Mlle")] <- "Miss"

train$Title <- as.factor(train$Title) 
summary(train$Title)

# Impute age
post <- mice(train[, ], maxit = 0)$post
post["Age"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(1,80))"
restricted <- mice(train, m = 100, post = post, seed = 123, method = 'norm')
train_temp <- complete(restricted, 1)
train <- train_temp

# Assure that no more missing
missing_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))
################################################################################
# FEATURE SELECTION
################################################################################
train$Ticket <- NULL
train$Name <- NULL
str(train)

################################################################################
# BUIDING MODEL
################################################################################
set.seed(123)

# Split data
inTraining <- createDataPartition(train$Survived, p = .75, list = FALSE)
training <- train[inTraining,]
testing  <- train[-inTraining,]

# train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# Neural network model tuning
nn_grid <-  expand.grid(size = c(5,7,8,9,10))

# Model
nn_fit <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, 
                data = training, 
                method = "mlp",
                trControl = fitControl,
                verbose = FALSE, 
                tuneGrid = nn_grid,
                metric = "ROC")
nn_fit
plot(nn_fit)

# Testing
pred <- predict(nn_fit, newdata = testing)
boosting_error <- mean(pred != testing$Survived)
print(paste("Accuracy ", 1 - boosting_error))
