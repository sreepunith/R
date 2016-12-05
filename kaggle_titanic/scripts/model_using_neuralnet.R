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
# Identify missing values
# Fare == 0 doesnt make sense, thus should be missing values
train$Fare[which(train$Fare == 0)] <- NA

#########################################
# CABIN
#########################################
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
post["Fare"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(1,500))"
restricted <- mice(train, m = 100, post = post, seed = 123, method = 'norm.predict')
train_temp <- complete(restricted, 1)
train <- train_temp

# Assure that no more missing
missing_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))

################################################################################
# OBSERVE DATA AFTER IMPUTATION
################################################################################

################################################################################
# CATEGORISE DATA
################################################################################


# Age
# boxplot(train$Age)
# 
# train$Age <- as.factor(findInterval(train$Age, c(21, 36)))
# 
# ct_table <- as.data.frame(table(train$Age, train$Survived))
# names(ct_table) <- c("Age", "Survived", "Quantity")
# ggplot(data = ct_table, 
#        mapping = aes(x = Survived, y = Quantity, fill = Age)) +
#   geom_bar(stat = "identity", position = "dodge")
# 
# Sibsp
train$SibSp <- as.factor(findInterval(train$SibSp, c(1, 2)))

# Parch
train$Parch <- as.factor(findInterval(train$Parch, c(1, 3)))

for(level in unique(train$Sex)){
  train[paste("Sex", level, sep = "_")] <- ifelse(train$Sex == level, 1, 0)
}
for(level in unique(train$Embarked)){
  train[paste("Embarked", level, sep = "_")] <- ifelse(train$Embarked == level, 1, 0)
}
for(level in unique(train$Pclass)){
  train[paste("Pclass", level, sep = "_")] <- ifelse(train$Pclass == level, 1, 0)
}
for(level in unique(train$Parch)){
  train[paste("Parch", level, sep = "_")] <- ifelse(train$Parch == level, 1, 0)
}
for(level in unique(train$SibSp)){
  train[paste("SibSp", level, sep = "_")] <- ifelse(train$SibSp == level, 1, 0)
}

################################################################################
# FEATURE SELECTION
################################################################################
train$Ticket <- NULL
train$Name <- NULL
str(train)

ga_ctrl <- gafsControl(functions = rfGA,
                       method = "repeatedcv",
                       repeats = 10)
# rf_ga <- gafs(x = train[, c("Age", "Fare", "Sex_male", "Sex_female",
#                             "Embarked_S", "Embarked_C", "Embarked_Q",
#                             "Pclass_3", "Pclass_1", "Pclass_2", "Parch_0",
#                             "Parch_1", "Parch_2", "SibSp_1", "SibSp_0", "SibSp_2")],
#               y = train[, c("Survived")],
#               iters = 200,
#               gafsControl = ga_ctrl)
# rf_ga <- gafs(x = train[, c("Age", "Fare", "Sex", "Embarked",
#                             "Pclass", "Parch", "SibSp")], 
#               y = train[, c("Survived")],
#               iters = 100,
#               gafsControl = ga_ctrl)
# filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 10)
# set.seed(10)
# rfWithFilter <- sbf(x = train[, c("Age", "Fare", "Sex_male", "Sex_female",
#                                   "Embarked_S", "Embarked_C", "Embarked_Q",
#                                   "Pclass_3", "Pclass_1", "Pclass_2", "Parch_0",
#                                   "Parch_1", "Parch_2", "SibSp_1", "SibSp_0", "SibSp_2")],
#                     y = train[, c("Survived")], sbfControl = filterCtrl)
# rfWithFilter

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
nn_grid <-  expand.grid(size = c(7, 8, 9, 10, 11, 12, 13, 14))

# Model
nn_fit <- train(Survived ~ Sex_female +
                  Pclass_1 + Pclass_2 + Pclass_3 +
                  Embarked_S + Embarked_C +
                  SibSp_0 + SibSp_2 +
                  Parch_0 + Parch_1 +
                  Age + Fare, 
                data = training, 
                method = "mlp",
                trControl = fitControl,
                verbose = TRUE, 
                tuneGrid = nn_grid,
                metric = "Accuracy")
nn_fit
plot(nn_fit)

importance <- varImp(nn_fit, scale=FALSE)
plot(importance)

# Testing
pred <- predict(nn_fit, newdata = testing)
boosting_error <- mean(pred != testing$Survived)
print(paste("Accuracy ", 1 - boosting_error))

