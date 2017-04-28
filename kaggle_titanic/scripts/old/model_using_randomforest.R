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
# FEATURE ENGINEER
################################################################################
#########################################
# LASTNAME & FIRSTNAME & TITLE
#########################################
name <- strsplit(train$Name, split='[,.]')

# First name
train$FName <- sapply(c(1:length(name)), function(x, name) 
  return (trimws(name[[x]][1], which = "both")), name = name)

# Title
train$Title <- sapply(c(1:length(name)), function(x, name) 
  return (trimws(name[[x]][2], which = "both")), name = name)

# Last name
lname <- sapply(c(1:length(name)), function(x, name) 
  return (trimws(name[[x]][3], which = "both")), name = name)

lname_splt <- strsplit(lname, split ='[()]')

train$LName <- sapply(c(1:length(lname_splt)), function(x, lname_splt) 
  return (trimws(lname_splt[[x]][1], which = "both")), lname_splt = lname_splt)

# Some don't have lastname, need to impute
lname_imp <- sapply(c(1:length(lname_splt)), function(x, lname_splt) 
  return (trimws(lname_splt[[x]][2], which = "both")), lname_splt = lname_splt)
lname_missing_idx <- which(train$LName == "")
train$LName[lname_missing_idx] <- lname_imp[lname_missing_idx]

train$LName <- as.factor(train$LName)
summary(train$LName)
#########################################
# IDENTIFY FAMILY
#########################################
train$Family <- train$SibSp + train$Parch + 1
train$FamilyId <- NA
# Identify if passenger are the same family 
# Conditions: 
# (1) similar ticket pattern, same embark, same pclass
# (2) same last name, same embark, same pclass
# (3) ticket pattern, one have Sibsp or Parch > 0


##############
# Grouping by similar tickets
###############
# There are a lot of similar tickets, So if passengers have the same ticket,
# they should be a family or a least travel together
train$Ticket <- as.factor(train$Ticket)
summary(train$Ticket)

# Find tickets ID that used by > 1 passengers
ticket <- data.frame(table(train$Ticket))
similar_ticket <- ticket[ticket$Freq > 1, 1]

# Assign all ticket to FamilyID as suitable familyID 
# then remove ones that not in similar ticket
train$FamilyId <- train$Ticket

# Find positions that do not be removed
ticket_idx <- sapply(similar_ticket, function(x) (return (which(train$FamilyId == x))))
ticket_idx <- unlist(ticket_idx)

# Set NA to position not in idx
train$FamilyId[-ticket_idx] <- NA

##############
# Grouping by similar last name, pclass, embarked
###############
# Check that most of people who have same last name embark at the same location
# Only Bertha, James, Martin
tbl <- table(train$LName, train$Embarked)
df <- as.data.frame.matrix(tbl)
a <- which(df$C > 0) 
b <- which(df$Q > 0) 
c <- which(df$S > 0) 
intersect(a, b)
intersect(a, c)
intersect(b, c)

# df[Reduce(intersect, list(a,b,c)),]

# Check that ost of people who have same last name stay in the same pclass
# Except:
# Anna      1 1 1
# Bertha    1 2 1
# Elizabeth 1 1 1
# George    2 1 1
# James     1 1 7
# John      2 1 7
# Martin    1 1 2
# William   1 5 5 

tbl <- table(train$LName, train$Pclass)
df <- as.data.frame.matrix(tbl)
a <- which(df["1"] > 0) 
b <- which(df["2"] > 0) 
c <- which(df["3"] > 0)
Reduce(intersect, list(a,b,c))
df[Reduce(intersect, list(a,b,c)),]
intersect(a, b)

# Assign the same familyID to people who have the same lname by using 
# the pattern lname_embark_pclass
lname <- data.frame(table(train$LName))
similar_lname <- lname[lname$Freq > 1, 1]

lname_idx <- sapply(similar_lname, function(x) (return (which(train$LName == x))))
lname_idx <- unlist(lname_idx)
diff <- setdiff(lname_idx, ticket_idx)

train$FamilyId <- as.character(train$FamilyId)
train$FamilyId[diff] <- sapply(train$LName[diff], function (x) return (gsub("[[:space:]]", "", x)))

##############
# Update Family(size) by FamilyID
###############
# People who have family ID but FamilySize = 1
idx <- intersect(which(!is.na(train$FamilyId)), which(train$Family == 1))
freq <- data.frame(table(train$FamilyId[idx]))
names(freq) <- c("FamilyId", "Freq")
freq$FamilyId <- as.character(freq$FamilyId)
train$FamilyId[which(is.na(train$FamilyId))] <- ""

for (i in 1:nrow(freq)) {
  train$Family[which(train$FamilyId == freq$FamilyId[i])] <- freq$Fre[i]
}

##############
# TITLE
###############
train$Title <- as.factor(train$Title)
summary(train$Title)
train$Title <- as.character(train$Title)
# Too many factors in Title, we need to merge them
train$Title[which(train$Title == "Capt")] <- "Mr"
train$Title[which(train$Title == "Col")] <- "Mr"
train$Title[which(train$Title == "Major")] <- "Mr"

train$Title[which(train$Title == "Don")] <- "Mr"
train$Title[which(train$Title == "Dr")] <- "Mr"
train$Title[which(train$Title == "Jonkheer")] <- "Mr"
train$Title[which(train$Title == "Rev")] <- "Mr"
train$Title[which(train$Title == "Sir")] <- "Mr"

train$Title[which(train$Title == "Mme")] <- "Mr"

train$Title[which(train$Title == "Lady")] <- "Mrs"
train$Title[which(train$Title == "the Countess")] <- "Mrs"

train$Title[which(train$Title == "Ms")] <- "Miss"
train$Title[which(train$Title == "Mlle")] <- "Miss"

train$Title <- as.factor(train$Title) 
summary(train$Title)

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
temp_train <- train[, c("Sex", "Age", "Title", "Fare", "Pclass")]
# Impute age
post <- mice(temp_train[, ], maxit = 0)$post
post["Age"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(1,80))"
post["Fare"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(1,500))"
restricted <- mice(temp_train, m = 5, post = post, seed = 567, method = 'norm')
train_temp <- complete(restricted, 1)
train$Age <- train_temp$Age
train$Fare <- train_temp$Fare

################################################################################
# OBSERVE DATA AFTER IMPUTATION
################################################################################
# Assure that no more missing
missing_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))


################################################################################
# CATEGORISE DATA & CREATE DUMMY VARIABLES FOR NOMINAL DATA
################################################################################
# Pclass 
for(level in unique(train$Pclass)){
  train[paste("Pclass", level, sep = "_")] <- ifelse(train$Pclass == level, 1, 0)
}

# Embarked
for(level in unique(train$Embarked)){
  train[paste("Embarked", level, sep = "_")] <- ifelse(train$Embarked == level, 1, 0)
}

# Sibsp
train$SibSp <- as.factor(findInterval(train$SibSp, c(1, 2)))

for(level in unique(train$SibSp)){
  train[paste("SibSp", level, sep = "_")] <- ifelse(train$SibSp == level, 1, 0)
}

# Parch
train$Parch <- as.factor(findInterval(train$Parch, c(1, 3)))

for(level in unique(train$Parch)){
  train[paste("Parch", level, sep = "_")] <- ifelse(train$Parch == level, 1, 0)
}

# Sex
for(level in unique(train$Sex)){
  train[paste("Sex", level, sep = "_")] <- ifelse(train$Sex == level, 1, 0)
}

# Fare
train$Fare_factor <- as.factor(findInterval(train$Fare, c(32)))

for(level in unique(train$Fare_factor)){
  train[paste("Fare_factor", level, sep = "_")] <- ifelse(train$Fare_factor == level, 1, 0)
}

# Age
train$Age_factor <- as.factor(findInterval(train$Age, c(21.1, 37, 60)))

for(level in unique(train$Age_factor)){
  train[paste("Age_factor", level, sep = "_")] <- ifelse(train$Age_factor == level, 1, 0)
}

# FamilySize
train$FamilySize <- as.factor(findInterval(train$Family, c(2, 5)))
for(level in unique(train$Age_factor)){
  train[paste("FamilySize", level, sep = "_")] <- ifelse(train$Age_factor == level, 1, 0)
}
################################################################################
# STANDARDIZED DATA
################################################################################
standardized_0_1 <- function(x) {
  return (x - min(x)/max(x) - min(x))
}

# Age
train$Age_standardized <- standardized_0_1(train$Age)

# Fare
train$Fare_standardized <- standardized_0_1(train$Fare)

################################################################################
# FEATURE SELECTION
################################################################################
train$Ticket <- NULL
train$Name <- NULL
str(train)

################################################################################
# BUIDING MODEL
################################################################################
set.seed(234)

# Split data
inTraining <- createDataPartition(train$Survived, p = .5, list = FALSE)
training <- train[inTraining,]
testing  <- train[-inTraining,]

# Train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  allowParallel = TRUE)

# Model tuning
grid <-  expand.grid(mtry = seq(1, 7, 1))

# Model
model <- train(Survived ~ Sex_male + 
                 Pclass_3 + Pclass_1 + Embarked +
                 Age + 
                 Fare + FamilySize,
               data = training, 
               method = "parRF",
               trControl = fitControl,
               verbose = TRUE, 
               tuneGrid = grid,
               preProc = c("center", "scale"),
               metric = "Kappa")
model

# Plot model performance
plot(model)

# Check feature importances
varImp(model)

# Testing
pred <- predict(model, newdata = testing)
error <- mean(pred != testing$Survived)
print(paste("Accuracy ", 1 - error))

