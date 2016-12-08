################################################################################
# IMPORT BUILT ADABAG MODEL
################################################################################
source("scripts/model_using_randomforest.R")

################################################################################
# LOAD DATA
################################################################################
test_raw <- read.csv("data/test.csv", na.strings = c("NA", ""))

# Backup
test <- test_raw

################################################################################
# LOAD DATA
################################################################################
str(test)

## Convert Name, Ticket, Cabin to character
indx <- c("Name", "Ticket", "Cabin")
test[indx] <- lapply(test[indx], as.character)

str(test)

################################################################################
# FEATURE ENGINEER
################################################################################
#########################################
# LASTNAME & FIRSTNAME & TITLE
#########################################
name <- strsplit(test$Name, split='[,.]')

# First name
test$FName <- sapply(c(1:length(name)), function(x, name) 
  return (trimws(name[[x]][1], which = "both")), name = name)

# Title
test$Title <- sapply(c(1:length(name)), function(x, name) 
  return (trimws(name[[x]][2], which = "both")), name = name)

# Last name
lname <- sapply(c(1:length(name)), function(x, name) 
  return (trimws(name[[x]][3], which = "both")), name = name)

lname_splt <- strsplit(lname, split ='[()]')

test$LName <- sapply(c(1:length(lname_splt)), function(x, lname_splt) 
  return (trimws(lname_splt[[x]][1], which = "both")), lname_splt = lname_splt)

# Some don't have lastname, need to impute
lname_imp <- sapply(c(1:length(lname_splt)), function(x, lname_splt) 
  return (trimws(lname_splt[[x]][2], which = "both")), lname_splt = lname_splt)
lname_missing_idx <- which(test$LName == "")
test$LName[lname_missing_idx] <- lname_imp[lname_missing_idx]

test$LName <- as.factor(test$LName)
summary(test$LName)
#########################################
# IDENTIFY FAMILY
#########################################
test$Family <- test$SibSp + test$Parch + 1
test$FamilyId <- NA
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
test$Ticket <- as.factor(test$Ticket)
summary(test$Ticket)

# Find tickets ID that used by > 1 passengers
ticket <- data.frame(table(test$Ticket))
similar_ticket <- ticket[ticket$Freq > 1, 1]

# Assign all ticket to FamilyID as suitable familyID 
# then remove ones that not in similar ticket
test$FamilyId <- test$Ticket

# Find positions that do not be removed
ticket_idx <- sapply(similar_ticket, function(x) (return (which(test$FamilyId == x))))
ticket_idx <- unlist(ticket_idx)

# Set NA to position not in idx
test$FamilyId[-ticket_idx] <- NA

##############
# Grouping by similar last name, pclass, embarked
###############
# Check that most of people who have same last name embark at the same location
# Only Bertha, James, Martin
tbl <- table(test$LName, test$Embarked)
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

tbl <- table(test$LName, test$Pclass)
df <- as.data.frame.matrix(tbl)
a <- which(df["1"] > 0) 
b <- which(df["2"] > 0) 
c <- which(df["3"] > 0)
Reduce(intersect, list(a,b,c))
df[Reduce(intersect, list(a,b,c)),]
intersect(a, b)

# Assign the same familyID to people who have the same lname by using 
# the pattern lname_embark_pclass
lname <- data.frame(table(test$LName))
similar_lname <- lname[lname$Freq > 1, 1]

lname_idx <- sapply(similar_lname, function(x) (return (which(test$LName == x))))
lname_idx <- unlist(lname_idx)
diff <- setdiff(lname_idx, ticket_idx)

test$FamilyId <- as.character(test$FamilyId)
test$FamilyId[diff] <- sapply(test$LName[diff], function (x) return (gsub("[[:space:]]", "", x)))

##############
# Update Family(size) by FamilyID
###############
# People who have family ID but FamilySize = 1
idx <- intersect(which(!is.na(test$FamilyId)), which(test$Family == 1))
freq <- data.frame(table(test$FamilyId[idx]))
names(freq) <- c("FamilyId", "Freq")
freq$FamilyId <- as.character(freq$FamilyId)
test$FamilyId[which(is.na(test$FamilyId))] <- ""

for (i in 1:nrow(freq)) {
  test$Family[which(test$FamilyId == freq$FamilyId[i])] <- freq$Fre[i]
}

##############
# TITLE
###############
test$Title <- as.factor(test$Title)
summary(test$Title)
test$Title <- as.character(test$Title)
# Too many factors in Title, we need to merge them
test$Title[which(test$Title == "Col")] <- "Mr"

test$Title[which(test$Title == "Dr")] <- "Mr"
test$Title[which(test$Title == "Rev")] <- "Mr"

test$Title[which(test$Title == "Dona")] <- "Mrs"

test$Title[which(test$Title == "Ms")] <- "Miss"
test$Title[which(test$Title == "Mlle")] <- "Miss"

test$Title <- as.factor(test$Title) 
summary(test$Title)

################################################################################
# MISSING DATA HANDLING
################################################################################
# Identify missing values
# Fare == 0 doesnt make sense, thus should be missing values
test$Fare[which(test$Fare == 0)] <- NA

#########################################
# CABIN
#########################################
# More than 70% of Cabin is missing, thus we have to remove
test$Cabin <- NULL

# Observe the missing data again
missing_plot <- aggr(test, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(test), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))

#########################################
# EMBARKED
#########################################
# Impute Embarked
test$Embarked[which(is.na(test$Embarked))] <- "S"

#########################################
# AGE
#########################################
# 19% of Age is missing. We'll find a way to impute the data
temp_test <- test[, c("Sex", "Age", "Title", "Fare")]
# Impute age
post <- mice(temp_test[, ], maxit = 0)$post
post["Age"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(1,80))"
post["Fare"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(1,500))"
post
restricted <- mice(temp_test, m = 200, post = post, seed = 345, method = 'norm.predict')
test_temp <- complete(restricted, 1)
test$Age <- test_temp$Age
test$Fare <- test_temp$Fare

################################################################################
# OBSERVE DATA AFTER IMPUTATION
################################################################################
# Assure that no more missing
missing_plot <- aggr(test, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(test), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))


################################################################################
# CATEGORISE DATA & CREATE DUMMY VARIABLES FOR NOMINAL DATA
################################################################################
# Pclass 
for(level in unique(test$Pclass)){
  test[paste("Pclass", level, sep = "_")] <- ifelse(test$Pclass == level, 1, 0)
}

# Embarked
for(level in unique(test$Embarked)){
  test[paste("Embarked", level, sep = "_")] <- ifelse(test$Embarked == level, 1, 0)
}

# Sibsp
test$SibSp <- as.factor(findInterval(test$SibSp, c(1, 2)))

for(level in unique(test$SibSp)){
  test[paste("SibSp", level, sep = "_")] <- ifelse(test$SibSp == level, 1, 0)
}

# Parch
test$Parch <- as.factor(findInterval(test$Parch, c(1, 3)))

for(level in unique(test$Parch)){
  test[paste("Parch", level, sep = "_")] <- ifelse(test$Parch == level, 1, 0)
}

# Sex
for(level in unique(test$Sex)){
  test[paste("Sex", level, sep = "_")] <- ifelse(test$Sex == level, 1, 0)
}

# Fare
test$Fare_factor <- as.factor(findInterval(test$Fare, c(32)))

for(level in unique(test$Fare_factor)){
  test[paste("Fare_factor", level, sep = "_")] <- ifelse(test$Fare_factor == level, 1, 0)
}

# Age
test$Age_factor <- as.factor(findInterval(test$Age, c(21.1, 37, 60)))

for(level in unique(test$Age_factor)){
  test[paste("Age_factor", level, sep = "_")] <- ifelse(test$Age_factor == level, 1, 0)
}

# FamilySize
test$FamilySize <- as.factor(findInterval(test$Family, c(2, 5)))
test$FamilySize <- as.factor(test$FamilySize)
################################################################################
# STANDARDIZED DATA
################################################################################
standardized_0_1 <- function(x) {
  return (x - min(x)/max(x) - min(x))
}

# Age
test$Age_standardized <- standardized_0_1(test$Age)

# Fare
test$Fare_standardized <- standardized_0_1(test$Fare)

################################################################################
# FEATURE SELECTION
################################################################################
test$Ticket <- NULL
test$Name <- NULL
str(test)

################################################################################
# PREDICT
################################################################################
pred <- predict(model, newdata = test)

# Attach prediction results to test set
test["Survived"] <- pred

# Create submission file
test_submit <- test[c("PassengerId", "Survived")]
write.csv(test_submit, "results/randomforest_test_result.csv", 
          quote = F, row.names=F)

