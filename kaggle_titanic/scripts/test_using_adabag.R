################################################################################
# IMPORT BUILT ADABAG MODEL
################################################################################
source("scripts/model_using_adabag.R")

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
# MISSING DATA HANDLING
################################################################################
#########################################
# CABIN
#########################################
test$Cabin <- NULL

missing_plot <- aggr(test, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(test), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))

#########################################
# AGE & FARE
#########################################
# Extract Title from Name
get_title_from_name <- function(x) {
  index_of_comma <- regexpr("\\, [A-Z][a-z]+\\.", x)
  index_of_dot <- regexpr("\\. ", x)
  return (substr(x, index_of_comma + 2, index_of_dot - 1))
}

test["Title"] <- get_title_from_name(test$Name)
summary(as.factor(test$Title))

# Too many factors in Title, we need to merge them
test$Title[which(test$Title == "Col")] <- "Miltary"

test$Title[which(test$Title == "Dr")] <- "Men_Honorable"
test$Title[which(test$Title == "Rev")] <- "Men_Honorable"

test$Title[which(test$Title == "Dona")] <- "Women_Honorable"

test$Title[which(test$Title == "Ms")] <- "Miss"
test$Title[which(test$Title == "Mlle")] <- "Miss"

test$Title <- as.factor(test$Title) 
summary(test$Title)

# Impute age & fare
post <- mice(test[, ], maxit = 0)$post
post["Age"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(1,80))"
post["Fare"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(1,500))"
restricted <- mice(test, m = 100, post = post, seed = 123, method = 'norm.predict')
test_temp <- complete(restricted, 1)
test <- test_temp

################################################################################
# OBSERVE DATA AFTER IMPUTATION
################################################################################
missing_plot <- aggr(test, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(test), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))
################################################################################
# CATEGORISE DATA
################################################################################
# Sibsp
test$SibSp <- as.factor(findInterval(test$SibSp, c(1, 2)))

# Parch
test$Parch <- as.factor(findInterval(test$Parch, c(1, 3)))

for(level in unique(test$Sex)){
  test[paste("Sex", level, sep = "_")] <- ifelse(test$Sex == level, 1, 0)
}
for(level in unique(test$Embarked)){
  test[paste("Embarked", level, sep = "_")] <- ifelse(test$Embarked == level, 1, 0)
}
for(level in unique(test$Pclass)){
  test[paste("Pclass", level, sep = "_")] <- ifelse(test$Pclass == level, 1, 0)
}
for(level in unique(test$Parch)){
  test[paste("Parch", level, sep = "_")] <- ifelse(test$Parch == level, 1, 0)
}
for(level in unique(test$SibSp)){
  test[paste("SibSp", level, sep = "_")] <- ifelse(test$SibSp == level, 1, 0)
}
################################################################################
# FEATURE SELECTION
################################################################################
test$Ticket <- NULL
test$Name <- NULL
str(test)

pred <- predict(ada_bag_model, newdata = test)

# Attach prediction results to test set
test["Survived"] <- pred

test_submit <- test[c("PassengerId", "Survived")]
write.csv(test_submit, "data/adabag_test_result.csv", quote=F, row.names=F)

