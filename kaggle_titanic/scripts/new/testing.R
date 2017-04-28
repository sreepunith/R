# Description: This script is to preprocess the Titanic test data and generate 
# discrete prediction by using the NN classifier built from the modeling script
################################################################################
# Import the build neural net model
################################################################################
source("scripts/new/modeling.R")

################################################################################
# Load the test data
################################################################################
test_raw <- read.csv("data/test.csv", na.strings = c("NA", ""))
test <- test_raw #backup

summary(test)
################################################################################
# Preprocess
################################################################################
# Identify missing values
# Fare == 0 doesnt make sense, thus should be missing values
test$Fare[which(test$Fare == 0)] <- NA

# Remove Cabin
test$Cabin <- NULL

# Remove Ticket
test$Ticket <- NULL

# Observe the missing data
missing_plot <- aggr(test, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(test), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))

# Extract Title from Name
get_title_from_name <- function(x) {
  index_of_comma <- regexpr("\\, [A-Z][a-z]+\\.", x)
  index_of_dot <- regexpr("\\. ", x)
  return (substr(x, index_of_comma + 2, index_of_dot - 1))
}

test["Title"] <- get_title_from_name(test$Name)
test$Title <- as.factor(test$Title)
summary(test$Title)

# Impute age & fare
imp <- mice(test[, -c(1, 3)], maxit = 20, m = 6)
test_imp <- complete(imp, "long", inc = TRUE)

col <- rep(c("blue", "red")[1+as.numeric(is.na(imp$data$Age))], 6)
stripplot(Age~.imp, data = test_imp, jit = TRUE, fac = 0.8, col = col, 
          xlab = "Imputation Number") #explore the distribution of each imputed version

test <- complete(imp, 3) # select the 3rd imputed version

missing_plot <- aggr(test, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(test), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern")) #observe missing data again

# Too many factors in Title, we need to merge them
test$Title <- as.character(test$Title) 
test$Title[which(test$Title == "Col")] <- "Miltary"

test$Title[which(test$Title == "Dr")] <- "Men_Honorable"
test$Title[which(test$Title == "Rev")] <- "Men_Honorable"

test$Title[which(test$Title == "Dona")] <- "Women_Honorable"

test$Title[which(test$Title == "Ms")] <- "Miss"
test$Title[which(test$Title == "Mlle")] <- "Miss"

test$Title <- as.factor(test$Title) 
summary(test$Title)

# Scale and center
test$Fare <- scale(test$Fare, scale = T, center = T)
test$Age <- scale(test$Age, scale = T, center = T)

# Data type checking finally
summary(test)
################################################################################
# Predict
################################################################################
# Generate probabilities for test set
test_prediction <- predict(model_2, newdata = test, type = "prob")
survived <- ifelse(test_prediction$X1 > 0.210, "1", "0")
survived <- as.factor(survived)

# Attach prediction results to test set
test["Survived"] <- survived
test["PassengerId"] <- test_raw$PassengerId

# Create submission file
test_submit <- test[c("PassengerId", "Survived")]
write.csv(test_submit, "results/randomforest_test_result.csv", 
          quote = F, row.names=F)
