################################################################################
# LOAD LIBRARY
################################################################################
library(VIM) #aggr
library(Amelia) #missmap
library(mice)
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

indx <- c("MSSubClass", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd",
          "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath", "BedroomAbvGr",
          "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces")
train[indx] <- lapply(train[indx], as.character)

################################################################################
### OBSERVE DATA
################################################################################
# Missing data
aggr(train, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))

missmap(train)

# Temporarily remove features with missing values
idx <- apply(train, 2, function(x) any(is.na(x))) #Check NAs by columns
train <- train[, which(idx == F)] #Remove columns has NAs

# Check again
missmap(train)
################################################################################
### IMPUTATION
################################################################################
pairs(train, pch = 21)
