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
# Dedicate this part to deal to edit data types of variables before doing anything
################################################################################
str(train)

# Alley: Type of alley access to property
train$Alley <- as.character(train$Alley)
train$Alley[which(is.na(train$Alley))] <- "NoAlley" 
train$Alley <- as.factor(train$Alley)
summary(train$Alley)

# OverallQual: Rates the overall material and finish of the house
# It should be treated as ORDINAL data type
train$OverallQual <- ordered(train$OverallQual, 
                             levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

# OverallCond: Rates the overall condition of the house
# It should be treated as ORDINAL data type
train$OverallCond <- ordered(train$OverallCond, 
                             levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

# YearBuilt: Original construction date
# Will treat YearBuilt as the NOMINAL data type
# train$YearBuilt <- factor(train$YearBuilt)

# YearRemodAdd: Remodel date (same as construction date if no remodeling or additions)
# Will treat YearBuilt as the NOMINAL data type
train$YearRemodAdd <- factor(train$YearRemodAdd)

# ExterQual: Evaluates the quality of the material on the exterior
# Will treat ExterQual as ORDINAL data type
train$ExterQual <- ordered(train$ExterQual,
                           levels = c("Po", "Fa", "TA", "Gd", "Ex"))

# ExterCond: Evaluates the present condition of the material on the exterior
# Will treat ExterCond as ORDINAL data type
train$ExterCond <- ordered(train$ExterCond,
                           levels = c("Po", "Fa", "TA", "Gd", "Ex"))

# BsmtQual: Evaluates the height of the basement
# Ordinal
train$BsmtQual <- as.character(train$BsmtQual)
train$BsmtQual[which(is.na(train$BsmtQual))] <- "NoBasement"
train$BsmtQual <- ordered(train$BsmtQual, 
                          levels = c("NoBasement", "Po", "Fa", "TA", "Gd", "Ex"))
summary(train$BsmtQual)

# BsmtCond: Evaluates the general condition of the basement
# Ordinal
train$BsmtCond <- as.character(train$BsmtCond)
train$BsmtCond[which(is.na(train$BsmtCond))] <- "NoBasement" 
train$BsmtCond <- ordered(train$BsmtCond,
                          levels = c("NoBasement", "Po", "Fa", "TA", "Gd", "Ex"))
summary(train$BsmtQual)


# BsmtExposure: Refers to walkout or garden level walls
# Ordinal
train$BsmtExposure <- as.character(train$BsmtExposure)
train$BsmtExposure[which(is.na(train$BsmtExposure))] <- "NoBasement" 
train$BsmtExposure <- ordered(train$BsmtExposure,
                              levels = c("NoBasement", "No", "Mn", "Av", "Gd"))
summary(train$BsmtExposure)

# BsmtFinType1: Rating of basement finished area
train$BsmtFinType1 <- as.character(train$BsmtFinType1)
train$BsmtFinType1[which(is.na(train$BsmtFinType1))] <- "NoBasement" 
train$BsmtFinType1 <- ordered(train$BsmtFinType1,
                              levels = c("NoBasement", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
summary(train$BsmtFinType1)

# BsmtFinType2: Rating of basement finished area (if multiple types)
train$BsmtFinType2 <- as.character(train$BsmtFinType2)
train$BsmtFinType2[which(is.na(train$BsmtFinType2))] <- "NoBasement" 
train$BsmtFinType2 <- ordered(train$BsmtFinType2,
                              levels = c("NoBasement", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
summary(train$BsmtFinType2)

# HeatingQC: Heating quality and condition
train$HeatingQC <- ordered(train$HeatingQC,
                           levels = c("Po", "Fa", "TA", "Gd", "Ex"))

# KitchenQual: Kitchen quality
train$KitchenQual <- ordered(train$KitchenQual,
                             levels = c("Po", "Fa", "TA", "Gd", "Ex"))

# Functional: Home functionality (Assume typical unless deductions are warranted)
train$Functional <- ordered(train$Functional,
                            levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"))

# FireplaceQu: Fireplace quality
train$FireplaceQu <- as.character(train$FireplaceQu)
train$FireplaceQu[which(is.na(train$FireplaceQu))] <- "NoFireplace" 
train$FireplaceQu <- ordered(train$FireplaceQu,
                             levels = c("NoFireplace", "Po", "Fa", "TA", "Gd", "Ex"))
summary(train$FireplaceQu)


# GarageFinish: Interior finish of the garage
train$GarageFinish <- as.character(train$GarageFinish)
train$GarageFinish[which(is.na(train$GarageFinish))] <- "NoGarage" 
train$GarageFinish <- ordered(train$GarageFinish,
                              levels = c("NoGarage", "Unf", "RFn", "Fin"))
summary(train$GarageFinish)

# GarageQual: Garage quality
train$GarageQual <- as.character(train$GarageQual)
train$GarageQual[which(is.na(train$GarageQual))] <- "NoGarage" 
train$GarageQual <- ordered(train$GarageQual,
                            levels = c("NoGarage", "Po", "Fa", "TA", "Gd", "Ex"))
summary(train$GarageQual)

# GarageCond: Garage condition
train$GarageCond <- as.character(train$GarageCond)
train$GarageCond[which(is.na(train$GarageCond))] <- "NoGarage" 
train$GarageCond <- ordered(train$GarageCond,
                            levels = c("NoGarage", "Po", "Fa", "TA", "Gd", "Ex"))
summary(train$GarageCond)

# PavedDrive: Paved driveway
train$PavedDrive <- ordered(train$PavedDrive,
                            levels = c("N", "P", "Y"))

# PoolQC: Pool quality
train$PoolQC <- as.character(train$PoolQC)
train$PoolQC[which(is.na(train$PoolQC))] <- "NoPool" 
train$PoolQC <- ordered(train$PoolQC,
                        levels = c("NoPool", "Fa", "TA", "Gd", "Ex"))
summary(train$PoolQC)

# Fence: Fence quality
train$Fence <- as.character(train$Fence)
train$Fence[which(is.na(train$Fence))] <- "NoFence" 
train$Fence <- ordered(train$Fence,
                       levels = c("NoFence", "MnWw", "GdWo", "MnPrv", "GdPrv"))
summary(train$Fence)

# MiscFeature: Miscellaneous feature not covered in other categories
train$MiscFeature <- as.character(train$MiscFeature)
train$MiscFeature[which(is.na(train$MiscFeature))] <- "NoMiscFeature" 
train$MiscFeature <- as.factor(train$MiscFeature)
summary(train$MiscFeature)

################################################################################
### OBSERVE DATA
################################################################################
# Missing data
aggr(train, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))

missmap(train)

################################################################################
### IMPUTATION
################################################################################
# Temporarily remove features with missing values
idx <- apply(train, 2, function(x) any(is.na(x))) #Check NAs by columns
train <- train[, which(idx == F)] #Remove columns has NAs

# Check again
missmap(train)



