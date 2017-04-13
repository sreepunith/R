# PURPOSE: This is an experiment of comparing 2 classifiers on a single domain
# DATASET: Wisconsin Breast Cancer Database
################################################################################
# Loading library
################################################################################
library(mlbench)
library(Amelia)
library(mice)
library(caret)
################################################################################
# Loading data
################################################################################
data(BreastCancer)
dt_raw <- BreastCancer
dt <- dt_raw

################################################################################
# Preprocessing
################################################################################
# Imputation
set.seed(123)
imp <- mice(dt[,-1], m = 1)
dt <- complete(imp, 1) # Extract the 1st version of the imputed data using complete()
missmap(dt)

################################################################################
# Modeling
################################################################################
set.seed(123)

# Split data
inTraining <- createDataPartition(dt$Class, p = .75, list = FALSE)
training <- dt[inTraining,]
testing  <- dt[-inTraining,]

# Train control
fitControl <- trainControl(method="cv", number = 10,
                           classProbs=T, savePredictions = T)

# Model
cart_fit <- train(Class ~ ., 
                data = training,
                method = "rpart",
                trControl = fitControl)
plot(cart_fit)

################################################################################
# Testing
################################################################################
pred <- predict(cart_fit, newdata = testing)
error <- mean(pred != testing$Class)
print(paste("Accuracy ", 1 - error))
