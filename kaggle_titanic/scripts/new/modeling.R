# Description: the goal of this script is to buid classifiers for predicting 
# survivors of Titanic accident by using 3 learning algorithms Neural Networks 
# (NN), CART, Random Forest (RF)
# Algorithms: NN, CART, RF
# Evaluation method: Accuracy, ROC, Statistical tests (Wilcoxon's signed-rank)
################################################################################
# Load libraries
################################################################################
library(VIM) #aggr
library(mice) #missing
library(caret)
library(corrplot) #corrplot
library(pROC)

set.seed(123)
################################################################################
# Load dataset
################################################################################
train_raw <- read.csv("data/train.csv", na.strings = c("NA", ""))
train <- train_raw

################################################################################
# Preprocessing
################################################################################
# Identify missing values
# Fare == 0 doesnt make sense, thus should be missing values
train$Fare[which(train$Fare == 0)] <- NA

# More than 70% of Cabin is missing, thus we have to remove
train$Cabin <- NULL

# Ticket makes no sense
train$Ticket <- NULL

# Observe the missing data again
missing_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))

# Impute Embarked
train$Embarked[which(is.na(train$Embarked))] <- "S"
missing_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern"))

# Extract Title from Name
get_title_from_name <- function(x) {
  index_of_comma <- regexpr("\\, [A-Z][a-z]+\\.", x)
  index_of_dot <- regexpr("\\. ", x)
  return (substr(x, index_of_comma + 2, index_of_dot - 1))
}

train["Title"] <- get_title_from_name(train$Name)
train$Title <- as.factor(train$Title)
summary(train$Title)

# Impute age & fare
imp <- mice(train[, -c(1, 2, 4)], maxit = 20, m = 6)
train_imp <- complete(imp, "long", inc = TRUE)

col <- rep(c("blue", "red")[1+as.numeric(is.na(imp$data$Age))], 6)
stripplot(Age~.imp, data = train_imp, jit = TRUE, fac = 0.8, col = col, 
          xlab = "Imputation Number") #explore the distribution of each imputed version

train <- complete(imp, 1) # select the 1st imputed version

missing_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data","Pattern")) #observe missing data again

# Too many factors in Title, we need to merge them
train$Title <- as.character(train$Title) 
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

train["Survived"] <- train_raw$Survived #add reponse variable back after imputation
train$Survived <- as.factor(train$Survived)

# Correlation checking
correlations <- cor(train[, c("Age", "SibSp", "Parch", "Fare")])
corrplot(correlations, order = "hclust")

# Scale and center
train$Fare <- scale(train$Fare, scale = T, center = T)
train$Age <- scale(train$Age, scale = T, center = T)

# Data type checking finally
levels <- unique(train$Survived)
train$Survived <- factor(as.character(train$Survived), labels = make.names(levels))
summary(train)
################################################################################
# Building models
################################################################################
# Split data
inTraining <- createDataPartition(train$Survived, p = .75, list = FALSE)
training <- train[inTraining,]
testing  <- train[-inTraining,]

# Train control
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 5,
                           classProbs = T, savePredictions = T)

# Model of NN
nn_grid <-  expand.grid(size = c(4, 5, 6, 7, 8, 9, 10))
nn_fit <- train(Survived ~ ., 
                data = training,
                method = "mlp",
                tuneGrid = nn_grid,
                trControl = fitControl)

# Model of CART
cart_grid <-  expand.grid(cp = seq(1,10, 2)*0.1)
cart_fit <- train(Survived ~ .,
                  data = training,
                  method = "rpart",
                  tuneGrid = cart_grid,
                  trControl = fitControl)

# Model
rf_grid <-  expand.grid(mtry = seq(1, 7, 1))
rf_fit <- train(Survived ~ .,
               data = training, 
               method = "parRF",
               trControl = fitControl,
               tuneGrid = rf_grid)

plot(cart_fit)
plot(nn_fit)
plot(rf_fit)
################################################################################
# Measure performance using ROC
################################################################################
# Extract the best size of nn & best cp of cart
best_size <- nn_fit$bestTune$size
best_cp <- cart_fit$bestTune$cp
best_mtry <- rf_fit$bestTune$mtry

# Extracting all fold results according to best size and best cp
nn_fold_result <- nn_fit$pred[nn_fit$pred$size == best_size, ]
cart_fold_result <- cart_fit$pred[cart_fit$pred$cp == best_cp, ]
rf_fold_result <- rf_fit$pred[rf_fit$pred$mtry == best_mtry, ]

rocobj_1 <- roc(obs ~ X1, nn_fold_result, ret = c("tp", "fp"))
rocobj_2 <- roc(obs ~ X1, cart_fold_result, ret = c("tp", "fp"))
rocobj_3 <- roc(obs ~ X1, rf_fold_result, ret = c("tp", "fp"))
plot(rocobj_1, print.thres="best", col = "blue", print.auc=TRUE)
plot(rocobj_2, print.thres="best", col = "red", add=TRUE)
plot(rocobj_3, print.thres="best", col = "green", add=TRUE)
legend("bottomright", legend=c("NN", "CART", "RF"),
       col=c("blue", "red", "green"), lwd=2)

################################################################################
# Statistical test
################################################################################
# Function for extracting accuracy by folds of caret
accuracy_by_fold <- function(fit_fold_result) {
  accuracy <- c()
  for (fld in unique(fit_fold_result$Resample)) {
    pred_by_fold <- fit_fold_result$pred[fit_fold_result$Resample == fld]
    obs_by_fold <- fit_fold_result$obs[fit_fold_result$Resample == fld]
    err <- mean(pred_by_fold != obs_by_fold)
    accuracy <- append(accuracy, 1 - err)
  }
  return (accuracy)
}

nn_accuracy_by_fold <- accuracy_by_fold(nn_fold_result)
cart_accuracy_by_fold <- accuracy_by_fold(cart_fold_result)
rf_accuracy_by_fold <- accuracy_by_fold(rf_fold_result)

# paired t-test for NN and RF because they seems equivalent
t.test(nn_accuracy_by_fold, rf_accuracy_by_fold, paired = T)

# ANOVA test for 3 classifiers
accuracy <- c(nn_accuracy_by_fold, cart_accuracy_by_fold, rf_accuracy_by_fold)
classifier <- c(rep("NN", 10), rep("CART", 10), rep("RF", 10))
dt <- data.frame(accuracy, classifier)
anova_result <- aov(accuracy ~ classifier, data = dt)
summary(anova_result)

################################################################################
# Fit model to an entire training
################################################################################
# Train control
fit_control_1 <- trainControl(method = "none",
                           classProbs = T, savePredictions = T)

# Model of NN
grid_1 <-  expand.grid(mtry = best_mtry)
model_1 <- train(Survived ~ ., 
                data = training,
                method = "parRF",
                tuneGrid = grid_1,
                trControl = fit_control_1)

# Generate probabilities for test set
pred <- predict(model_1, newdata = testing, type = "prob")

# Use ROC to find the best cut-off point
dt <- data.frame(X1 = pred$X1, obs = testing$Survived)
rocobj <- roc(obs ~ X1, dt, ret = c("tp", "fp"))
plot(rocobj, print.thres="best", col = "blue", print.auc=TRUE) #0.210

# Testing
testing_survived <- ifelse(pred$X1 > 0.210, "X1", "X0")
error <- mean(testing_survived != testing$Survived)
print(paste("Accuracy ", 1 - error))
################################################################################
# Fit model to an entire train data
################################################################################
model_2 <- train(Survived ~ ., 
                     data = train,
                     method = "parRF",
                     tuneGrid = grid_1,
                     trControl = fit_control_1)
