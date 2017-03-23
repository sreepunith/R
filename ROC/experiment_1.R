# This is an implementation of using ROC
# Dataset: Iris
################################################################################
# LOADING LIBRARY
################################################################################
library(ROCR) # ROC
library(nnet) # Neural nets with softmax
library(caret) # Create stratified partitions for k-folds
library(rrcov) # Salmon dataset
library(ggplot2)
################################################################################
# LOADING DATA
################################################################################
data(salmon)
dt_raw <- salmon
dt <- dt_raw #backup raw data
summary(dt)

################################################################################
# DATA PREPROCESSING
################################################################################
# Change Species values to 1, 2, 3
# dt$Origin <- as.character(dt$Origin)
# dt$Origin[dt$Origin == "Alaskan"] <- A
# dt$Origin[dt$Origin == "Canadian"] <- -1
# dt$Origin <- as.factor(dt$Origin)

# Standardize
dt[, -ncol(dt)] <- scale(dt[, -ncol(dt)], center = T, scale = T)

summary(dt)
################################################################################
# DATA SPLITING
################################################################################
set.seed(123) # Set seed for experiment reproduction

N <- nrow(dt)
smpl <- sample(N)
percentage <- 0.8

# Shuffle data
dt <- dt[smpl, ]
  
# Create training and testing sets
training_size <- floor(N*percentage)
training <- dt[1:training_size, ]  
testing <- dt[(training_size+1):N, ]

# Summary
summary(training)
summary(testing)

################################################################################
# MODELING
################################################################################
set.seed(123)

# train control
fitControl <- trainControl(method="cv", classProbs=T, savePredictions = T)

# Model
nn_fit <- train(Origin ~ ., 
                data = training,
                method = "mlp",
                trControl = fitControl)
pred <- predict(nn_fit, newdata = testing)

# auc <- roc(testing$Origin[], as.numeric(pred))
# plot(roc)
roc_dt <- nn_fit$pred[nn_fit$pred, ]

g <- ggplot(roc_dt, aes(m = Alaskan, d = factor(obs, levels = c("Canadian", "Alaskan")))) + 
  geom_roc(n.cuts = 20) + 
  coord_equal() +
  style_roc()

g + annotate("text", x = 0.75, y = 0.25, label = paste("AUC =", round((calc_auc(g))$AUC, 4)))

D.ex <- rbinom(50, 1, .5)
rocdata <- data.frame(D = c(D.ex, D.ex), 
                      M = c(rnorm(50, mean = D.ex, sd = .4), rnorm(50, mean = D.ex, sd = 1)), 
                      Z = c(rep("A", 50), rep("B", 50)))

ggplot(rocdata, aes(m = M, d = D), color = Z) + geom_roc()