library(caret)
library(ggplot2)
library(mlbench)
library(plotROC)

data(Sonar)

ctrl <- trainControl(method="cv", summaryFunction=twoClassSummary, classProbs=T,
                     savePredictions = T)

rfFit <- train(Class ~ ., data=Sonar, method="rf", preProc=c("center", "scale"), 
               trControl=ctrl)

# Select a parameter setting
selectedIndices <- rfFit$pred$mtry == 2

g <- ggplot(rfFit$pred[selectedIndices, ], aes(m=M, d=factor(obs, levels = c("R", "M")))) + 
  geom_roc(n.cuts=0) + 
  coord_equal() +
  style_roc()

g + annotate("text", x=0.75, y=0.25, label=paste("AUC =", round((calc_auc(g))$AUC, 4)))

# K = 10 # 10-fold
# 
# # Create 10-fold of the data
# testing_folds <- createFolds(training$Origin, k = K, list = TRUE, returnTrain = F)
# 
# data_train <- training[-testing_folds[[1]], ]
# data_test <- training[testing_folds[[1]], ]
# 
# output <- list()
# for(k in 1:K) {
#   nn <- nnet(Origin ~ ., data = data_train, size = 3, maxit= 500) # Training model
#   pred <- predict(nn, newdata = data_test) # Prediction
#   output$predictions[[k]] <- as.vector(pred)
#   output$labels[[k]] <- data_test$Origin
# }
# pred <- prediction(output$predictions, output$labels)
# perf <- performance(pred, "tpr", "fpr")
# plot(perf, avg = "vertical", spread.estimate = "boxplot", lwd = 3, lty = 3)

# auc <- roc(testing$Origin[], as.numeric(pred))
# plot(roc)
# roc_dt <- nn_fit$pred[nn_fit$pred, ]
# 
# g <- ggplot(roc_dt, aes(m = Alaskan, d = factor(obs, levels = c("Canadian", "Alaskan")))) + 
#   geom_roc(n.cuts = 20) +
#   coord_equal() +
#   style_roc()
# 
# g + annotate("text", x = 0.75, y = 0.25, label = paste("AUC =", round((calc_auc(g))$AUC, 4)))
# 
# D.ex <- rbinom(50, 1, .5)
# rocdata <- data.frame(D = c(D.ex, D.ex), 
#                       M = c(rnorm(50, mean = D.ex, sd = .4), rnorm(50, mean = D.ex, sd = 1)), 
#                       Z = c(rep("A", 50), rep("B", 50)))
# 
# ggplot(rocdata, aes(m = M, d = D), color = Z) + geom_roc()