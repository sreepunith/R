set.seed(123)

# Split data
inTraining <- createDataPartition(dt$Tonnage, p = .75, list = FALSE)
training <- dt[inTraining,]
testing  <- dt[-inTraining,]

# Train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 1)

# Model tuning
ada_bag_grid <-  expand.grid(mfinal = 30, maxdepth = c(5, 6, 7, 8, 9, 10))

# Model
ada_bag_model <- train(Productivity ~ ., 
                       data = training, 
                       method = "AdaBag",
                       trControl = fitControl,
                       verbose = TRUE, 
                       tuneGrid = ada_bag_grid,
                       metric = "Accuracy")
ada_bag_model
plot(ada_bag_model)

# Testing
pred <- predict(ada_bag_model, newdata = testing)
boosting_error <- mean(pred != testing$Productivity)
print(paste("Accuracy ", 1 - boosting_error))