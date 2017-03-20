################################################################################
# LOAD LIBRARY
################################################################################
library(anytime)
library(caret)
library(Amelia)
library(corrplot)
################################################################################
# LOAD DATA
################################################################################
dt_raw <- read.csv("data/Machine_Ordered.csv")
dt <- dt_raw

summary(dt)

################################################################################
# PREPROCESS DATA
################################################################################
dt <- dt[, c("ThrottlePosition___", "EngineSpeed_rpm_", 
             "DesiredEngineSpeed_rpm_", "BoostPressure_kPa_", 
             "EngineLoadFactor___", "FuelConsumptionRate_l_h_",
             "GroundSpeed_mph_", "TransmissionOutputSpeed_rpm_",
             "FrontServiceBrakeOilPressure_kPa_", "RearServiceBrakeOilPressure_kPa_",
             "HydraulicOilTemperature_degC_", "HydraulicOilTemperature_degC_",
             "FuelGauge___", "SystemVoltage_volts_", "Tonnage")]

dt$Productivity <- as.factor(findInterval(dt$Tonnage, c(17, 20)))
levels(dt$Productivity) <- c("Low", "Normal", "High")

summary(dt$Productivity)

# dt$Tonnage <- NULL

low <- dt[dt$Productivity == "Low",]
normal <- dt[dt$Productivity == "Normal",]
high <- dt[dt$Productivity == "High",]

dt$Productivity <- NULL
################################################################################
# CLEANING
################################################################################
# Identifying correlations between predictors
correlations <- cor(dt[1:14])
# dim(correlations)
# correlations[1:14, 1:14]
corrplot(correlations, order = "hclust")

highCorr <- findCorrelation(correlations, cutoff = .75)
length(highCorr)
dt <- dt[, -highCorr]

# Remove outlier
dt <- dt[-which(dt$HydraulicOilTemperature_degC_.1 < 1),]
# dt <- dt[-which(dt$FrontServiceBrakeOilPressure_kPa_ >1000),]

################################################################################
# SPLITTING THE DATA
################################################################################
# Split data into training and testing
N <- nrow(dt)
training_percent <- 0.8
smpl <- sample(1:N, N) 

dt <- dt[smpl,]

training_size <- N*training_percent
testing_size <- N - training_size

# Training
training_dt <- dt[1:training_size, ]

# Testing
holdout <- dt[(training_size+1):N,]

################################################################################
# BUIDING MODEL
################################################################################
set.seed(123)

# Split data
inTraining <- createDataPartition(training_dt$Tonnage, p = .5, list = FALSE)
training <- training_dt[inTraining,]
testing  <- training_dt[-inTraining,]

# Train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  repeats = 1)

# Model tuning
grid <-  expand.grid(size = c(7, 8, 9, 10))

# Model
model <- train(Tonnage ~ .,
               data = training, 
               method = "mlp",
               trControl = fitControl,
               verbose = TRUE, 
               tuneGrid = grid,
               preProc = c("center", "scale"))
model

# Plot model performance
plot(model)

# Check feature importances
varImp(model)

# Testing
pred <- predict(model, newdata = testing[,-ncol(testing)])
error <- mean(pred != testing$Productivity)
print(paste("Accuracy ", 1 - error))

################################################################################
# SHIFT DISTRIBUTION
################################################################################
newlow <- low
newlow$FuelGauge___ <- newlow$FuelGauge___ - 0.001

# New tonnage values
predicted_tonnage <- predict(model, newdata = newlow[,-ncol(newlow)])
old_tonnage <- low$Tonnage
improved_percentage <- (sum(predicted_tonnage) - sum(old_tonnage))/sum(old_tonnage)
improved_percentage


ggplot() + aes(tonnage)+ geom_histogram(binwidth=1, colour="black", fill = "blue")
ggsave("figs/tonnage_original.png")
ggplot() + aes(low$Tonnage)+ geom_histogram(binwidth=1, colour="black", fill = "blue")
ggsave("figs/tonnage_new.png")



