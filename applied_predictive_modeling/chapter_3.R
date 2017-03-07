################################################################################
# LOADING AND REMOVING UNWANTED COLS
################################################################################
# Load library
library(AppliedPredictiveModeling) #data
library(e1071) #skewness
library(caret) #boxcox transformation, preProcess
library(corrplot) #corrplot

# Load data
data(segmentationOriginal)

# Separate training and testing
segData <- segmentationOriginal[segmentationOriginal$Case == "Train",]

# Remove unnecessary cols
cellID <- segData$Cell
class <- segData$Class
case <- segData$Case
segData <- segData[, -(1:3)]

# Remove status cols
statusColNum <- grep("Status", names(segData))
segData <- segData[, -statusColNum]

################################################################################
# TRANSFORMATION
################################################################################
# Check skewness of predictors
skewValues <- apply(segData, 2, skewness)

# Use preProcess func of caret package
trans <- preProcess(segData, method = c("BoxCox", "center", "scale", "pca"))
transformed <- predict(trans, segData)

################################################################################
# FILTERING
################################################################################
correlations <- cor(segData) #correlation matrix
corrplot(correlations, order = "hclust") # visualize correlation matrix
highCorr <- findCorrelation(correlations, cutoff = .75) # return indexes of cols that highly correlated
filteredSegData <- segData[, -highCorr]

################################################################################
# CREATING DUMMIES VARIABLES
################################################################################
data(cars)
carSubset <- cars[, c("Price", "Mileage", "sedan")]
head(carSubset)


data(oil)
oilType

sampNum <- floor(length(oilType) * .6) +1
set.seed(629)
oilSplits <- vector(mode = "list", length = 20)
for(i in seq(along = oilSplits)) oilSplits[[i]] <- table(sample(oilType, size = sampNum))

oilSplits <- do.call("rbind", oilSplits)
summary(oilSplits/sampNum)

oilSplits2 <- createDataPartition(oilType, p = .60, times = 20)

oilSplits2 <- lapply(oilSplits2, function(x, y) table(y[x]), y = oilType)
oilSplits2 <- do.call("rbind", oilSplits2)
summary(oilSplits2/61)





