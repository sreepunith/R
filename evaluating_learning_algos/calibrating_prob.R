# PURPOSE: This is an experiment of calibrating probabilities
# DATASET: Iris
# ALGORITHMS: CART, NN
# ERROR ESTIMATION: 5x2-fold
# SIGNIFICANCE STAT TESTS: t-test, Wilcoxon's signed-rank test
################################################################################
# Loading library
################################################################################
library(PresenceAbsence)
library(caret)
library(AppliedPredictiveModeling)

set.seed(123)
################################################################################
# Loading data
################################################################################
simulated_train <- quadBoundaryFunc(500)
simulated_test <- quadBoundaryFunc(1000)

plot(simulated_train)

data(iris)
dt_raw <- iris
dt <- dt_raw