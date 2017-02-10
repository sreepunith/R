################################################################################
# LOAD LIBRARY
################################################################################
library(VIM) #aggr
library(ggplot2)
################################################################################
# LOAD DATA
################################################################################
train_raw <- read.csv("data/glass.csv", na.strings = c("NA", ""))

# Back up
train <- train_raw
################################################################################
# DATA TYPES
################################################################################
str(train) #Structure
head(train) #Check head of the data

train$Type <- as.factor(train$Type) #Convert reponse variable to factor
str(train) #Double check if converted

################################################################################
# MISSING VALUES
################################################################################
# Missing data
aggr(train, col=c('navyblue','red'), numbers=TRUE, 
     sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, 
     ylab=c("Histogram of missing data","Pattern"))

# No missing values
################################################################################
# DATA EXPLORATION
################################################################################
ggplot(data = train, mapping = aes(x = Na, ..density.., colour = Type)) +
  geom_histogram(binwidth = 0.5)


box_cox_trans <- function(x, lambda) {
  return ((x^lambda - 1)/lambda)
}
x <- seq(0, 20, 0.5)
lambda <- 4

xmark <- box_cox_trans(x, lambda)
dt <- data.frame(x = x, x_mark = xmark)
ggplot(data = dt, mapping = aes(x = x, y = xmark)) +
  geom_point()
