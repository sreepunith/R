##############################################################################
## LOAD LIBRARY
##############################################################################
library(plyr)

##############################################################################
## LOAD DATA
##############################################################################
algorithms_accuracy_raw <- read.csv("diabetes-slr/csv/algorithms-accuracy.csv", na.strings = c("NA", ""))

algorithms_accuracy <- algorithms_accuracy_raw

##############################################################################
## PREPROCESS
##############################################################################
# Add IDs
# algorithms_accuracy["PaperId"] <- seq_len(nrow(algorithms_accuracy))

algorithms_accuracy <- algorithms_accuracy[which(!is.na(algorithms_accuracy$Accuracy)),]
algorithms_accuracy$Title <- NULL
algorithms_accuracy$Dataset <- NULL

algorithms_accuracy$Algorithms <- as.character(algorithms_accuracy$Algorithms)
algorithms_accuracy$Algorithms[algorithms_accuracy$Algorithms == "Ant Colony"] <- "AC"
algorithms_accuracy$Algorithms[algorithms_accuracy$Algorithms == "Artificial Neural Networks"] <- "ANN"
algorithms_accuracy$Algorithms[algorithms_accuracy$Algorithms == "Bagging"] <- "BA"
algorithms_accuracy$Algorithms[algorithms_accuracy$Algorithms == "Bayesian Network"] <- "BN"
algorithms_accuracy$Algorithms[algorithms_accuracy$Algorithms == "Boosting"] <- "BO"
algorithms_accuracy$Algorithms[algorithms_accuracy$Algorithms == "Decision Trees"] <- "DT"
algorithms_accuracy$Algorithms[algorithms_accuracy$Algorithms == "Fuzzy System"] <- "FS"
algorithms_accuracy$Algorithms[algorithms_accuracy$Algorithms == "Genetic Algorithm"] <- "GA"
algorithms_accuracy$Algorithms[algorithms_accuracy$Algorithms == "k-Nearest Neighbor"] <- "KNN"
algorithms_accuracy$Algorithms[algorithms_accuracy$Algorithms == "Logistic Regression"] <- "LR"
algorithms_accuracy$Algorithms[algorithms_accuracy$Algorithms == "Stack Generalization"] <- "SG"
algorithms_accuracy$Algorithms[algorithms_accuracy$Algorithms == "Support Vector Machine"] <- "SVM"
##############################################################################
## BOX PLOT
##############################################################################
min_a <- aggregate(Accuracy ~ Algorithms, algorithms_accuracy, FUN = max)
max_a <- aggregate(Accuracy ~ Algorithms, algorithms_accuracy, FUN = min)
mean_a <- aggregate(Accuracy ~ Algorithms, algorithms_accuracy, FUN = mean)

names(min_a) <- c("Algorithms", "Min")
names(max_a) <- c("Algorithms", "Max")
names(mean_a) <- c("Algorithms", "Mean")

min_max <- join(max_a, min_a, by = "Algorithms")
min_max <- join(min_max, mean_a, by = "Algorithms")

min_max["Group"] <- "Group 1"

  
# ggplot() + 
#   geom_pointrange(data=min_max, mapping=aes(x=Algorithms, y=Mean, ymin=Min, ymax=Max), 
#                   size=1, color="blue", fill="white", shape=22) +

ggplot(data=min_max, aes(x=Algorithms, y= Mean, colour=Algorithms)) +
  geom_errorbar(aes(x=Algorithms, y=Mean, ymin=Min, ymax=Max), width=.8) +
  geom_point(aes(y=Mean, colour=Algorithms, width = .8), size = 2) +
  geom_line(aes(group=Group, colour = "Yellow"), linetype = 2, size = .3) +
  xlab("Algorithm") +
  ylab("Accuracy (%)") +
  scale_fill_discrete(name="") +
  theme(legend.position="none")














