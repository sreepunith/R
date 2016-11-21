##############################################################################
## LOAD LIBRARY
##############################################################################
library(plyr)

##############################################################################
## LOAD DATA
##############################################################################
metrics_raw <- read.csv("diabetes-slr/csv/evaluation-metrics.csv", na.strings = c("NA", ""))

metrics <- metrics_raw

# Remove Medicine papers
metrics <- metrics[-c(25, 28),]
##############################################################################
## LOAD DATA
##############################################################################

simpliefied <- metrics[,c("Title", "Accuracy", "Sentivity", "Specificity", "Precison", "F.Measure", "AUC", "SumOfSquareError")]

accuracy <- count(simpliefied$Accuracy)
sensitivity <- count(simpliefied$Sentivity)
specificity <- count(simpliefied$Specificity)
precision <- count(simpliefied$Precison)
fmeasure <- count(simpliefied$F.Measure)
auc <- count(simpliefied$AUC)
sse <- count(simpliefied$SumOfSquareError)


metric_count <- accuracy
names(metric_count) <- c("YN", "Accuracy")
metric_count["Sensitivity"] <- sensitivity$freq
metric_count["Specificity"] <- specificity$freq
metric_count["Precison"] <- precision$freq
metric_count["F.Measure"] <- fmeasure$freq
metric_count["ROC"] <- auc$freq
metric_count["Sum.Of.Square.Error"] <- sse$freq

library(reshape2)
metric_count_reshaped <- melt(metric_count, id.vars=1)
# metric_count_reshaped <- dcast(metric_count_reshaped, variable ~ YN)
names(metric_count_reshaped) <- c("YN", "Metric", "Value")

metric_count_reshaped <- metric_count_reshaped[which(metric_count_reshaped$YN == "Yes"),]
library(ggplot2)
ggplot(metric_count_reshaped, aes(x = factor(Metric), y = Value, fill = YN), position=position_dodge()) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_colour_manual(palette="BuPu") +
  ylab("# of Papers") +
  xlab("") +
  scale_fill_discrete(name="") +
  theme(legend.position="none") +
  scale_y_continuous(breaks = seq(0, 30, by = 5))
