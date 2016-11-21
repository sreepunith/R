##############################################################################
## LOAD LIBRARY
##############################################################################
library(plyr)

##############################################################################
## LOAD DATA
##############################################################################
algorithms_raw <- read.csv("diabetes-slr/csv/algorithms.csv", na.strings = c("NA", ""))

algorithms <- algorithms_raw

##############################################################################
## PREPROCESS
##############################################################################
# Add IDs
algorithms["PaperId"] <- seq_len(nrow(algorithms))

# Remove Medicine papers
algorithms <- algorithms[-c(which(algorithms$Algorithms == "Not Applicable")),]
algorithms <- algorithms[-c(which(algorithms$Algorithms == "Cox Regression Model")),]

##############################################################################
## PIE CHART
##############################################################################
algorithms_freq <- count(algorithms$Algorithms)
names(algorithms_freq) <- c("Algorithm", "Frequency")
algorithms_freq["Percentage"] <- algorithms_freq$Frequency / sum(algorithms_freq$Frequency)

label <- paste0(round(algorithms_freq$Percentage * 100, 2),"%")

at <- nrow(algorithms_freq) - as.numeric(cumsum(sort(table(algorithms_freq)))-0.5*sort(table(algorithms_freq)))

ggplot(algorithms_freq, aes(x="", y= Percentage, fill=Algorithm))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(x = 1.3, y = Percentage/2 + c(0, cumsum(Percentage)[-length(Percentage)]), label = label), size=5) +
  theme(legend.position="bottom") +
  ylab("") +
  xlab("") +
  scale_fill_discrete(name="")
