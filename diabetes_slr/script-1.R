##############################################################################
## LOAD LIBRARY
##############################################################################
library(plyr)

##############################################################################
## LOAD DATA
##############################################################################
papers_general_raw <- read.csv("diabetes-slr/csv/papers-general.csv", na.strings = c("NA", ""))

papers_general <- papers_general_raw

##############################################################################
## PREPROCESS
##############################################################################
# Add IDs
papers_general["PaperId"] <- seq_len(nrow(papers_general))

# Remove Medicine papers
papers_general <- papers_general[-c(25, 28),]

##############################################################################
## PIE CHART
##############################################################################
validation_methods <- count(papers_general$ValidationMethods)
names(validation_methods) <- c("ValidationMethods", "Frequency")
validation_methods["Percentage"] <- validation_methods$Frequency / sum(validation_methods$Frequency)

label <- paste0(round(validation_methods$Percentage * 100, 2),"%")

ggplot(validation_methods, aes(x="", y= Percentage, fill=ValidationMethods))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(x = 1.3, y = Percentage/2 + c(0, cumsum(Percentage)[-length(Percentage)]), label = label), size=5) +
  theme(legend.position="bottom") +
  ylab("") +
  xlab("") +
  scale_fill_discrete(name="")

##############################################################################
## PIE CHART
##############################################################################
c <- papers_general[which(papers_general$ValidationMethods == "Leave-One-Out"),]
c$Title


