##############################################################################
## LOAD LIBRARY
##############################################################################
library(plyr)

##############################################################################
## LOAD DATA
##############################################################################
total_0_raw <- read.csv("diabetes-slr/csv/total-0.csv", na.strings = c("NA", ""))
total_1_raw <- read.csv("diabetes-slr/csv/total-1.csv", na.strings = c("NA", ""))
total_2_raw <- read.csv("diabetes-slr/csv/total-2.csv", na.strings = c("NA", ""))

total_0 <- total_0_raw
total_1 <- total_1_raw
total_2 <- total_2_raw

##############################################################################
## ADD IDs COLUMNS
##############################################################################
total_0["PaperId"] <- seq_len(nrow(total_0))
total_1["PaperId"] <- seq_len(nrow(total_1))
total_2["PaperId"] <- seq_len(nrow(total_2))

##############################################################################
## PLOT PAPER FREQUENCY BY YEAR
##############################################################################
require(plyr)
frequency_0 <- count(total_0$Year)
frequency_1 <- count(total_1$Year)
frequency_2 <- count(total_2$Year)
names(frequency_0) <- c("Year", "Frequency")
names(frequency_1) <- c("Year", "Frequency")
names(frequency_2) <- c("Year", "Frequency")

## Merging them into a single data frame
paper_step_freq <- data.frame(Year = 2000:2016)
paper_step_freq <- join(paper_step_freq, frequency_2)
paper_step_freq <- cbind(paper_step_freq, frequency_0$Frequency)
paper_step_freq <- cbind(paper_step_freq, frequency_1$Frequency)
names(paper_step_freq) <- c("Year", "Step2", "Step0", "Step1")
paper_step_freq$Step2[is.na(paper_step_freq$Step2)] <- 0

ggplot() +
  geom_area(data = paper_step_freq, aes(x = Year, y = Step0 , fill = "Step 1")) +
  geom_area(data = paper_step_freq, aes(x = Year, y = Step1, fill = "Step 2")) +
  geom_area(data = paper_step_freq, aes(x = Year, y = Step2, fill = "Step 3")) +
  xlab("Year") +
  ylab("Number of Papers") +
  scale_fill_discrete(name="") +
  scale_x_continuous(breaks = seq(2000, 2016, by = 4)) +
  scale_y_continuous(breaks = seq(0, 180, by = 20)) +
  theme(legend.position="bottom")

##############################################################################
## PLOT JOURNAL
##############################################################################
journal_freq_0 <- count(total_0$Journal)
names(journal_freq_0) <- c("Journal", "Frequency")
journal_freq_0 <- journal_freq_0[-which(is.na(journal_freq_0$Journal)),]

journal_freq_1 <- count(total_1$Journal)
names(journal_freq_1) <- c("Journal", "Frequency")
journal_freq_1 <- journal_freq_1[-which(is.na(journal_freq_1$Journal)),]

journal_freq_2 <- count(total_2$Journal)
names(journal_freq_2) <- c("Journal", "Frequency")
journal_freq_2 <- journal_freq_2[-which(is.na(journal_freq_2$Journal)),]

journal_step_freq <- data.frame(Journal = journal_freq_2$Journal, Step2 = journal_freq_2$Frequency)
journal_step_freq["Step0"] <- journal_freq_0$Frequency[match(journal_step_freq$Journal, journal_freq_0$Journal)]
journal_step_freq["Step1"] <- journal_freq_1$Frequency[match(journal_step_freq$Journal, journal_freq_1$Journal)]

##############################################################################
## PLOT CONFERENCE
##############################################################################
conference_freq_0 <- count(total_0$Booktitle)
names(conference_freq_0) <- c("Booktitle", "Frequency")
conference_freq_0 <- conference_freq_0[-which(is.na(conference_freq_0$Booktitle)),]

conference_freq_1 <- count(total_1$Booktitle)
names(conference_freq_1) <- c("Booktitle", "Frequency")
conference_freq_1 <- conference_freq_1[-which(is.na(conference_freq_1$Booktitle)),]

conference_freq_2 <- count(total_2$Booktitle)
names(conference_freq_2) <- c("Booktitle", "Frequency")
conference_freq_2 <- conference_freq_2[-which(is.na(conference_freq_2$Booktitle)),]

conference_step_freq <- data.frame(Booktitle = conference_freq_2$Booktitle, Step2 = conference_freq_2$Frequency)
conference_step_freq["Step0"] <- conference_freq_0$Frequency[match(conference_step_freq$Booktitle, conference_freq_0$Booktitle)]
conference_step_freq["Step1"] <- conference_freq_1$Frequency[match(conference_step_freq$Booktitle, conference_freq_1$Booktitle)]


