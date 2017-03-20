################################################################################
# LOAD LIBRARY
################################################################################
library(ggplot2)
library(lubridate)
library(dplyr)
library(vcd)
library(reshape)
################################################################################
# LOAD DATA
################################################################################
# Equip
dt_equip <- read.csv("data/EquipmentTask_LD118.csv")
dt_equip$Operator <- as.factor(as.character(dt_equip$Operator))
dt_equip$ActivityName <- as.factor(as.character(dt_equip$ActivityName))
dt_equip$ActivityCategoryName <- as.factor(as.character(dt_equip$ActivityCategoryName))
dt_equip$LocationName <- as.factor(as.character(dt_equip$LocationName))
dt_equip$PanelCave <- as.factor(as.character(dt_equip$PanelCave))
dt_equip$ActivityGroup <- as.factor(as.character(dt_equip$ActivityGroup))

# Bogger
dt_bogger <- read.csv("data/BoggerCycle_LD118.csv")
dt_bogger$Operator <- as.factor(as.character(dt_bogger$Operator))

################################################################################
# PROCESS DATA
################################################################################
# Select observations only in Nov 11
dt_equip_1111 <- with(dt_equip, 
                      dt_equip[date(StartDateTime) == "2016-11-12", ])
dt_equip_1111 <- with(dt_equip_1111, 
                      dt_equip_1111[date(StartDateTime) == "2016-11-12", ])


dt_equip_1111$Operator <- as.factor(as.character(dt_equip_1111$Operator))
dt_equip_1111$ActivityCategoryName <- as.factor(as.character(dt_equip_1111$ActivityCategoryName))

dt_bogger_1111 <- with(dt_bogger, dt_bogger[date(StartDateTime) == "2016-11-12", ])
dt_bogger_1111$Operator <- as.factor(as.character(dt_bogger_1111$Operator))

# Filter operators exist in both files
operator_bogger <- levels(dt_bogger_1111$Operator)
dt_equip_1111_match <- dt_equip_1111[dt_equip_1111$Operator %in% operator_bogger,]
dt_equip_1111_match$Operator <- as.factor(as.character(dt_equip_1111_match$Operator))

# Find the duration of the task
dt_equip_1111_match$Duration <- as.numeric(difftime(dt_equip_1111_match$EndDateTime, dt_equip_1111_match$StartDateTime, units = "secs"))

dt_bogger_1111$Duration <- as.numeric(difftime(dt_bogger_1111$EndDateTime, dt_bogger_1111$StartDateTime, units = "secs"))

# Find the tonnage by hour of operator
operator_by_tonnage <- aggregate(Tonnage ~ Operator, dt_bogger_1111, sum)
operator_by_duration <- aggregate(Duration ~ Operator, dt_bogger_1111, sum)

operator_bogger_tb <- data.frame(operator_by_tonnage, operator_by_duration$Duration)
names(operator_bogger_tb) <- c("Operator", "Tonnage", "Duration")

operator_bogger_tb["Tonnage_Per_Hour"] <- operator_bogger_tb$Tonnage/3600
operator_bogger_tb

ggplot(data = operator_bogger_tb, aes(x = Operator, y = Tonnage_Per_Hour)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

# ggsave("operator_effectiveness.png")

# Summarize tasks for operators
temp <- as.data.frame.matrix(table(dt_equip_1111_match$Operator, 
                                   dt_equip_1111_match$ActivityCategoryName))

# temp$Operator <- rownames(temp)

test <- aggregate(Duration ~ Operator + ActivityCategoryName, sum, data = dt_equip_1111_match)

task_duration <- reshape(test, idvar = c("Operator"), 
        timevar = "ActivityCategoryName", direction = "wide")


temp <- temp[order(temp[,1]),]
task_duration <- task_duration[order(task_duration[,1]),]
operator_bogger_tb <- operator_bogger_tb[order(operator_bogger_tb[,1]),]

task_duration <- cbind(task_duration, temp, operator_bogger_tb[, 2:ncol(operator_bogger_tb)])
