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
# Remove manual record in Orb
dt_bogger <- dt_bogger[dt_bogger$X_CreatedInOrbBy == "Orb",]
dt_bogger <- dt_bogger[dt_bogger$X_LastUpdatedInOrbBy == "Orb",]

dt_equip <- dt_equip[dt_equip$X_CreatedInOrbBy == "Orb",]

# Filter operators exist in both files
operator_bogger <- levels(dt_bogger$Operator)
dt_equip_match <- dt_equip[dt_equip$Operator %in% operator_bogger,]
dt_equip_match$Operator <- as.factor(as.character(dt_equip_match$Operator))

# Find the duration of the task
dt_equip_match$DurationEquip <- as.numeric(difftime(dt_equip_match$EndDateTime, 
                                               dt_equip_match$StartDateTime, units = "secs"))

dt_bogger$DurationBogger <- as.numeric(difftime(dt_bogger$EndDateTime, dt_bogger$StartDateTime, units = "secs"))

# Summary of duration of operator (Equip)
operator_by_duration_equip <- aggregate(DurationEquip ~ Operator, dt_equip_match, sum)


# Summary tonnage and duration of operators (Bogger)
operator_by_tonnage <- aggregate(Tonnage ~ Operator, dt_bogger, sum)
operator_by_duration_bogger <- aggregate(DurationBogger ~ Operator, dt_bogger, sum)

operator_bogger_tb <- data.frame(operator_by_tonnage, operator_by_duration_bogger$DurationBogger)
names(operator_bogger_tb) <- c("Operator", "Tonnage", "DurationBogger")
operator_bogger_tb["Tonnage_Per_Hour"] <- (operator_bogger_tb$Tonnage*3600)/operator_bogger_tb$DurationBogger


# Summarize tasks for operators
task <- as.data.frame.matrix(table(dt_equip_match$Operator, 
                                   dt_equip_match$ActivityCategoryName))
task$Operator <- rownames(task)

# Summarize task duration for operators
task_duration <- aggregate(DurationEquip ~ Operator + ActivityCategoryName, sum, data = dt_equip_match)

task_duration <- reshape(task_duration, idvar = c("Operator"), 
                         timevar = "ActivityCategoryName", direction = "wide")

# Combine them together
task <- task[order(task$Operator),]
task_duration <- task_duration[order(task_duration$Operator),]
operator_bogger_tb <- operator_bogger_tb[order(operator_bogger_tb$Operator),]
operator_by_duration_equip <- operator_by_duration_equip[order(operator_by_duration_equip$Operator),]

operators <- cbind(task_duration, task[1:ncol(task)-1], 
                   operator_bogger_tb[, 2:ncol(operator_bogger_tb)],
                   operator_by_duration_equip$DurationEquip)

colnames(operators)[ncol(operators)] <- "EquipDuration"

operators <- operators[order(operators$Tonnage_Per_Hour),]

operators <- operators[operators$EquipDuration > operators$DurationBogger,]

tonnage <- dt_bogger$Tonnage
tonnage <- tonnage[tonner < 30]
tonnage <- tonnage[tonner > 10]

ggplot() + aes(tonnage)+ geom_histogram(binwidth=1, colour="black", fill = "blue")
ggsave("figs/tonnage_distribution.png")
