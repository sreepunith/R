################################################################################
# LOAD LIBRARY
################################################################################
library(anytime)
################################################################################
# LOAD DATA
################################################################################
dt_lhd_machine_11 <- read.csv("data/NewMachine11.csv")
dt_lhd_machine_12 <- read.csv("data/NewMachine12.csv")
dt_bogger <- read.csv("data/BoggerCycle_LD118.csv")
dt_equip <- read.csv("data/EquipmentTask_LD118.csv")

dt_equip_1111 <- dt_equip[date(dt_equip$StartDateTime) == "2016-11-11",]
dt_equip_1111 <- dt_equip_1111[dt_equip_1111$Operator == "094c68186f33332bfeb662f33d65a41a60720296fba85eb006294319a0d35226",]


dt_bogger_1111 <- dt_bogger[date(dt_equip$StartDateTime) == "2016-11-11",]
dt_bogger_1111 <- dt_bogger_1111[dt_bogger_1111$Operator == "094c68186f33332bfeb662f33d65a41a60720296fba85eb006294319a0d35226",]

dt_lhd_machine_11 <- dt_lhd_machine_11[dt_lhd_machine_11$X >= 38710,]
dt_lhd_machine_12 <- dt_lhd_machine_12[date(dt_lhd_machine_12$NewOriginTime) == "2016-11-11",]
dt_lhd_machine_12 <- dt_lhd_machine_12[hour(dt_lhd_machine_12$NewOriginTime) < 18,]

dt_lhdmachine_11 <- rbind(dt_lhd_machine_11, dt_lhd_machine_12)
################################################################################
# DEFINE DATA
# Analyse the data of the operator 
# 094c68186f33332bfeb662f33d65a41a60720296fba85eb006294319a0d35226
################################################################################
equip <- dt_equip_1111 # Equipment data for Nov 11
bogger <- dt_bogger_1111 # Bogger data for Nov 11
machine_sensor <- dt_lhdmachine_11 # Machine sensor for Nov 11

write_csv(bogger, "equip.csv")
write_csv(machine_sensor, "machine_sensor.csv")

