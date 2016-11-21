################################################################################
### LOAD LIBRARY
################################################################################


################################################################################
### LOAD DATA
################################################################################
train_raw <- read.csv("data/train.csv", na.strings = c("NA", ""))
test_raw <- read.csv("data/test.csv", na.strings = c("NA", ""))

# Back up
train <- train_raw

################################################################################
### DATA TYPES
################################################################################
str(train)

## Convert Name, Ticket, Cabin to character
indx <- c("Name", "Ticket", "Cabin")
train[indx] <- lapply(train[indx], as.character)

str(train)
################################################################################
### OBSERVE DATA
################################################################################
## Observe missing data
library(VIM)
missing_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, 
                  sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
dev.copy(png,filename="figs/missing_plot.png")
dev.off ()

## Find the relation between Survived, Pclass, Age, and Fare
ggplot(train, aes(x=Fare, y=Age)) + 
  geom_point() +
  facet_grid(Survived ~ Pclass)

dev.copy(png,filename="figs/sur_pc_age_fare_facet_grid.png")
dev.off ()

## Find the relation between Survived, Embarked, Age, and Fare
ggplot(train, aes(x=Fare, y=Age)) + 
  geom_point() +
  facet_grid(Survived ~ Embarked)

## Find the relation between Survived, Embarked
ct_table <- as.data.frame(table(train$Embarked, train$Survived))
names(ct_table) <- c("Embarked", "Survived", "Quantity")
ggplot(data = ct_table, 
       mapping = aes(x = factor(Survived), y = Quantity, fill = Embarked)) +
  geom_bar(stat = "identity")

## Find the relation between Survived, Pclass
ct_table <- as.data.frame(table(train$Pclass, train$Survived))
names(ct_table) <- c("Pclass", "Survived","Quantity")
ggplot(data = ct_table,
       mapping = aes(x = factor(Survived), y = Quantity, fill = Pclass)) +
  geom_bar(stat = "identity")

## Find the relation between Survived, Sex
ct_table <- as.data.frame(table(train$Sex, train$Survived))
names(ct_table) <- c("Sex", "Survived", "Quantity")
ggplot(data = ct_table, 
       mapping = aes(x = factor(Survived), y = Quantity, fill = Sex)) +
  geom_bar(stat = "identity") +
  ggtitle("Distribution of Survived by Sex") +
  xlab("Survived") +
  ylab("# of Passengers") +
  theme(legend.position = "bottom")




