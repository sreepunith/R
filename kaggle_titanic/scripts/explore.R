################################################################################
# LOAD LIBRARY
################################################################################
library(VIM) #aggr


################################################################################
# LOAD DATA
################################################################################
train_raw <- read.csv("data/train.csv", na.strings = c("NA", ""))

# Back up
train <- train_raw

################################################################################
# DATA TYPES
################################################################################
str(train)

## Convert Name, Ticket, Cabin to character
indx <- c("Name", "Ticket", "Cabin")
train[indx] <- lapply(train[indx], as.character)

str(train)
################################################################################
### OBSERVE DATA
################################################################################
# Missing data
missing_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, 
                  sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
dev.copy(png,filename="figs/missing_plot.png")
dev.off ()

# Find the relation between Survived, Pclass, Age, and Fare
ggplot(train, aes(x=Fare, y=Age)) + 
  geom_point() +
  facet_grid(Survived ~ Pclass)

dev.copy(png,filename="figs/survived_pclass_age_fare_facet_grid.png")
dev.off ()

# Find the relation between Survived, Embarked, Age, and Fare
ggplot(train, aes(x=Fare, y=Age)) + 
  geom_point() +
  facet_grid(Survived ~ Embarked)

dev.copy(png,filename="figs/survived_embarked_age_fare_facet_grid.png")
dev.off ()

## Find the relation between Survived, Sibsp
ggplot(data = train, mapping = aes(x = SibSp)) +
  geom_histogram()

## Find the relation between Survived, Parch
ggplot(data = train, mapping = aes(x = Parch)) +
  geom_histogram()

dev.copy(png,filename="figs/survived_parch_bar.png")
dev.off ()

## Find the relation between Survived, Embarked
ct_table <- as.data.frame(table(train$Embarked, train$Survived))
names(ct_table) <- c("Embarked", "Survived", "Quantity")
ggplot(data = ct_table, 
       mapping = aes(x = Survived, y = Quantity, fill = Embarked)) +
  geom_bar(stat = "identity")

dev.copy(png,filename="figs/survived_embarked_bar.png")
dev.off ()

## Find the relation between Survived, Pclass
ct_table <- as.data.frame(table(train$Pclass, train$Survived))
names(ct_table) <- c("Pclass", "Survived","Quantity")
ggplot(data = ct_table,
       mapping = aes(x = Survived, y = Quantity, fill = Pclass)) +
  geom_bar(stat = "identity")

dev.copy(png,filename="figs/survived_pclass_bar.png")
dev.off ()

## Find the relation between Survived, Sex
ct_table <- as.data.frame(table(train$Sex, train$Survived))
names(ct_table) <- c("Sex", "Survived", "Quantity")
ggplot(data = ct_table, 
       mapping = aes(x = Survived, y = Quantity, fill = Sex)) +
  geom_bar(stat = "identity") +
  ggtitle("Distribution of Survived by Sex") +
  xlab("Survived") +
  ylab("# of Passengers") +
  theme(legend.position = "bottom")

dev.copy(png,filename="figs/survived_sex_bar.png")
dev.off ()

# Extract Title from Name
get_title_from_name <- function(x) {
  index_of_comma <- regexpr("\\, [A-Z][a-z]+\\.", x)
  index_of_dot <- regexpr("\\. ", x)
  return (substr(x, index_of_comma + 2, index_of_dot - 1))
}

train["Title"] <- get_title_from_name(train$Name)
train$Title <- as.factor(train$Title)

ct_table <- as.data.frame(table(train$Title, train$Survived))
names(ct_table) <- c("Title", "Survived", "Quantity")
ggplot(data = ct_table, 
       mapping = aes(x = Survived, y = Quantity, fill = Title)) +
  geom_bar(stat = "identity")
################################################################################
### NORMALITY CHECK
################################################################################
train$Age[which(is.na(train$Age))] <- median(train$Age, na.rm = TRUE)

ggplot(data = train, mapping = aes(x = Age)) +
  geom_bar(stat = "count")

qqnorm(train$Age)
qqline(train$Age)

box_cox_trans <- function(x, lambda) {
  return ((x^lambda - 1)/lambda)
}

age_bc <- box_cox_trans(train$Age, 0.8)
age_log <- log10(train$Age)
age_reci <- 1/train$Age

hist(age_reci)
hist(age_log)
qqnorm(age_bc)
hist(age_bc)
hist(train$Age)
qqline(age_bc)

library(AID)
boxcoxnc(train$Age)

make_unit <- function(x) {x / sqrt(sum(x^2))}
z <- c(1,2,3)
make_unit(z)
x <- c(0,0,1)

Q <- matrix(c(x,y,z), nrow = 3, byrow = FALSE)