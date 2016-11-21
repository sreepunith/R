# functions
readData <- function(file.name, column.types, missing.types) {
  read.csv(file = file.name,
           colClasses = column.types,
           na.strings = missing.types
           )
}

#implementation
train.data.filename <- "titanic/train.csv"
test.data.filename <- "titanic/test.csv"
missing.types <- c("NA", "")
train.column.types <- c('integer', #PassengerId
                        'factor',  #Survived
                        'factor',  #Pclass
                        'character', #Name
                        'factor', #Sex
                        'numeric', #Age
                        'integer', #SibSp
                        'integer', #Parch
                        'character', #ticket
                        'numeric', #Faretes
                        'character', #Cabin
                        'factor' #embarked
                        )

test.column.types <- train.column.types[-2] #no Survived in test

train.raw <- readData(file.name = train.data.filename, column.types = train.column.types
                      , missing.types = missing.types)

df.train <- train.raw

test.raw <- readData(file.name = test.data.filename, column.types = test.column.types,
                     missing.types = missing.types)

df.infer <- test.raw

#data munging
require(Amelia)
missmap(obj = df.train, main = "Titanic Training Data - Missings Map",
        col = c("yellow", "black"), legend = FALSE)

barplot(table(df.train$Survived),
        names.arg = c("Perished", "Survived"),
        main = "Survived", col = "black", beside = FALSE)

barplot(table(df.train$Pclass),
        names.arg = c("first", "second", "third"),
        main = "Pclass (Passenger traveling class)", col = "firebrick")

barplot(table(df.train$Sex), main = "Sex", col = "Green")

hist(df.train$Age, main = "Age", xlab = NULL)

mosaicplot(df.train$Pclass ~ df.train$Survived,
           main = "Passenger fate by Travelling class",
           shade = FALSE, color = TRUE,
           xlab = "Pclass", ylab = "Survived")

mosaicplot(df.train$Sex ~ df.train$Survived,
           main = "Passenger fate by Gender",
           shade = FALSE, color = TRUE,
           xlab = "Sex", ylab = "Survived")

boxplot(df.train$Age ~ df.train$Survived,
        main = "Passenger fate by Age",
        xlab = "Survived", ylab = "Age")

mosaicplot(df.train$Cabin ~ df.train$Survived,
           main = "Passenger fate by Cabin",
           xlab = "Survived", ylab = "Cabin")

summary(df.train$Age)

boxplot(df.train$Age ~ df.train$Pclass,
        main = NULL,
        xlab = "Pclass", ylab = "Survived")

#TITLE

getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  data <- substr(data, title.dot.start+2, title.comma.end-1)
  return (data)
}

df.train$Title <- getTitle(df.train$Name)

require(Hmisc)
options(digits = 2)
bystats(df.train$Age, df.train$Title, fun = function(x)c(Mean=mean(x), Median=median(x)))

title.na.train <- c('Dr', 'Master', 'Miss', 'Mr', 'Mrs')

imputeAge <- function(df.age, df.title, title.na) {
  for (title in title.na) {
    df.age[which(df.title == title)] <- impute(df.age[which(df.title == title)])
  }
  return (df.age)
}

df.train$Age <- imputeAge(df.train$Age, df.train$Title, title.na.train)

bystats(df.train$Age, df.train$Title, fun = function(x) c(Mean=mean(x), Media=median(x)))

#EMBARKED
df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'

summary(df.train$Embarked)

#FARE
boxplot(summary(df.train$Fare))

fare_les_than_seven <- subset(df.train, Fare < 7)
fare_les_than_seven[order(fare_les_than_seven$Fare),c("Age", "Title", "Pclass", "Fare")]

#Impute missings on Fare feature with median fare by Pclass
imputeFare <- function(df.Fare, df.Pclass, na.fare) {
  for (title in title.na) {
    df.age[which(df.title == title)] <- impute(df.age[which(df.title == title)])
  }
  return (df.age)
}

df.train$Fare[which(df.train$Fare == 0)] <- NA
df.train$Fare <- imputeMedian

#GIVING UP



