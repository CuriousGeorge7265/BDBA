#---------------------------------------------------------------
# Lab session 1
#---------------------------------------------------------------

remove(list = ls())

#---------------------------------------------------------------
# 2: R, the calculator
#---------------------------------------------------------------

3*(5+2)
8^(1+14)
1e2/9
100 %% 9


8+
  3

log(5.2)
sqrt(9.8)
abs(-12.1)
exp(2.2)
factorial(5)

pi
pi / 3.141593



x
x <- exp(3*log(9))
x <- x + 98
y <- x/3


?log
?exp


#---------------------------------------------------------------
# 3: Booleans (logical expressions)
#---------------------------------------------------------------

5 < 4
150 < exp((pi^2) / 2) 
exp((pi^2)/2) > 150


x <- 5 
(x + 3) <= 10
(x + 3) >= 10
(x*2) == 10



!((x + 3) <= 10) 
!((x + 3) >= 10) 
(x*2) != 10



y <- 22 
if (y > 20) { 
  y <- y / 2
  } 
y


x <- 12 
if (x > 10) {
  x <- x / 2
  } else {
  x <- x + 3
  } 
x 


3 + 5 + 6 + 4 + 5 
sqrt(3) + sqrt(5) + sqrt(6) + sqrt(4) + sqrt(5)
3*5*6*4*5 


#---------------------------------------------------------------
# 4: Vectors
#---------------------------------------------------------------

x <- c(3, 5, 6, 4, 5) 
sum(x) 
sum(sqrt(x)) 
prod(x)
sqrt(x)


sqrt(x)[1]
sqrt(x)[4]
sqrt(x)[length(x)]


y <- sqrt(x)
y[1]
y[4]
y[c(1, 4)]


1:10 
21:25 
c(1:10, 21:25)


x <- cars
View(cars)
x <- cars$dist

x > mean(x)

selection <- (x > mean(x))
sum(selection) 
x[selection]

x^2 + x + 1


x^2 + x + 1 == x^2 + x + rep(1, length(x)) 
all.equal(x^2 + x + 1, x^2 + x + rep(1, length(x)))


#---------------------------------------------------------------
# 5: Working with univariate data
#---------------------------------------------------------------

x <- cars
summary(x) 
hist(x$speed) 
plot(x$dist) 
plot(x) 
lines(x) 
plot()

#---------------------------------------------------------------
# 6: Data frames
#---------------------------------------------------------------

# Set working directory
setwd("Directory where the data are stored")

# Read the data
dsTitanic <- read.csv(file="train.csv", 
                      stringsAsFactors = FALSE)

#---------
# Quick summaries
#---------
dsTitanic

class(dsTitanic)
View(dsTitanic)
head(dsTitanic)
tail(dsTitanic)
str(dsTitanic)
summary(dsTitanic)


#---------
# Referencing columns and rows
#---------

dsTitanic$Survived[1:10]

dsTitanic[1:10, ]
dsTitanic[1:10, ]$Survived
dsTitanic$Survived[1:10]
dsTitanic[1:10, 1:3]
dsTitanic[-(1:30), c("Survived", "Name")]

dsTitanic <- data.frame(dsTitanic,1)
dsTitanic <- cbind(dsTitanic,1)

colnames(dsTitanic)
originalNames <- colnames(dsTitanic) 
colnames(dsTitanic)[7] <- "numberSiblingsSpouses"
colnames(dsTitanic)
colnames(dsTitanic) <- c(originalNames) 
colnames(dsTitanic)

rownames(dsTitanic)
rownames(dsTitanic) <- dsTitanic$Name
rownames(dsTitanic)
rownames(dsTitanic) <- NULL
rownames(dsTitanic)


dsTitanic$lnFare <- log(dsTitanic$Fare)
str(dsTitanic)
dsTitanic[is.na(dsTitanic)] <- 0

#---------
# Ways to treat missing values
#---------

# Load the Titanic data again
dsTitanic <- read.csv(file="train.csv", stringsAsFactors = FALSE)

# Listwise deletion of all rows with any missings
dsTitanicComplete <-  dsTitanic[complete.cases(dsTitanic),]

# Missing values: focus on Age
str(dsTitanic$Age)
summary(dsTitanic$Age)

mean(dsTitanic$Age)
mean(dsTitanic$Age,na.rm=TRUE)

mean(dsTitanicComplete$Age)

# Impute missing values with the mean value
dsTitanic$Age[is.na(dsTitanic$Age)] <- 
  mean(dsTitanic$Age,na.rm=TRUE)


#---------
# Subsetting from data frames
#---------

# Subsetting directly applied to the data frame
dsTitanic[, c("PassengerId","Survived","Pclass")]
dsTitanic[dsTitanic$Age < 25, ]
dsTitanic[dsTitanic$Fare > 30, ]

dsTitanic[dsTitanic$Age < 25 & dsTitanic$Fare > 30,
          c("PassengerId","Survived","Pclass")]


# Subsetting with the subset function
subset(dsTitanic,
       dsTitanic$Age < 25 & dsTitanic$Fare > 30,
       c("PassengerId","Survived","Pclass"))


#---------
# Make training and test sets
#---------

# Determine the number of observations, and set the 
# fraction of training observations
nObservations <- nrow(dsTitanic)
pTraining     <- 0.7
nTraining     <- round(pTraining*nObservations)

# Option 1. Select first set of observations as training set, 
# and the latter as test set
obsTraining   <- 1:nTraining

dsTitanic.Train <- dsTitanic[obsTraining,]
dsTitanic.Test  <- dsTitanic[-obsTraining,]


# Option 2. Select training set based on a random sample from 
# the available observations, using the sample function
obsTraining <- sample(1:nObservations, nTraining, 
                      replace=FALSE)

dsTitanic.Train <- dsTitanic[obsTraining,]
dsTitanic.Test  <- dsTitanic[-obsTraining,]




#---------------------------------------------------------------
# 7: Building models: decision trees
#---------------------------------------------------------------

#---------
# Install and load required packages
#---------

# Install the packages (this needs to be done only once, after
# which the instructions are usually commented out)
install.packages("rpart", dependencies = TRUE)
install.packages("rpart.plot", dependencies = TRUE)

# Loading the required libraries (needs to be done every session)
library(rpart)
library(rpart.plot)

#---------
# Reading training and test data 
#---------
setwd("C:/Dropbox/Teaching/2019 Big Data/Course Documents/LabSessions/Data")

dsTitanic.Train  <- read.csv("train.csv", stringsAsFactors = FALSE)
dsTitanic.Test   <- read.csv("test.csv", stringsAsFactors = FALSE)


#---------
# Data preparation
#---------

# Check if missing values are present in the training and test 
# sets. This happens to be the case for Age (training, test)
# and Fare (test)
colSums(is.na(dsTitanic.Train))
colSums(is.na(dsTitanic.Test))

# Mean substitution for Age in the training and test sets. 
# As the test set is not supposed to be available at the 
# time of training the data, the missing Age information in 
# the test set will be imputed based on the average of the 
# training set
avgAge <- mean(dsTitanic.Train$Age, na.rm=TRUE)

dsTitanic.Train$Age[is.na(dsTitanic.Train$Age)] <- avgAge 
dsTitanic.Test$Age[is.na(dsTitanic.Test$Age)]   <- avgAge

# Mean substitution for Fare in the test set 
avgFare <- mean(dsTitanic.Train$Fare, na.rm=TRUE)

dsTitanic.Test$Fare[is.na(dsTitanic.Test$Fare)] <- avgFare

# Check structure of the two data frames
str(dsTitanic.Train)
str(dsTitanic.Test)

# Some observations of Embarked are also missing, or rather, 
# empty
which(dsTitanic.Train$Embarked == "")

dsTitanic.Train <- 
  dsTitanic.Train[-which(dsTitanic.Train$Embarked == ""), ]

dsTitanic.Train$Embarked <- factor(dsTitanic.Train$Embarked)
dsTitanic.Test$Embarked <- factor(dsTitanic.Test$Embarked)


#---------
# Build the tree on the training set
#---------
# Specify the model
mdlTree <- Survived ~ Pclass + Sex + Age + SibSp + 
  Parch + Fare + Embarked

# Train the model
rsltTree <- rpart(mdlTree, data = dsTitanic.Train, 
                  method="class", 
                  parms = list(split = "information"))

#---------
# Plot basic tree, basic and more colorful ones
#---------
rpart.plot(rsltTree)
rpart.plot(rsltTree, 
           box.col=c("pink", "palegreen3")[rsltTree$frame$yval], 
           extra = 1)
rpart.plot(rsltTree, 
           box.col=c("pink", "palegreen3")[rsltTree$frame$yval], 
           extra = 104)

#---------
# Print the decision tree as a set of rules
#---------
print(rsltTree)

#---------
# Make predictions for the test set
#---------

# Find predictions for the test set
predTree <- predict(rsltTree, dsTitanic.Test, type = "class")

# Note that the class predictions are stored in a vector 
# with type `factor'. Straightforward comparison with the
# survival information in the input data may cumbersome.
# The predTree vector is therefore converted to factor.
# As this leads to a numeric vector with values 1 and 2, 
# an amount 1 is subsequently substracted.
predTree <- as.numeric(predTree) - 1

# Combine passenger id's with the survival prediction into 
# a single data frame
dsResult <- 
  data.frame(PassengerID = dsTitanic.Test$PassengerId, 
             Survived=predTree)

# Write the data frame to a csv file. 
write.csv(dsResult, file="MySurvivalPredictions.csv",
          row.names=FALSE)


#---------------------------------------------------------------
# 8: Example markdown
#---------------------------------------------------------------


# See the seperate script for rMarkdown


