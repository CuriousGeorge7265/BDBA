
# Data Handling: Missing and Subsetting 
#---------
# Missing Value 
#---------
dsTitanic[complete.cases(dsTitanic),]
## Impute 
dsTitanic$Age[is.na(dsTitanic$Age)] <- 
  mean(dsTitanic$Age,na.rm=TRUE)

colSums(is.na(dsTitanic.Train))
colSums(is.na(dsTitanic.Test))
## Drop empty rows 
which(dsTitanic.Train$Embarked == "")

dsTitanic.Train <- 
  dsTitanic.Train[-which(dsTitanic.Train$Embarked == ""), ]

dsTitanic.Train$Embarked <- factor(dsTitanic.Train$Embarked)
dsTitanic.Test$Embarked <- factor(dsTitanic.Test$Embarked)



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


