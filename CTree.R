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
# Make predictions for the test set
#---------

# Find predictions for the test set
predTree <- predict(rsltTree, dsTitanic.Test, type = "class")
predTree <- as.numeric(predTree) - 1

# Write the data frame to a csv file. 
write.csv(dsResult, file="MySurvivalPredictions.csv",
          row.names=FALSE)

                  

#---------
# Plot basic tree, basic and more colorful ones
#---------
rpart.plot(rsltTree)
rpart.plot(rsltTree, 
           box.col=c("pink", "palegreen3")[rsltTree$frame$yval], 
           extra = 1)
           
           
