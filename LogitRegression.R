# Define the model. The period means 'all avaiable columns'
mdlA <- churn ~ .
rsltLogit <- glm(mdlA, data = churnTrain, family = "binomial")

# Make predictions on the test set, predLogit contains predicted
# class probabilities
predLogit <- predict(rsltLogit, churnTest, type="response")

# Before classification performance can be determined, this
# predLogit has to be converted to factor (or the target variable
# to a (0,1) variable). Classification performance is assessed 
# for the holdout set.
classLogit <- factor(predLogit > 0.5,
                     levels = c(FALSE, TRUE),
                     labels = c("0", "1"))
