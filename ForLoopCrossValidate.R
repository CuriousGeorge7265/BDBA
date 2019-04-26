
# Specifying nbr of splits, folds
nFolds <- 5 # 5-fold cross validation # Randomly assign a fold-id’s to each observation
iFolds <- sample (1:nFolds, nrow (dsData), replace = TRUE) table (iFolds)
# Create list to stole results (list can have info with diff. data types & diff. lengths)
rslt <- list()
# for-loop process (evaluation of model by re-running instruction for K folds)
# Define the loop:
for (fold in 1:nFolds ){
}
# 1: Select the training and test sets
dsData.Train <- dsData[iFolds != fold ,] dsData.Test <- dsData[iFolds == fold ,]
# 2: Train the models
logRslt <- glm(mdlA , data = dsData.Train, binomial ( link = "logit")) regRslt <- lm(mdlA , data = dsData.Train)
treeRslt <- rpart(mdlA , data = dsData.Train, method ="class",
                   parms=list(split ="information"))
# 3: Find observed and predicted target values for the test set
yvalue.Test <- dsData.Test$dep.variable
predLog.Test <- predict(logRslt, dsData.Test, type = c("response")) predReg.Test <- predict (regRslt, dsData.Test)
predTree.Test <- predict(treeRslt, dsData.Test, type = "prob")[ ,2]
# 4: Store observed and predicted targets in rslt object
rslt$observed[[fold]] <- yvalue.Test rslt$predicted$Log[[fold]] <- unname(predLog.Test) rslt$predicted$Reg[[fold]] <- unname(predReg.Test) rslt$predicted$Tree[[fold]] <- unname(predTree.Test)