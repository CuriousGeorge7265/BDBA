#Accuracy
#Simple Confusion matrix
# Define original and predicted values of the dependent from the object
yvalue <- dsData$d.dep.variable
# For classification trees:
ypred <- predict(treeRsltB, type = c("prob"))[, 2] # For regression trees results:
ypred<-predict(regRsltB)
# For simple regression model
ypred2<-predict(regRslt, type=c("response")) # For logit regression results:
ypredC <- predict(rsltLog, type ="response")
head(cbind (yvalue, ypred), 10)
# Convert the predicted probabilities of a successful outcomes into predicted classifications. All predicted values above a threshold equal to 0.5 are predicted a successful outcome
ypred <- as.numeric(ypred > 0.5)
# Confusion matrix
-> gives predicted class probabilities
table(Predicted = ypred, Observed = yvalue)
# Accuracy, Sensitivity, Specificity, FPR & Precision
P <- sum(yvalue == 1)
N <- sum(yvalue == 0)
A <- P + N
TP <- sum ((ypred == 1)*(yvalue == 1)) FP <- sum ((ypred == 1)*(yvalue == 0)) TN <- sum ((ypred == 0)*(yvalue == 0)) FN <- sum ((ypred == 0)*(yvalue == 1))
cbind (Accuracy = (TP + TN)/(P+N), Specificity.TNR = TN/(FP+TN), Sensitivity.TPR = TP/(FN+TP), FPR = FP/(FP+TN),
       Precision = TP/(FP + TP))
Confusion matrix function -> combine more mdls
#Create the classPerf function:
classPerf <- function(y, p, tau = 0.5){ # y:yvalue (binary)
  prf
  #Summarise
}
# p:ypred
# tau:threshold (0.5 by default)
# Convert classification probabilities to classification predictions using the specified threshold tau
p <- as.numeric(p > tau)
# Define observed and predicted classification variables as factors. Data type factor is applied in order to prevent errors when predicted classes are all of one kind
y <- factor (y, levels = c(0 ,1))
p <- factor (p, levels = c(0 ,1))
# Make a classification table
tbl <- table (Predicted = p, Observed = y)
# Identify classifications
TP <- tbl [2,2]
FN <- tbl [1,2]
TN <- tbl [1,1]
FP <- tbl [2,1]
# Measure performance
perf <- c(Accuracy = (TP+TN)/sum(tbl), Sensitivity = TP/(TP + FN), Specificity = TN/(FP + TN),
          Precision = TP/(FP + TP))
# return the outcome
return (perf)
prf <- data.frame(Logit=classPerf(yvalue, predLog), LinReg=classPerf(yvalue, predReg),
                  Tree=classPerf(yvalue, predTree))