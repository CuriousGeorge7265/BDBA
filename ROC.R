#ROC Curve
stargazer(prf,
          summary = FALSE,
          align = TRUE, no.space = TRUE, type = "html", out = paste0(dir,"Name of file.doc"))
install.packages ("ROCR", dependencies = TRUE ) library (ROCR)
# Collect observed and predicted values
yvalue <- dsData$d.dep.variable
#For treeRslt
ypredA <- predict(treeRslt, type = c("prob"))[, 2]
#For regRslt
ypredB<-predict(regRslt)
#For logRslt
ypredC <- predict(rsltLog, type ="response")
# Make predictive summary & use performance to prepare plot
predA <- prediction(ypredA, yvalue)
perfA <- performance(predA, measure ="tpr", x.measure = "fpr")
# Make the plot
plot (perfA, col = rainbow(10), lwd = 2)
abline (a = 0, b = 1, lty = 3, lwd = 1.5 , col = "blue")
# Add multiple ROC’s in one graph (mdlA & mdlB, predA & predB etc.)
plot (perfA, lty = 1, lwd = 2.0 , col = "red", main ="Name of graph") plot (perfB, lty = 1, lwd = 2.0 , col = "blue", add = TRUE)
plot (perfC, lty = 1, lwd = 2.0 , col = "green", add = TRUE)
abline (a = 0, b = 1, lty = 3, lwd = 1.5)
legend (0.6, 0.5, c("line 1", "line 2", “line 3”), col = c("blue", "red", “green”), lwd =3)
# Determine the area underneath the curve (AUC) value -> from 0 to 1
auc.tmp <- performance (predA,"auc") auc <- as.numeric(auc.tmp@y.values) auc
