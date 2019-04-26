mdlB <- MedianValue ~ (Crime + Industry + Rooms + Age1940)*CharlesDummy
rsltA <- lm(mdlA, data = dsHousing)
predA <- predict(rsltA, dsHousing)
plot(rsltA)
stargazer(rsltA, rsltB,
          title="Regression results",
          align=TRUE,
          no.space=TRUE,
          type="html",
          out ="regressionResults.doc")
          
#Change level of factor variables
levels(churnTrain$churn)
table(churnTrain$churn)

churnTrain$churn <- factor(churnTrain$churn,
                           levels=c("no", "yes"), 
                           labels = c("0", "1"))
churnTest$churn  <- factor(churnTest$churn,
                           levels=c("no", "yes"), 
                           labels = c("0", "1"))
                           
                           
 # Define the model. The period means 'all avaiable columns'
mdlA <- churn ~ .

