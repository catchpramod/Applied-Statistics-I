# a)	Minimum-redundancy-maximum-relevance (mRMR) feature selection

library('mRMRe')
data_cgps <- mRMR.data(data.frame(cgps.ic50, cgps.ge))
selected <- mRMR.classic(data=data_cgps,target_indices=1,50)
solutions(selected)[[1]]
apply(solutions(selected)[[1]],2, function(x,y) {return(y[x])}, y=featureNames(selected))

# b)	Recursive Feature Elimination

library(mlbench)
library(caret)
data(PimaIndiansDiabetes)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
predictors(results)
