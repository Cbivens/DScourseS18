#!/bin/bash
# Data Sci Spring 2018
# 3-29-2018
# machine learning with housing data
# cross validation
# c bivens

library(glmnet)
library(mlr)

housing<-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
names(housing)<-c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names)
#    1. CRIM      per capita crime rate by town
#    2. ZN        proportion of residential land zoned for lots over 
#                 25,000 sq.ft.
#    3. INDUS     proportion of non-retail business acres per town
#    4. CHAS      Charles River dummy variable (= 1 if tract bounds 
#                 river; 0 otherwise)
#    5. NOX       nitric oxides concentration (parts per 10 million)
#    6. RM        average number of rooms per dwelling
#    7. AGE       proportion of owner-occupied units built prior to 1940
#    8. DIS       weighted distances to five Boston employment centres
#    9. RAD       index of accessibility to radial highways
#    10. TAX      full-value property-tax rate per $10,000
#    11. PTRATIO  pupil-teacher ratio by town
#    12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks 
#                 by town
#    13. LSTAT    % lower status of the population
#    14. MEDV     Median value of owner-occupied homes in $1000's

#---------------------------------------------------------------
# OLS
#---------------------------------------------------------------

# Adding features
housing$lmedv<-log(housing$medv)
housing$medv<-NULL # drop median value
formula<-as.formula(lmedv ~ .^3 + poly(crim,6) +
                          poly(zn ,6)+
                          poly(indus,6) +
                          poly(nox,6) +
                          poly(rm,6) +
                          poly(age,6) +poly(dis ,6) +
                          poly(rad,6) +
                          poly(tax,6) +
                          poly(ptratio,6) +
                          poly(b,6) +
                          poly(lstat,6))

mod_matrix<-data.frame(model.matrix(formula,housing))
# now replace the intercept column by the response since MLR will do
# "y ~ ." and get the intercept by default

mod_matrix[,1]=housing$lmedv
colnames(mod_matrix)[1] = "lmedv"
# make sure to rename it otherwise MLR won’tfind it
head(mod_matrix) 
# just make sure everything is hunky-dory

# Break up the data:
n<-nrow(mod_matrix)
train<-sample(n,size = .8*n)
test<-setdiff(1:n, train)
housing.train<-mod_matrix[train,]
housing.test<-mod_matrix[test,]
                          
                          
# Add some other features
housing$lmedv<-log(housing$medv)
housing$medv<-NULL
housing$dis2<-housing$dis^2
housing$chasNOX<-housing$crim*housing$nox

# Break up the data:
n<-nrow(housing)
train<-sample(n,size=.8*n)
test<-setdiff(1:n,train)
housing.train<-housing[train,]
housing.test<-housing[test,]

dim(housing.train)
# [1] 404  16

# Define the task:
theTask <- makeRegrTask(id = "taskname", data = housing.train, target = "lmedv")
print(theTask)

#--------------------------------------------------------------
# OLS
#--------------------------------------------------------------

# tell mlr what prediction algorithm we'll be using (OLS)
predAlg <- makeLearner("regr.lm")

# Set resampling strategy (here let's do 6-fold CV)
resampleStrat <- makeResampleDesc(method = "CV", iters = 6)

# Do the resampling
sampleResults <- resample(learner = predAlg, task = theTask, resampling = resampleStrat, measures=list(rmse))

# Mean RMSE across the 6 folds
print(sampleResults$aggr)
# rmse.test.rmse = 0.1865421 

#------------------------------------------------------------
# LASSO
#------------------------------------------------------------

# Tell it a new prediction algorithm
predAlg <- makeLearner("regr.glmnet")

# Search over penalty parameter lambda and force elastic net parameter to be 1 (LASSO)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))

# Take 50 random guesses at lambda within the interval I specified above
tuneMethod <- makeTuneControlRandom(maxit = 50L)

# Do the tuning
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)
print(tunedModel$x)
# $lambda
# [1] 0.01466217

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))
# Resampling: cross-validation
# Measures:             rmse      
# [Resample] iter 1:    0.1965280 
# [Resample] iter 2:    0.2034588 
# [Resample] iter 3:    0.2809826 
# [Resample] iter 4:    0.1751785 
# [Resample] iter 5:    0.1686213 
# [Resample] iter 6:    0.1990183 
# Aggregated Result: rmse.test.rmse=0.2072434
# Resample Result
# Task: taskname
# Learner: regr.glmnet
# Aggr perf: rmse.test.rmse=0.2072434
# Runtime: 0.253681

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = housing.test)

print(head(prediction$data))
#      truth response
# 4  3.508556 3.403116
# 10 2.939162 2.971730
# 11 2.708050 2.935026
# 12 2.939162 3.084137
# 14 3.015535 3.059693
# 15 2.901422 3.022178

OOSE<-sqrt(mean((prediction$data$truth-prediction$data$response)^2))
print(OOSE)
# [1] 0.1917285

#------------------------------------------------------------
# Ridge Regression
#------------------------------------------------------------

# Search over penalty parameter lambda and force elastic net parameter to be 0 (ridge)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=0))

# Do the tuning again
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

print(tunedModel$x)
# lambda = 0.006271684

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))
# Resampling: cross-validation
# Measures:             rmse      
# [Resample] iter 1:    0.1894790 
# [Resample] iter 2:    0.1995994 
# [Resample] iter 3:    0.1873840 
# [Resample] iter 4:    0.2427689 
# [Resample] iter 5:    0.1759651 
# [Resample] iter 6:    0.1732309 
# Aggregated Result: rmse.test.rmse=0.1961141
# Resample Result
# Task: taskname
# Learner: regr.glmnet
# Aggr perf: rmse.test.rmse=0.1961141
# Runtime: 0.197462

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = housing.test)

OOSEridge<-sqrt(mean((prediction$data$truth-prediction$data$response)^2))
print(OOSEridge)
# = 0.2043624

#-------------------------------------------------------------------
# Elastic net Model
#-------------------------------------------------------------------

# Search over penalty parameter lambda and force elastic net parameter to be 0 and 1 (lasso/ridge weighted alpha)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=1))

# Do the tuning again
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

print(tunedModel$x)
# $lambda
# [1] 0.07564617
# $alpha
# [1] 0.001897175

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))
# Resampling: cross-validation
# Measures:             rmse      
# [Resample] iter 1:    0.1879226 
# [Resample] iter 2:    0.2195686 
# [Resample] iter 3:    0.2189743 
# [Resample] iter 4:    0.2094945 
# [Resample] iter 5:    0.1714250 
# [Resample] iter 6:    0.1926368 
# Aggregated Result: rmse.test.rmse=0.2007728
# Resample Result
# Task: taskname
# Learner: regr.glmnet
# Aggr perf: rmse.test.rmse=0.2007728
# Runtime: 0.308043

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = housing.test)

OOSEelast<-sqrt(mean((prediction$data$truth-prediction$data$response)^2))
print(OOSEelast)
# = 0.2056368

performance(prediction, measures = list(rmse))
# rmse = 0.2056368
