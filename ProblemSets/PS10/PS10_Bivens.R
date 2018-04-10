
#!/bin/bash
# Problem Set 10
# DS Spring 18
# 4-10
# C Bivens

#-------------------------------------------------------------------
# PACKAGES
#-------------------------------------------------------------------

# downloaded following packages for running each tribe
library(e1071)
# use for bayesians and analogizers
# cost/gamma are reg. parameters 
library(kknn)
# use for analogizers
library(GA)
# use for evolutionaries
library(rpart)
# use for symbolists -- tree models
# regularization parameters
#       minsplit : integer
#       minbucket: integer
#       cp: numeric, typicaly very small
#       mlr algorithm name: classif.rpart
library(nnet)
# use for connectionists/neural networks
library(mlr)
library(glmnet)
library(MASS)
library(ggvis)

#-------------------------------------------------------------------
# BRINGING IN/GETTING FAMILIAR WITH THE DATA
#-------------------------------------------------------------------

set.seed(100)
# for reproducibility

income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

#-------------------------------------------------------------------
# CLEANING THE DATA
#-------------------------------------------------------------------

# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL

# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)

# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]

#-------------------------------------------------------------------
#  BUILDING CLASSIFICATION TASK
#-------------------------------------------------------------------
# target variable <- income$high.earner

IncomeTask<-makeClassifTask(id = "task", data = income,
                            target = "high.earner")
# ^ defining the task following the mlr package example
print(IncomeTask)
# Supervised task: task
# Type: classif
# Target: high.earner
# Observations: 32561
# Features:
#   numerics     factors     ordered functionals 
# 5           7           0           0 
# Missings: FALSE
# Has weights: FALSE
# Has blocking: FALSE
# Has coordinates: FALSE
# Classes: 2
# <=50K  >50K 
# 24720  7841 
# Positive class: <=50K

#-------------------------------------------------------------------
#  CREATING CROSS-VALIDATION AND TUNING STRATEGIES
#-------------------------------------------------------------------

CrossValStrat <- makeResampleDesc(method = "CV", iters = 3)
# 3-fold cross=validation

Tuner <- makeTuneControlRandom(maxit = 10L)
# Random tuning with 10 guesses

#-------------------------------------------------------------------
#  THE LEARNERS
#-------------------------------------------------------------------

TreeLearner<-makeLearner("classif.rpart",predict.type="response")
LogLearner<-makeLearner("classif.glmnet",predict.type="response")
NeuralLearner<-makeLearner("classif.nnet",predict.type="response")
BayesLearner<-makeLearner("classif.naiveBayes",
                          predict.type="response")
kNNLearner<-makeLearner("classif.kknn",predict.type="response")
svmLearner<-makeLearner("classif.svm",predict.type="response")

#-------------------------------------------------------------------
#  SETTING UP HYPERPARAMETERS FOR EACH LEARNER ALGORITHM
#-------------------------------------------------------------------

# TREE MODEL -------------------------------------------------------
TreeParam<-makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)

# LOGIT MODEL -----------------------------------------------------
LogitParam<-makeParamSet(makeNumericParam("lambda",lower=0,upper=3),
                          makeNumericParam("alpha",lower=0,upper=1))

# NEURAL NETWROK MODEL --------------------------------------------
NeuralParam<-makeParamSet(
  makeIntegerParam("size",lower = 1, upper = 10),
  makeNumericParam("decay", lower = .1, upper = .5),
  makeIntegerParam("maxit", lower = 1000, upper = 1000)
)

# kNN MODEL -------------------------------------------------------
knnParam<-makeParamSet(makeIntegerParam("k",lower = 1, upper = 30))

# SVM MODEL -------------------------------------------------------
svmParam<-makeParamSet( 
                       makeDiscreteParam("cost", values = c(2^-2, 2^-1, 2^0, 2^1, 2^2, 2^10)),
                       makeDiscreteParam("gamma", values = c(2^-2, 2^-1, 2^0, 2^1, 2^2, 2^10))
)

#-------------------------------------------------------------------
#  TUNING THE MODELS (EXCEPT BAYES)
#-------------------------------------------------------------------

# TREE MODEL -------------------------------------------------------
TreeTune<-tuneParams(learner = TreeLearner,
                     task = IncomeTask, 
                     resampling = CrossValStrat,
                     measures = list(f1,gmean),
                     par.set = TreeParam,
                     control = Tuner,
                     show.info = TRUE)

# LOGIT MODEL ------------------------------------------------------
LogitTune<-tuneParams(learner = LogLearner,
                     task = IncomeTask, 
                     resampling = CrossValStrat,
                     measures = list(f1,gmean),
                     par.set = LogitParam,
                     control = Tuner,
                     show.info = TRUE)

# NEURAL NETWROK MODEL ---------------------------------------------
NeuralTune<-tuneParams(learner = NeuralLearner,
                     task = IncomeTask, 
                     resampling = CrossValStrat,
                     measures = list(f1,gmean),
                     par.set = NeuralParam,
                     control = Tuner,
                     show.info = TRUE)

# kNN MODEL -------------------------------------------------------
knnTune<-tuneParams(learner = kNNLearner,
                       task = IncomeTask, 
                       resampling = CrossValStrat,
                       measures = list(f1,gmean),
                       par.set = knnParam,
                       control = Tuner,
                       show.info = TRUE)

# SVM MODEL ------------------------------------------------------
svmTune<-tuneParams(learner = svmLearner,
                    task = IncomeTask, 
                    resampling = CrossValStrat,
                    measures = list(f1, gmean),
                    par.set = svmParam,
                    control = Tuner,
                    show.info = TRUE)

#-------------------------------------------------------------------
#  APPLYING OPTIMAL TUNING PARAMETERS (MINUS BAYES)
#-------------------------------------------------------------------

OptTree<-setHyperPars(learner=TreeLearner, par.vals = TreeTune$x)
OptLogit<-setHyperPars(learner=LogLearner, par.vals = LogitTune$x)
OptNeural<-setHyperPars(learner=NeuralLearner, par.vals = NeuralTune$x)
OptKNN<-setHyperPars(learner=kNNLearner, par.vals = knnTune$x)
OptSVM<-setHyperPars(learner=svmLearner, par.vals = svmTune$x)

#-------------------------------------------------------------------
#  VERIFYING PERFORMANCE
#-------------------------------------------------------------------

TreeResults <-resample(learner=OptTree,task=IncomeTask,
                      resampling=CrossValStrat, 
                      measures=list(f1,gmean))

LogitResults <-resample(learner=OptLogit,task=IncomeTask,
                      resampling=CrossValStrat, 
                      measures=list(f1,gmean))

NeuralResults <-resample(learner=OptNeural,task=IncomeTask,
                      resampling=CrossValStrat, 
                      measures=list(f1,gmean))

KnnResults <-resample(learner=OptKNN,task=IncomeTask,
                      resampling=CrossValStrat, 
                      measures=list(f1,gmean))

svmResults <-resample(learner=OptSVM,task=IncomeTask,
                      resampling=CrossValStrat, 
                      measures=list(f1,gmean))

#-------------------------------------------------------------------
#  TRAINING FINAL MODELS
#-------------------------------------------------------------------

TreeModel    <-train(learner=OptTree, task=IncomeTask)
LogitModel   <-train(learner=OptLogit, task=IncomeTask)
NeuralModel  <-train(learner=OptNeural, task=IncomeTask)
KnnModel     <-train(learner=OptKNN, task=IncomeTask)
SvmModel     <-train(learner=OptSVM, task=IncomeTask)
BayesModel   <-train(learner=BayesLearner, task=IncomeTask)

#-------------------------------------------------------------------
#  PREDICTING IN TEST DATA
#-------------------------------------------------------------------

TreePredict    <-predict(TreeModel,newdata=income.test)
LogitPredict   <-predict(LogitModel,newdata=income.test)
NeuralPredict  <-predict(NeuralModel,newdata=income.test)
KnnPredict     <-predict(KnnModel,newdata=income.test)
SvmPredict     <-predict(SvmModel,newdata=income.test)
BayesPredict   <-predict(BayesModel,newdata=income.test)

#-------------------------------------------------------------------
#  EVALUATING PERFORMANCE
#-------------------------------------------------------------------

print(performance(TreePredict,measures=list(f1,gmean)))
print(performance(LogitPredict,measures=list(f1,gmean)))
print(performance(NeuralPredict,measures=list(f1,gmean)))
print(performance(KnnPredict,measures=list(f1,gmean)))
print(performance(SvmPredict,measures=list(f1,gmean)))
print(performance(BayesPredict,measures=list(f1,gmean)))
