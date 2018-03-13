#!/bin/sh
Rbatch

# PS7 for Data Science Spring 2018
# 3/12/2018

library(MixedDataImpute)
library(mice)
library(stargazer)
library(readr)
library(tibble)
# loading necessary packages

wages <- read_csv("Downloads/wages.csv")
head(wages)
# bringing in wages data set

cnames<-c("logwage","hgc", "college", "tenure", "age", "married")
wages<-as.data.frame.list(wages, col.names = cnames)
is.data.frame(wages)
# converting csv to data frame and checking

# where tenure or hgc is n/a, drop records
wages1<- wages[!is.na(wages$tenure), ]
head(wages1)
wages2<- wages1[!is.na(wages1$hgc), ]
head(wages2)

# making a LaTex output summary table of the data
stargazer(wages2)

# pulling out logwage NA values
missing<-wages2$logwage[is.na(wages2$logwage)]


# finding rate at which logwages are missing
Rlogwage<-length(missing)/length(wages2$logwage)
Rlogwage
# = 0.2512337

# now taking out logwage records with na value
wages3<-wages2[!is.na(wages2$logwage),]

# linear regression model of hgc on logwage
LinearModel<-lm(formula = wages3$logwage ~ wages3$hgc,data = wages3)
LinearModel
typeof(LinearModel)
# Call:
# lm(formula = wages3$logwage ~ wages3$hgc, data = wages3)
# Coefficients:
#  (Intercept)    wages$hgc  
# 0.99880      0.04989        

# imputing missing data using basic mean approach
logMean<-sum(wages3$logwage)/length(wages3$logwage)
logMean
# = 1.62519
wages2$logwage[is.na(wages2$logwage)]<-logMean
View(wages2$logwage)

# making a new data set for the next imputation
wages4<- wages1[!is.na(wages1$hgc), ]


# imputing missing log values from the regression model made
#    with the complete data
LM<-(0.04989*wages4$hgc + 0.99880)
wages4$logwage[is.na(wages4$logwage)]<-(0.04989*wages4$hgc + 0.99880)
View(wages4$logwage)

# making a new data set for the next imputation
wages5<- wages1[!is.na(wages1$hgc), ]

# imputation with mice
wages5.imp = mice(wages5, seed = 12345)
summary(wages5.imp)
fit = with(wages5.imp, lm(logwage ~ hgc))
round(summary(pool(fit)),2)
# est   se     t     df Pr(>|t|) lo 95 hi 95 nmis  fmi
# (Intercept) 1.02 0.04 23.32 215.19        0  0.93  1.10   NA 0.14
# hgc         0.05 0.00 14.80 137.33        0  0.04  0.06    0 0.18
# lambda
# (Intercept)   0.13
# hgc           0.16

# updating using coefficent and intercept given above
reg.MICE<-(0.16*wages5$hgc + 0.13)
wages5$logwage[is.na(wages5$logwage)]<-(0.16*wages5$hgc + 0.13)
View(wages5$logwage)

logwageVectors<-data.frame(wages2$logwage, wages4$logwage, wages5$logwage)
stargazer(logwageVectors)
