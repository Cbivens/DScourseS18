# Probelm Set 8 Data Science S18
# 3-27-18
# Christin Bivens

#!/bin/bash

######### Part 4

library(nloptr)
# bringing out package nloptr

set.seed(100)
# setting seed for reproducibility

N <- 100000
K <- 10
sigma <- 0.5
# assigning values for matrix dimensions

X<-matrix(rnorm(N*K,mean=0,sd=sigma),N,K)
head(X)
# making a matrix "X"

X[,1] <- 1 
# first column of X should be all ones

eps<-rnorm(N,mean=0,sd=0.5)
beta<-c(1.5,-1,-.25,.75,3.5,-2,.5,1,1.25,2)
Y<- beta%*%X + eps
head(Y)
# creating Y
# [,1]
# [1,]  0.2813611
# [2,]  0.9728459
# [3,] -1.3468027
# [4,] -0.1414746
# [5,]  1.7566247
# [6,]  1.5836980

######### Part 5

betaOLS<- solve(t(X) %*% X) %*% t(X) %*% Y
head(betaOLS)
# [1,] 0.7748080
# [2,] 0.4963190
# [3,] 0.5152915
# [4,] 0.6001095
# [5,] 0.7091331
# [6,] 0.9668910

############ Part 6

# set up a stepsize
alpha <-0.0000003

# set up a number of iterations
maxiter<-500000

## Our objective function
objfun <- function(beta,Y,X) {
  return (beta%*%X+eps)
}

# define the gradient of our objective function
gradient <- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

# create a vector to contain all beta's for all steps
beta.All <- matrix("numeric",length(beta),maxiter)

# gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,Y,X)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

# print result and plot all xs for every iteration
print(iter)
#[1] 1104

print(paste("The minimum of f(beta,Y,X) is ", beta, sep = ""))
# [1] "The minimum of f(beta,Y,X) is 0.774807975493321" 
# [2] "The minimum of f(beta,Y,X) is 0.496318913121644" 
# [3] "The minimum of f(beta,Y,X) is 0.515291455510075" 
# [4] "The minimum of f(beta,Y,X) is 0.600109530781268" 
# [5] "The minimum of f(beta,Y,X) is 0.709133235430076" 
# [6] "The minimum of f(beta,Y,X) is 0.966890850499107" 
# [7] "The minimum of f(beta,Y,X) is 0.313341072858513" 
# [8] "The minimum of f(beta,Y,X) is 0.0361304989593428"
# [9] "The minimum of f(beta,Y,X) is 0.624437148940285" 
# [10] "The minimum of f(beta,Y,X) is 0.858535580804735"

############ Part 7 BLFGS/Nedler-Mead

#### BLFGS

## Our objective function
eval_f <- function(beta,Y,X) {
  return (beta%*%X+eps)
}

## Gradient of our objective function
eval_grad_f <- function(beta,Y,X) {
   return (-2*t(X)%*%(Y-X%*%beta))
}

## initial values
x0 <- -5

## Algorithm parameters
opts <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8)

## Find the optimum!
rest <- nloptr( x0=x0,eval_f=eval_f,eval_grad_f=eval_grad_f,opts=opts)
print(rest)

#### Nedler-Mead
## initial values
xstart <- 5

## Algorithm parameters
options<-list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-8)

## Find the optimum!
res <- nloptr( x0=xstart,eval_f=objfun,opts=options)
print(res)
# Call:
# nloptr(x0 = x0, eval_f = eval_f, eval_grad_f = eval_grad_f, opts = opts)
# Minimization using NLopt version 2.4.2 
# NLopt solver status: 1 ( NLOPT_SUCCESS: Generic success return value. 
# )
# Number of Iterations....: 13 
# Termination conditions:  xtol_rel: 1e-08 
# Number of inequality constraints:  0 
# Number of equality constraints:    0 
# Optimal value of objective function:  -6.54296875 
# Optimal value of controls: 2.25

############ Part 8

gradient <- function (theta ,Y,X) {
  grad <- as.vector ( rep (0, length (theta )))
  beta <- theta [1:( length ( theta) -1)]
  sig <- theta [ length (theta )]
  grad [1:( length ( theta) -1)] <- -t(X)%*%(Y - X%*%beta )/(sig ^2)
  grad[ length (theta )] <- dim (X)[1] /sig - crossprod (Y-X%*%beta )/(sig
                                                                       ^3)
  return ( grad )
}
# 0.03613  0.62444  0.85854

############ Part 9

estimates<-lm(Y~X -1)
estimates
# Call:
# lm(formula = Y ~ X - 1)
# Coefficients:
#  X1       X2       X3       X4       X5       X6       X7  
# 0.77481  0.49632  0.51529  0.60011  0.70913  0.96689  0.31334  
# X8       X9      X10  
# 0.03613  0.62444  0.85854 

library(stargazer)
stargazer(estimates)
# using stargazer to make the LaTex output of the linear regression
