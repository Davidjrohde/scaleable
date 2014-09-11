########## R function: scalarLogit.r ##########
# For fitting logistic regression using glm()
# and JJ approximation by breaking data and 
# process in pieces.
#
# Author: Cathy Lee
# Last changed: 08 SEP 2014
##############################################

# Clear R memory:

rm(list=ls()) 

set.seed(465858)

# Load required packages:

library(LiblineaR)
library(speedglm)
library(biglm)
library(glmnet)

# Load user-written function:

source("simpLogistReg.r")
source("glmPackCompare.r")

# Set flags:

doGLM <- F;      doBatchFit <- T;     
doRealTime <- T; maxIterONL <- 3 
dobenchMarking <- T

lambda <- function(x) 
{
   nzi <- (1:length(x))[x!=0]
   ans <- rep(0.125,length(x))
   ans[nzi] <- tanh(x[nzi]/2)/(4*x[nzi])
   return(ans)
}

# Define dimension parameter and true values:

nMax <- 10000 #120.425 seconds
nWarm <- 5000

sigsq.beta <- 10e5 
betaVec <- c(0.5,-1.1,0.3,-0.2,1.6,-2.9)

# Simulate data for batch processing:

x1 <- runif(nWarm); x2 <- runif(nWarm)
x3 <- runif(nWarm); x4 <- runif(nWarm)
x5 <- runif(nWarm)
X <- cbind(rep(1,nWarm),x1,x2,x3,x4,x5)
y <- rbinom(nWarm,rep(1,nWarm),1/(1+exp(-(X%*%betaVec))))

if (FALSE)
{
   x1Max <- runif(nMax); x2Max <- runif(nMax)   
   x3Max <- runif(nMax); x4Max <- runif(nMax)
   x5Max <- runif(nMax)
   XMax <- cbind(rep(1,nMax),x1Max,x2Max,x3Max,x4Max,x5Max)
   yMax <- rbinom(nMax,rep(1,nMax),1/(1+exp(-(XMax%*%betaVec))))
}

if (doGLM)
{
   # Fit data using glm():

   timeGLM <- system.time(
              fitGLM <- glm(yMax ~ x1Max + x2Max + x3Max + x4Max + x5Max,
                            family=binomial(link=logit)))[3]
   betaGLM <- fitGLM$coef
   SEbetaGLM <- sqrt(diag(vcov(fitGLM)))
   print(timeGLM)
}

if (doBatchFit|doRealTime)
{
   #X <- XMax[1:nWarm,]
   #y <- yMax[1:nWarm]
   XTy <- crossprod(X,y-0.5)
   ncX <- ncol(X)
   n <- nWarm
   MFVBfit <- simpLogistReg(y,X,sigsq.beta)
}

if(doRealTime)
{
   n <- nWarm

   # Initialize parameters with batch values:
   
   Sigma.q.beta <- MFVBfit$Sigma.q.beta
   mu.q.beta <- MFVBfit$mu.q.beta
   xiVec <- MFVBfit$xiVec
   XTy <- XTy
   XTwtX <- crossprod(X,lambda(xiVec)*X)
   
   timeMFVB <- system.time(
   while (n < nMax)
   {
      # Simulate new data on the fly:

      n <- n + 1
      #print(n)

      XNew <- c(1,runif(5))
      yNew <- rbinom(1,1,(1/(1+exp(-(XNew%*%betaVec)))))

      #yNew <- yMax[n] 
      #XNew <- XMax[n,]

      # Perform updates:

      itNum <- 100; converged <- FALSE
      while (!converged)
      {
         itNum <- itNum + 1

         # Update scalar, vector and matrices:

         xi <- sqrt(diag(t(XNew)%*%(Sigma.q.beta + tcrossprod(mu.q.beta))%*%XNew))
         XTy <- XTy + XNew*(yNew-0.5)
         XTwtX <- XTwtX + lambda(xi)*tcrossprod(XNew) 
                  
         # Update q*(beta,u) parameters:

         Sigma.q.beta <- solve(2*XTwtX + rep((1/sigsq.beta),ncX))
         mu.q.beta <- Sigma.q.beta%*%XTy

         if (itNum >= maxIterONL) converged <- TRUE  
      } 
   })[3]
}
print(timeMFVB)
ans <- cbind(betaVec,round(mu.q.beta,2))
colnames(ans) <- c("True","MFVB")
print(ans)

if (dobenchMarking)
{
   # Benchmarking logistic regression using glm.fit, 
   # bigglm, speedglm, glmnet, LiblineaR:

   glmPackCompare(1000000, 5, 500, FALSE, NULL, 1)
}

########## End of scalarLogit.Rs ##########
