simpLogistReg <- function(y,X,sigsq.beta)
{
   # Do MFVB initialisations:

   xiVec <- rep(1,length(y))
   itnum <- 0;    converged <- FALSE
   maxIter <- 50; logMLcurr <- -1e20 
   while (!converged) 
   {
      itnum <- itnum + 1
                  
      # Update q*(beta,u) parameters:

      Sigma.q.beta <- solve(crossprod(X,2*lambda(xiVec)*X) + rep((1/sigsq.beta),ncX))
      mu.q.beta <- Sigma.q.beta%*%XTy

      # Update xiVec:

      xiVec <- sqrt(diag(X%*%(Sigma.q.beta + tcrossprod(mu.q.beta))%*%t(X)))

      if (itnum >= maxIter) converged <- TRUE   
   }
   return(list(Sigma.q.beta=Sigma.q.beta,mu.q.beta=mu.q.beta,xiVec=xiVec)) 
}
