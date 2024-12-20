# maximizing the log-likelihood function of Pareto distribution by using Optim function.
dat <- c(21.72,14.65,50.42,28.78,11.23)

neg.loglik.fun<-function(par,dat){
  total=0
  for(i in dat){
    total<- total + (log(par)-(par+1)*log(i) )
  }
  return(-total)
}

ml.est <- optim(par=5, fn=neg.loglik.fun, hessian = T, dat=dat)
ml.est
se<-sqrt(diag(solve(ml.est$hessian)))
CI<- ml.est$par +c(-1,1)*qnorm(.975)*se
CI