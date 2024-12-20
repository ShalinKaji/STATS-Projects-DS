n <- c(1,2,3,5,10,30)
theta <- c(1,5,50,100)
theta_mme = numeric(0)
theta_mle = numeric(0)
mse_mme = numeric(0)
mse_mle = numeric(0)

for (i in theta){
  a=1
  for (j in n){
    
    for(k in 1:1000){
      X <- runif(j,0,i)
      theta_mme[k] <- 2*mean(X)
      theta_mle[k] <- max(X)
      }
    mse_mme[a] <- mean((theta_mme-i)^2)
    mse_mle[a] <- mean((theta_mle-i)^2)
    print(paste("Printing MSE_MME for n = ",j," and theta = ", i, " MSE = ",mse_mme[a]))
    print(paste("Printing MSE_MLE for n = ",j," and theta = ", i, " MSE = ",mse_mle[a]))
    a=a+1
  }
  
  plot(n,mse_mme,type = 'l', col="red",lwd=2, ylab="")
  title(main = paste("Plotting graph for MSE vs 'n' for theta = ",i), ylab = 'Mean-Squared Error')
  lines(n,mse_mle,type = 'l', col="blue",lwd=2)
  legend("topright", legend = c('MME','MLE'), fill = c('red','blue'))
}
  