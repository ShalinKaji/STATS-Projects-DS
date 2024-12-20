library(boot)

# Given: Investigation focuses on Confidence lvl (1-alpha) = 0.95

n <- c(5,10,30,100)
lambda <- c(0.01,0.1,1,10)

lower.z = numeric(0)
upper.z = numeric(0)
lower.boot = numeric(0)
upper.boot= numeric(0)
z_coverage = numeric(0)
boot_coverage = numeric(0)

mean.boot<-function(rec,sub){
  return(mean(rec[sub]))
  } 

data_info = matrix(,nrow = 1,ncol = 4,dimnames = list(c(1),c("lamda","n","z_coverage","boot_coverage")))
data_info = data_info[-c(1),]
for(param in lambda){
  conf_lvl_LLN = numeric(0)
  conf_lvl_BootPerCI = numeric(0)
  k=1
  for(datasize in n){
    
    
    for (i in 1:5000){
      
      # Generating data following Exponential Distribution.
      data_sim <- rexp(datasize, param)
      
      # Calculating the Confidence Interval using Large Sample Mean formula.
      lower.z <- mean(data_sim) - (qnorm(.975)*sd(data_sim)/sqrt(datasize))  
      upper.z <- mean(data_sim) + (qnorm(.975)*sd(data_sim)/sqrt(datasize))
      
      # Calculating Parametric Bootstrap Percentile CI.
      BootR <- boot(data=data_sim, statistic = mean.boot, R= 999)
      lower.boot <- quantile(BootR$t,.025)
      upper.boot <- quantile(BootR$t,.975)
      
      z_coverage[i] <- ((1/param) >= lower.z) & ((1/param) <= upper.z)
      boot_coverage[i] <- ((1/param) >= lower.boot) & ((1/param) <= upper.boot)
    }
    
    conf_lvl_LLN[k] <- mean(z_coverage)
    conf_lvl_BootPerCI[k] <- mean(boot_coverage)
    
    data_info = rbind(data_info,c(param,datasize,conf_lvl_LLN[k],conf_lvl_BootPerCI[k]))
    k=k+1
  }
  plot(n,conf_lvl_LLN,type = 'l', col="red",lwd=2, ylab="")
  title(main = paste("Plotting graph of Coverage Probability vs N for lambda = ",param), ylab = 'Coverage Probability')
  lines(n,conf_lvl_BootPerCI,type = 'l', col="blue",lwd=2)
  legend("bottomright", legend = c('LS-CI','Boot-CI'), fill = c('red','blue'))
}

data_info

