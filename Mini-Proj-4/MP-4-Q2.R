# STATS-Mini-Project-4
# Setting working directory to Proj-4 folder.
setwd("C:/Users/Shalin Kaji/Desktop/UT-Dallas-Spr23/STATS-DS-Min.Chen/Mini-Proj-4")
getwd()


# Observing our .csv file
companyrecs <- read.csv("voltage.csv")
remotebr <- companyrecs[companyrecs$location==0,]
localbr <- companyrecs[companyrecs$location==1,]

boxplot(localbr$voltage,remotebr$voltage,names = c("local","remote"))
summary(remotebr$voltage)

mean_diff <- function(data, index) {
  mean(data[index,1]) - mean(data[index,2])
}

boot_res <- boot(data = data.frame(remotebr$voltage,localbr$voltage),statistic = mean_diff, R=9999)
boot.ci(boot_res)
boot_res$t0

t.test(remotebr$voltage,localbr$voltage,paired = F, var.equal = F)
qqnorm(remotebr$voltage)
qqline(remotebr$voltage)
