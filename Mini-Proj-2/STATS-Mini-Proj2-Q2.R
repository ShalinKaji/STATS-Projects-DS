# STATS-Mini-Project-2
# Setting working directory to Proj-2 folder.
setwd("C:/Users/Shalin Kaji/Desktop/UT-Dallas-Spr23/STATS-DS-Min.Chen/Mini-Proj-2")
getwd()

# Observing our .csv file
motor <- read.csv("motorcycle.csv")
ctyacc <- motor$Fatal.Motorcycle.Accidents
boxplot(ctyacc, horizontal = TRUE, xlab = 'Number of Fatalities.')
stripchart(ctyacc, method = "jitter", pch = 19, add = TRUE, col = "orange")
dstats <- summary(ctyacc)
dstats
print(paste("Standard Deviation: ", sd(ctyacc)))
print(paste("IQR: ", dstats[5]-dstats[2]))
motor[motor$Fatal.Motorcycle.Accidents>((dstats[5]-dstats[2])*1.5+dstats[5]),]

