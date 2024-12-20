# STATS-Mini-Project-4
# Setting working directory to Proj-4 folder.
setwd("C:/Users/Shalin Kaji/Desktop/UT-Dallas-Spr23/STATS-DS-Min.Chen/Mini-Proj-4")
getwd()


# Observing our .csv file
gasstats <- read.csv("vapor.csv")
gasstats
theoval <- gasstats$theoretical
expval <- gasstats$experimental

t.test(theoval, expval, paired = FALSE, var.equal = TRUE,alternative = c("two.sided"))
