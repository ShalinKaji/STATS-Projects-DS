# STATS-Mini-Project-2
# Setting working directory to Proj-2 folder.
setwd("C:/Users/Shalin Kaji/Desktop/UT-Dallas-Spr23/STATS-DS-Min.Chen/Mini-Proj-2")
getwd()

# Observing our .csv file
rdrace <- read.csv("roadrace.csv")
head(rdrace,10)

# Question 1-a
# Create a barplot of Maine, what does it conclude?
barplot(table(rdrace$Maine)/length(rdrace$Maine), ylim = c(0,1))
print('Probabilities of runners from Maine/Away: ')
table(rdrace$Maine)/length(rdrace$Maine)
print('To conclude: No. of runners from Maine is thrice the no. of runners from other cities.')

# Question 1-b
# Creating histograms of running times for Runners from Maine/Away.
fr_Maine <- rdrace[rdrace$Maine=='Maine',]
fr_Away <- rdrace[rdrace$Maine=="Away",]
p1 <- hist(fr_Maine$Time..minutes., breaks=20)
p2 <- hist(fr_Away$Time..minutes., breaks=20)
plot(p1,col="light blue",xlim = c(20,160), main="Runner's Time" , xlab = "Time in Mins" , ylab = "Number of Runners")
plot(p2,col="light pink",add=T)

legend("topright", c("Maine","Away"), col=c("light blue","light pink"),lwd = 5)
# Statistics for Maine Runners
desc_Maine <- summary(fr_Maine$Time..minutes.)
desc_Maine
print(paste("Range: ",desc_Maine["Max."]-desc_Maine["Min."]))
print(paste("Standard Deviation: ", sd(fr_Maine$Time..minutes.)))
print(paste("IQR: ",(desc_Maine["3rd Qu."]-desc_Maine["1st Qu."])))

#Statistics for Away Runners
desc_Away <- summary(fr_Away$Time..minutes.)
desc_Away
print(paste("Range: ",desc_Away["Max."]-desc_Away["Min."]))
print(paste("Standard Deviation: ", sd(fr_Away$Time..minutes.)))
print(paste("IQR: ",(desc_Away["3rd Qu."]-desc_Away["1st Qu."])))
print("In conclusion: The running times of Runners from Maine & Away follow Symmetric distribution. ")

# Question 1-C
# Creating Box plots for both Runner categories.
bplt_runners <- cbind("Maine"=(fr_Maine$Time..minutes.),"Away"=(fr_Away$Time..minutes.))
boxplot(bplt_runners, beside=T, horizontal = TRUE)

# Question 1-d
Run_M <- rdrace[rdrace$Sex=='M',]
Run_F <- rdrace[rdrace$Sex=='F',]

mfrunner <- cbind("Male"=(type.convert(Run_M$Age)),"Female"=(type.convert(Run_F$Age)))
boxplot(mfrunner, beside=T, horizontal = TRUE)
summary(mfrunner)
sd_run1 <- sd(mfrunner[,"Male"],na.rm = T)
sd_run1
sd_run1 <- sd(mfrunner[,"Female"],na.rm = T)
sd_run1
