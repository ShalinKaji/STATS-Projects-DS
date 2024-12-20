# STATS-Mini-Project-5
# Setting working directory to Proj-5 folder.
setwd("C:/Users/Shalin Kaji/Desktop/UT-Dallas-Spr23/STATS-DS-Min.Chen/Mini-Proj-5")
getwd()
library(BSDA)

# Exploring our .csv file
data <- read.csv("bodytemp-heartrate.csv")
typeof(data$gender)
mdata <- data[data$gender==1,]
fdata <- data[data$gender==2,]

# Question-1(a)
# Hypothesis Testing for body_temp metric for M and F data.
# Since number of obv is 65 we can assume that it follows Normal Distribution from the Law of Large Numbers.
print('Mean BodyTemps are unequal as shown by z-test on M-F data.')
z.test(x=mdata$body_temperature,y=fdata$body_temperature,mu=0,sigma.x=sd(mdata$body_temperature),sigma.y=sd(fdata$body_temperature),alternative="two.sided")
summary(mdata$body_temperature)
summary(fdata$body_temperature)


# Question-1(b)
# Hypothesis Testing for heart_rate metric for M and F data.
# Since number of obv is 65 we can assume that it follows Normal Distribution from the Law of Large Numbers.
print('Mean Heart-Rates are unequal as shown by z-test on M-F data.')
z.test(x=mdata$heart_rate,y=fdata$heart_rate,mu=0,sigma.x=sd(mdata$heart_rate),sigma.y=sd(fdata$heart_rate),alternative="two.sided",conf.level = 0.99)
summary(mdata$heart_rate)
summary(fdata$heart_rate)
# Question-1(c)
rho <- cor(mdata$heart_rate,mdata$body_temperature) # finding Pearson's correlation coeff.
rho
# found the point estimate of rho.
ggpubr::ggscatter(mdata, x = "heart_rate", y = "body_temperature",
                  add = "reg.line", conf.int = TRUE,
                  cor.coef = TRUE, cor.method = "pearson",
                  xlab = "Heart Rate", ylab = "Body Temperature")


rho <- cor(fdata$heart_rate,fdata$body_temperature) # finding Pearson's correlation coeff.
rho
# found the point estimate of rho.
ggpubr::ggscatter(fdata, x = "heart_rate", y = "body_temperature",
                  add = "reg.line", conf.int = TRUE,
                  cor.coef = TRUE, cor.method = "pearson",
                  xlab = "Heart Rate", ylab = "Body Temperature")


