T = pmax(rexp(1000,.1),rexp(1000,.1))
head(T,10)
#hist(T, breaks=50)
#curve((0.2*exp(-0.1*x)-0.2*exp(-0.2*x)) *20000, from =0, to=100, add = TRUE, col="red")
mean(T)
sum(T>15)/length(T)

for(i in 1:5){
T = pmax(rexp(100000,.1),rexp(100000,.1))
head(T,10)
#hist(T, breaks=50)
#curve((0.2*exp(-0.1*x)-0.2*exp(-0.2*x)) *20000, from =0, to=100, add = TRUE, col="red")
print(mean(T))
print(sum(T>15)/length(T))
}