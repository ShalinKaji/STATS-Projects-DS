pi=c(0,1)
for(i in 1:100){
x = runif(10000,0,1)
y = runif(10000,0,1)
dis= sqrt((0.5-x)^2+(0.5-y)^2)
pi[i]=4*sum(dis<.5)/length(dis)
}
mean(pi)