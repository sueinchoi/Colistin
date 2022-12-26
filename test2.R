library(cmprsk)

ss <- rexp(100)
gg <- factor(sample(1:3,100,replace=TRUE),1:3,c('a','b','c'))
cc <- sample(0:2,100,replace=TRUE)
print(xx <- cuminc(ss,cc,gg, cencode = 0))
plot(xx,lty=1,color=1:6)
?cuminc


