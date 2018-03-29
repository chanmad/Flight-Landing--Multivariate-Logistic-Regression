rm(list=ls())
library("faraway")
data("orings")
str(orings)
summary(orings)

plot(damage/6 ~temp, orings, xlim=c(25,85), ylim=c(0,1), ylab="Prob of damage")

#repeating each occurance 6 times and creating 0s for the times when damage is 0
damage2<- sapply(orings$damage, function(x) rep(c(0,1), times=c(6-x,x)))
orings2<- with(orings,data.frame(temp=rep(temp, each=6), damage=as.vector(damage2)))
model2<-glm(damage~temp, family = binomial, orings2)
summary(model2)

#prob of failing when temp=31
predict(model2, newdata = data.frame(temp=31), type="response", se.fit = T)
c(0.993-1.96*0.01153,0.993+1.96*0.01153) #upper bound>1 as this approach assumes equal width

#better approach for CI is to apply CI on eta and then find p
predict(model2, newdata = data.frame(temp=31), type="link", se.fit = T)
ilogit(c(4.96-1.96*1.67,4.96+1.96*1.67))

#binomial data instead of bernoulli data
model1<-glm(cbind(damage,6-damage)~temp, family = binomial, orings)
summary(model1)
summary(model2)
#introduce dispersion- this results in same beta but higher se
sigma.squared<-sum(residuals(model1, type="pearson")^2)/(nrow(orings)-2)
summary(model1, dispersion = sigma.squared)
