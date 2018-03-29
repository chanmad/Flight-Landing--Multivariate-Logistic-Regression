rm(list=ls())
install.packages("faraway")
library(faraway)

data(wcgs) #data collected in 1960s, and only for men in a certain age- so bear in mind when
#extending this causation of people of today and women in general and other age groups

class(wcgs)
str(wcgs)
?wcgs

summary(wcgs[,c("chd","height","cigs")]) #max cigs looks like an outlier
#also keep in mind the ratio of people with coronary heart disease
#median of cigs=0 means at least half of the people do not smoke at all

attach(wcgs)
pct<-round(table(chd)/length(chd)*100,1)
labs<-c("No","Yes")
labs<-paste(labs,pct)
labs<-paste(labs,"%",sep="")
pie(table(chd),labels=labs,col=rainbow(length(labs)),main="Pie chart of coronary heart disease")

#visualize association
plot(chd~height) #association isnt very clear
wcgs$y<-ifelse(chd=="No",0,1) 
windows()
plot(jitter(y,0.1)~jitter(height),wcgs,xlab="Height",ylab="Heart disease",pch=".")

library(ggplot2)
ggplot(wcgs,aes(x=height,fill=chd))+geom_histogram(position = "dodge",binwidth = 1)


plot(chd~cigs) #whitespace is yes, the proportion of chd inc with cigs
ggplot(wcgs,aes(x=cigs,fill=chd))+geom_histogram(position = "dodge",binwidth = 20)
#for people who smoke, proportion of yes(blue) is higher, while for non smokers, chd=no is higher

#correlation between chd and H and chd and cigs
cor(wcgs[,1:12], use = "complete.obs")

model.lm<-lm(y~height+cigs, data=wcgs)
summary(model.lm) #problem with using linear reg for this model is since output is binary,
#but our model is assuming continuous inputs
#In linear model, we have 2 parameters (mean and var).
#Y~N(mu,var), Y relies on X only through its mean(=mu) 
#and variability (std error) of coefficients is dependent on var.


lmod<-glm(formula=chd~height+cigs, family=binomial, wcgs)
summary(lmod)

beta.lmod<-coef(lmod)
plot(jitter(y,0.1)~jitter(height),wcgs)

curve(ilogit(beta.lmod[1]+beta.lmod[2]*x+beta.lmod[3]*0),add=TRUE)

#cigarette use vs chd
plot(jitter(y,0.1)~jitter(cigs),wcgs)
curve(ilogit(beta.lmod[1]+beta.lmod[2]*60+beta.lmod[3]*x),add=TRUE)
curve(ilogit(beta.lmod[1]+beta.lmod[2]*78+beta.lmod[3]*x),lty=2,add=TRUE)

#comapring linear vs logistic models:
#1 significance of coefficients
#2 signs of coefficients
#3 predicted value from both the models

summary(lmod$fitted.values)
summary(model.lm$fitted.values)
plot(lmod$fitted.values~model.lm$fitted.values)


#compare models for height as it had a high p value
lmod.s<-glm(chd~cigs, family=binomial,wcgs)
anova(lmod.s,lmod,test="Chi")

#added a new variable BMI
wcgs$BMI<-with(wcgs, 703*wcgs$weight/(wcgs$height)^2)
lmod.full<- glm(chd~age+height+weight+BMI+sdp+dbp+chol+dibep+cigs+arcus, family = binomial,wcgs)
summary(lmod.full)

attach(wcgs)
round(cor(data.frame(height,weight,BMI)),digits = 1) #as height and bmi arent corr, we take both
round(cor(sdp,dbp),1) #for this we can check which variable has higher cor with Y in case of linear reg,
#in case of logistic, we need to regress y with both.
summary(glm(chd~dbp,family=binomial))
summary(glm(chd~sdp,family=binomial)) #this is preferred due to higher beta

summary(sdp)
summary(dbp) #preferably standardize this and then perform glm 


lmod.full2<- glm(chd~age+height+BMI+dbp+chol+dibep+cigs+arcus, family=binomial, data=wcgs)
drop1(lmod.full2, test="Chi") #single term deletions

lmod.0<-glm(chd~age+height+weight+BMI+sdp+dbp+chol+dibep+cigs, family = binomial,wcgs)
summary(lmod.0)

model.AIC<- step(lmod.0,trace = 0)
summary(model.AIC) #here, first collinearity, domain knowledge, then signifiance and then AIC were used to elimi

#alternatively if we use stepAIC- we might retain vars with high p 
#since AIC is measuring prediction- this can be SUB-OPTIMAL
model.AIC2<-step(lmod.full, trace=0)
summary(model.AIC2)


wcgsm<-na.omit(wcgs)
dim(wcgsm)
model.final<-glm(chd~age+height+BMI+dbp+chol+dibep+cigs,family=binomial, wcgsm)
linpred<-predict(model.final)
predprob<-predict(model.final, type="response")
predout<-ifelse(predprob<0.5, "no","yes")
wcgsm<-data.frame(wcgsm,predout,predprob)
xtabs(~chd+predout,wcgsm)

#confidence Interval
new.ind<-data.frame(age=50,height=70,BMI=25,dbp=80, chol=200, dibep="B", cigs=20)
predict(lmod.0,newdata = new.ind,type = "link") #linear predictor
predict(lmod.0,newdata = new.ind,type="response") #probability

#Variation in logistic- Link functions
lmod.0<-glm(chd~age+height+BMI+dbp+chol+dibep+cigs, family=binomial,wcgs)
lmod.probit<-glm(chd~age+height+BMI+dbp+chol+dibep+cigs, family=binomial(link=probit),wcgs)
lmod.cloglog<- glm(chd~age+height+BMI+dbp+chol+dibep+cigs, family=binomial(link=cloglog),wcgs)
lmod.cauchit<- glm(chd~age+height+BMI+dbp+chol+dibep+cigs, family=binomial(link=cauchit),wcgs)

round(coef(lmod.0),3)
round(coef(lmod.probit),3)
round(coef(lmod.cloglog),3)
round(coef(lmod.cauchit),3)

#predval gives p values on y axis- using a link to map eta to p.
predval<-sapply(list(lmod.0,lmod.probit,lmod.cloglog,lmod.cauchit),fitted)
colnames(predval)<-c("logit","probit", "cloglog", "cauchit")
round(predval[1:10,],3)
#observing values around the center
round(predval[fitted(lmod.0)>0.3 & fitted(lmod.0)<0.5,],3)
#observing tail
round(predval[fitted(lmod.0)<0.01,],3)


