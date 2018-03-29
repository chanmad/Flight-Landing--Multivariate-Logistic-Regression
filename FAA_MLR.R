rm(list=ls())
library(tidyverse)
library("readxl")


FAA1<-read.csv("C:\\Users\\madha\\OneDrive\\Documents\\UC-BANA\\Spring Sem 2018\\Stat Modeling\\FAA1.csv")
FAA2<-read_xls("C:\\Users\\madha\\OneDrive\\Documents\\UC-BANA\\Spring Sem 2018\\Stat Modeling\\FAA2.xls")
str(FAA1)
str(FAA2)

#One variable was missing in FAA2 but merge using all=TRUE binds the data with no errors
FAA3<-merge(FAA1,FAA2,all = TRUE)

FAA3[,'speed_ground']<-round(FAA3$speed_ground, digits = 4)
head(FAA3$speed_ground)
str(FAA3)
FAA3<-FAA3 %>% distinct(speed_ground, .keep_all = TRUE)
summary(FAA3)

#Data Cleaning- abnormal values
#FAA3<-FAA3[FAA3$duration>40 | FAA3$speed_ground>30 | FAA3$speed_ground<140 | FAA3$speed_air>30 | FAA3$speed_air<140 
#           |FAA3$height>6 | FAA3$distance>6000,]

FAA3<-subset(FAA3,FAA3$duration>40) 
FAA3<-subset(FAA3,FAA3$speed_ground>30)
FAA3<-subset(FAA3,FAA3$speed_ground<140)
FAA3<- subset(FAA3,FAA3$speed_air>30 | is.na(FAA3$speed_air)) 
FAA3<-subset(FAA3,FAA3$speed_air<140 | is.na(FAA3$speed_air)) 
FAA3<- FAA3[FAA3$height>6,] 
FAA3<-FAA3[FAA3$distance<6000,]

dim(FAA3)
summary(FAA3)

par(mfrow=c(2,2))
hist(FAA3$duration)
hist(FAA3$no_pasg)
hist(FAA3$speed_ground)
hist(FAA3$speed_air)
hist(FAA3$height)
hist(FAA3$pitch)
hist(FAA3$distance)

#Findings from cleaned data- refer report
#Initial Analysis- Correlation

FAA3$aircraft<- as.numeric(FAA3$aircraft=="boeing")
table(FAA3$aircraft)

table1<-cor(FAA3, use = "complete.obs")
plot(FAA3$distance,FAA3$aircraft)
plot(FAA3$distance,FAA3$speed_ground)
plot(FAA3$distance,FAA3$speed_air)
plot(FAA3$distance,FAA3$duration)
plot(FAA3$distance,FAA3$height)
plot(FAA3$distance,FAA3$pitch)

#Model building- Stepwise regression

model1<-lm(distance~aircraft,data=FAA3)
p1<-summary(model1)$coefficients[2,4]
c1<-summary(model1)$coefficients[2,1]

model2<-lm(distance~no_pasg,data=FAA3)
p2<-summary(model2)$coefficients[2,4]
c2<-summary(model2)$coefficients[2,1]

model3<-lm(distance~speed_ground,data=FAA3)
p3<-summary(model3)$coefficients[2,4]
c3<-summary(model3)$coefficients[2,1]

model4<-lm(distance~speed_air,data=FAA3)
p4<-summary(model4)$coefficients[2,4]
c4<-summary(model4)$coefficients[2,1]

model5<-lm(distance~height,data=FAA3)
p5<-summary(model5)$coefficients[2,4]
c5<-summary(model5)$coefficients[2,1]

model6<-lm(distance~pitch,data=FAA3)
p6<-summary(model6)$coefficients[2,4]
c6<-summary(model6)$coefficients[2,1]

model7<-lm(distance~duration,data=FAA3)
p7<-summary(model7)$coefficients[2,4]
c7<-summary(model7)$coefficients[2,1]

names(FAA3)[-7]
p_all<-c(p1,p2,p3,p4,p5,p6,p7)
c_all<-c(c1,c2,c3,c4,c5,c6,c7)

table2<-data.frame(names(FAA3)[-7],p_all,c_all)
table2 <- transform(table2, c_all = ifelse(c_all >0, "Positive", "Negative"))
table2<-arrange(table2, desc(table2$p_all))

#standardizing all Xs
FAA3[,-7] <- scale(FAA3[,-7])
summary(FAA3)

model1s<-lm(distance~aircraft,data=FAA3)
c1s<-summary(model1s)$coefficients[2,1]

model2s<-lm(distance~no_pasg,data=FAA3)
c2s<-summary(model2s)$coefficients[2,1]

model3s<-lm(distance~speed_ground,data=FAA3)
c3s<-summary(model3s)$coefficients[2,1]

model4s<-lm(distance~speed_air,data=FAA3)
c4s<-summary(model4s)$coefficients[2,1]

model5s<-lm(distance~height,data=FAA3)
c5s<-summary(model5s)$coefficients[2,1]

model6s<-lm(distance~pitch,data=FAA3)
c6s<-summary(model6s)$coefficients[2,1]

model7s<-lm(distance~duration,data=FAA3)
c7s<-summary(model7s)$coefficients[2,1]

names(FAA3)[-7]
c_alls<-c(c1s,c2s,c3s,c4s,c5s,c6s,c7s)

table3<-data.frame(names(FAA3)[-7],c_alls,c_alls)
table3 <- transform(table3, c_alls = ifelse(c_alls >0, "Positive", "Negative"))
table3<-arrange(table3, desc(table3$c_alls.1))

table1
table2
table3

table0<-table3[,1] #X variables sorted in order of importance

#all 3 tables point that speed_ground, speed_air and aircraft have highest correlation 
#and also are most significant with largest regression coefficient

#16 Collinearity

model1c<-lm(distance~speed_ground, data=FAA3)
model2c<-lm(distance~speed_air, data=FAA3)
model3c<-lm(distance~speed_air+speed_ground, data=FAA3)

c1c<-summary(model1c)$coefficients[2,1]
c2c<-summary(model2c)$coefficients[2,1]
c3c<-summary(model3c)$coefficients[2:3,1]
cor(FAA3$speed_ground,FAA3$speed_air, use = "complete.obs")
library(car)
vif(model3c)

#as it can be evidenced, the correlation is very high and thus it has caused multicollinearity
#The sign change in model 3 also points to the same as does the high vif for model3c.
#As both variables seem to impact the model by same measure, we will retain the speed ground since it has a lot more data 

#17 Variable Selection
model1v<-lm(distance~speed_ground, data=FAA3)
model2v<-lm(distance~speed_ground+speed_air,data=FAA3)
model3v<-lm(distance~speed_ground+speed_air+aircraft, data=FAA3)
model4v<-lm(distance~speed_ground+speed_air+aircraft+height, data=FAA3)
model5v<-lm(distance~speed_ground+speed_air+aircraft+height+pitch, data=FAA3)
model6v<-lm(distance~speed_ground+speed_air+aircraft+height+pitch+no_pasg, data=FAA3)
model6v<-lm(distance~speed_ground+speed_air+aircraft+height+pitch+no_pasg, data=FAA3)
model7v<-lm(distance~speed_ground+speed_air+aircraft+height+pitch+no_pasg+duration, data=FAA3)

#r-squared values- it is observed rsquared uniformly increases with no. of vars added
r1<-summary(model1v)$r.squared
r2<-summary(model2v)$r.squared
r3<-summary(model3v)$r.squared
r4<-summary(model4v)$r.squared
r5<-summary(model5v)$r.squared
r6<-summary(model6v)$r.squared
r7<-summary(model7v)$r.squared

r_all<-c(r1,r2,r3,r4,r5,r6,r7)
par(mfrow=c(1,1))
plot(r_all)

#18 r-squared adjusted
r1a<-summary(model1v)$adj.r.squared
r2a<-summary(model2v)$adj.r.squared
r3a<-summary(model3v)$adj.r.squared
r4a<-summary(model4v)$adj.r.squared
r5a<-summary(model5v)$adj.r.squared
r6a<-summary(model6v)$adj.r.squared
r7a<-summary(model7v)$adj.r.squared
radj_all<-c(r1a,r2a,r3a,r4a,r5a,r6a,r7a)
plot(radj_all)

#19 AIC values
a1<-AIC(model1v)
a2<-AIC(model2v)
a3<-AIC(model3v)
a4<-AIC(model4v)
a5<-AIC(model5v)
a6<-AIC(model6v)
a7<-AIC(model7v)

a_all<-c(a1,a2,a3,a4,a5,a6,a7)
plot(a_all)

#20 Smaller AIC and larger R-squared values are preferred, both of which converge for model4v
#Thus speed_ground,speed_air,aircraft,height are included in the model- despite the high correlation

#21 Variable selection-automated
model_null<-lm(distance~1,data=FAA3)
model_nospeedair<-lm(distance~speed_ground+aircraft+height+pitch+no_pasg+duration, data=FAA3)
stepAIC(model_null, scope=list(lower=model_null, upper=model_nospeedair), direction = 'forward')
