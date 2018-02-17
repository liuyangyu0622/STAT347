load("D:/Study/UChicago/Second Quarter/STAT34700/HW2/Mortality.RData")

#b
data1999=subset(Mortality,Year==1999)
data2013=subset(Mortality,Year==2013)

popu1999=NULL
for (i in 1:30){
  if (i %% 3 ==0){
    popu1999 = c(popu1999,data1999$Population[i])
  }
}
age=c(45,46,47,48,49,50,51,52,53,54)
plot(age,popu1999,xlab="Age",ylab="Population",main="Age distribution in 1999")

popu2013=NULL
for (i in 1:30){
  if (i %% 3 ==0){
    popu2013 = c(popu2013,data2013$Population[i])
  }
}
age=c(45,46,47,48,49,50,51,52,53,54)
plot(age,popu2013,xlab="Age",ylab="Population",main="Age distribution in 2013")

#c


# Problem #2 --------------------------------------------------------------

install.packages("faraway")
library(faraway)
#a
data(turtle)
View(turtle)
turtle$total = turtle$male + turtle$female
turtle$fraction = turtle$male / turtle$total
plot(turtle$temp,turtle$fraction)

#b
fit_2b = glm(cbind(male,female)~temp,family=binomial,data=turtle)
summary(fit_2b)
pchisq(deviance(fit_2b),df.residual(fit_2b),lower=FALSE)
#does not fit well

#c
no

#d
halfnorm(residuals(fit_2b))
#no outlier

#e
elogits = with(turtle,log((male+0.5)/(total- male+0.5)))
plot(turtle$temp,elogits)

#f
fit_2d = glm(cbind(male,female)~temp + temp^2,family=binomial,turtle)
summary(fit_2d)

#g???
var(turtle[1:3,5])

#h
newturtle = data.frame(matrix(rep(0,25),nrow=5,ncol=5))
newturtle$X1 = c(27.2,27.7,28.3,28.4,29.9)
male=NULL
for (i in 1:15){
  if (i %% 3 ==1){
    male = c(male,turtle$male[i]+turtle$male[i+1]+turtle$male[i+2])
  }
}
newturtle$X2 = male

total=NULL
for (i in 1:15){
  if (i %% 3 ==1){
    total = c(total,turtle$total[i]+turtle$total[i+1]+turtle$total[i+2])
  }
}
newturtle$X4 = total
newturtle$X3 = newturtle$X4 - newturtle$X2
newturtle$X5 = newturtle$X2 / newturtle$X4
colnames(newturtle)=c("temp","male","female","total","proportion")

fit_2h = glm(cbind(male,female) ~ temp, family=binomial, data=newturtle)
summary(fit_2h)
pchisq(deviance(fit_2h),df.residual(fit_2h),lower=FALSE)
pchisq(64.429-14.863,1,lower=FALSE)


# Problem #3 --------------------------------------------------------------

#a
data(chredlin)
help(chredlin)
hist(chredlin$involact,breaks=seq(0,2.3,by=0.05))

#b
fit_3b = glm(involact ~ race + fire + theft + age + log(income), family=gaussian, data=chredlin)
summary(fit_3b)
fit_3b1 = lm(involact ~ race + fire + theft + age + log(income), data=chredlin)
summary(fit_3b1)

#c
residuals(fit_3b1)
plot(fit_3b1)

#d
chredlin$binary=rep(1,47)
for (i in 1:47){
  if (chredlin[i,5]==0){
    chredlin[i,8]=0
  }
}

fit_3d = glm(binary ~ race + fire + theft + age + log(income), family=binomial, data=chredlin)
summary(fit_3e)
#sparse+overestimate sd Hauck-Donner effect

#e
fit_3e = glm(binary ~ race + age, family=binomial, data=chredlin)
summary(fit_3f)

fit_3er = glm(binary ~ race, family=binomial, data=chredlin)
summary(fit_3fr)

fit_3ea = glm(binary ~ age, family=binomial, data=chredlin)
summary(fit_3fa)

#teat age
pchisq(18.296-9.2286,1,lower=FALSE)
#test race
pchisq(45.408-9.2286,1,lower=FALSE)

#f
plot(chredlin$race,chredlin$age,pch="")
text(chredlin$race,chredlin$age,chredlin$binary,cex=1.0)

#g
fit_3g = glm(binary ~ race + age, family=binomial(link=probit), data=chredlin)
summary(fit_3g)

a=fitted.values(fit_3e)
b=fitted.values(fit_3g)
plot(fitted.values(fit_3f),fitted.values(fit_3g),main="Plot of Fitted Values in logit and porbit Model",xlab)
