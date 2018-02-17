
# Problem 1 ---------------------------------------------------------------

install.packages("faraway")
library(faraway)
data(pneumo)

normal = pneumo$Freq[1:8]
mild = pneumo$Freq[9:16]
severe = pneumo$Freq[17:24]
data1 = data.frame(normal,mild,severe)
data1$year = pneumo$year[1:8]
data1$sum = data1$normal + data1$mild + data1$severe

#a
pneumo$proportion=rep(0,24)
for (i in 1:8){
  pneumo[i,4]=pneumo$Freq[i]/data1$sum[i]
}

for (i in 9:16){
  pneumo[i,4]=pneumo$Freq[i]/data1$sum[i-8]
}

for (i in 17:24){
  pneumo[i,4]=pneumo$Freq[i]/data1$sum[i-16]
}

install.packages("ggplot2")
library(ggplot2)
pneumo$status=factor(pneumo$status,levels=c("normal","mild","severe"))
ggplot(pneumo, aes(x=year, y=proportion, group=status, linetype=status))+geom_line()

#b
install.packages("nnet")
library(nnet)
mod_1b = multinom(cbind(normal,mild,severe) ~ year, data1)
sumary(mod_1b)
fitted(mod_1b)

#c
mod_1c = multinom(cbind(normal,mild,severe) ~ log(year), data1)
summary(mod_1c)


#d
pneumo$fitted = rep(0,24)
pneumo$fitted[1:8] = fitted(mod_1c)[,1]
pneumo$fitted[9:16] = fitted(mod_1c)[,2]
pneumo$fitted[17:24] = fitted(mod_1c)[,3]
ggplot(pneumo, aes(x=year, y=fitted, group=status, linetype=status))+geom_line()

#e
library(MASS)
pneumo1=pneumo[,-5]
pneumo1$status=factor(pneumo1$status,levels=c("normal","mild","severe"))
mod_1e = polr(status ~ year,weights = Freq, data = pneumo1)
summary(mod_1e)
ilogit(3.96) 

#f
mod_1f = polr(status ~ log(year),weights = Freq, data = pneumo1)
summary(mod_1f)
fitted(mod_1f)[1:8,]

#g
pneumo1$fitted = rep(0,24)
pneumo1$fitted[1:8] = fitted(mod_1f)[1:8,1]
pneumo1$fitted[9:16] = fitted(mod_1f)[1:8,2]
pneumo1$fitted[17:24] = fitted(mod_1f)[1:8,3]
ggplot(pneumo1, aes(x=year, y=fitted, group=status, linetype=status))+geom_line()

#h
data1$NotNormal = data1$mild + data1$severe
binmod = glm(cbind(NotNormal,normal) ~ year, data1, family=binomial)
summary(binmod)
exp(0.096)

#i
binmod2 = glm(cbind(mild,severe) ~ year, data1, family=binomial)
summary(binmod2)
fitted(binmod2)
pchisq(4.0767-1.7284,1,lower=FALSE)


#j
fitted(binmod2)
fitted(binmod)

prob_disease = fitted(binmod)
prob_normal = 1-prob_disease
prob_mild = prob_disease * fitted(binmod2)
prob_severe = prob_disease * (1-fitted(binmod2))

pneumo$fitted2 = rep(0,24)
pneumo$fitted2[1:8]=prob_normal
pneumo$fitted2[9:16]=prob_mild
pneumo$fitted2[17:24]=prob_severe
ggplot(pneumo, aes(x=year, y=fitted2, group=status, linetype=status))+geom_line()


# Problem 2 ---------------------------------------------------------------

data(quine)

#a

#additive
mod_2a = glm(Days ~ ., family=poisson, quine)
sumary(mod_2a)
#check dispersion
dp = sum(residuals(mod_2a,type="pearson")^2)/mod_2a$df.res
dp
#dispersion
mod_2a2 = glm(Days ~ ., family=quasipoisson, quine)
sumary(mod_2a2)
#drop non-signicant predictors
drop1(mod_2a2,test="F")

#interaction

mod_2aint = glm(Days ~ .^2, family=poisson, quine)
sumary(mod_2aint)
#check dispersion
dpint = sum(residuals(mod_2aint,type="pearson")^2)/mod_2aint$df.res
dpint
#dispersion
mod_2aint2 = glm(Days ~ .^2, family=quasipoisson, quine)
sumary(mod_2aint2)
#drop non-significant interactions
drop1(mod_2aint2,test="F")
#final model with interactions and dispersion
mod_2aintf = glm(Days ~ Eth + Sex + Age + Lrn + Eth:Age + Sex:Age, family = quasipoisson, quine)
sumary(mod_2aintf)

#compare the additive and interaction model
anova(mod_2a2, mod_2aintf, test = "F")


#f
meanf=NULL;varf=NULL
for (i1 in levels(quine[,1]))
  for (i2 in levels(quine[,2]))
    for (i3 in levels(quine[,3]))
      for (i4 in levels(quine[,4])){
        temp=quine[colSums(t(quine[,1:4])==c(i1,i2,i3,i4))==4,5]
        meanf=c(meanf,mean(temp))
        varf=c(varf,sum((temp-mean(temp))^2)/(length(temp)-1))
      }
ggplot(data.frame(x=meanf[!is.na(meanf)],y=varf[!is.na(varf)]),aes(x,y))+
  geom_point()+
  xlab("Mean")+
  ylab("Variance")+
  ggtitle("Variance vs Mean")+
  stat_function(fun=function(x) x+x^2/2.154)

nu=1/lm(varf-meanf~I(meanf^2)-1)$coef
nu
summary(lm(varf-meanf~I(meanf^2)-1))

#additive model
fit_nb1 = glm(Days~.,family=negative.binomial(nu),quine)
sumary(fit_nb1)
#interaction model
fit_nb2 = glm(Days~.^2,family=negative.binomial(nu),quine)
sumary(fit_nb2)
drop1(fit_nb2, test = "F" )
fit_nbf = glm(Days~Eth + Sex + Age + Lrn + Eth:Age + Sex:Age,family=negative.binomial(nu),quine)
sumary(fit_nbf)

#compare additive model and interaction model
anova(fit_nb1, fit_nbf, test = "F")

plot(fit_nbf)
plot(mod_2aintf)


# Problem 3 ---------------------------------------------------------------

#a
fiji=read.table("fiji.txt",head=TRUE)
fiji$marriage=factor(fiji$marriage)
fiji$abode=factor(fiji$abode)
fiji$edu=factor(fiji$edu)
fiji$resp = fiji$average*fiji$tot
fiji$resp = round(fiji$resp)

fiji=fiji[fiji$resp>0,]
fit_3aint=glm(resp ~ offset(log(tot))+(marriage+edu+abode)^2,family=poisson,fiji)
sumary(fit_3aint)
fit_3a = glm(resp ~ offset(log(tot))+marriage+edu+abode,family=poisson,fiji)
sumary(fit_3a)

#goodness of fit
pchisq(deviance(fit_3a),df.residual(fit_3a),lower=FALSE)
#residual
plot(fit_3a)
#outlier
halfnorm(residuals(fit_3a))

#dispersion
fit_3aq = glm(resp ~ offset(log(tot))+marriage+edu+abode,family=quasipoisson,fiji)
sumary(fit_3aq)

plot(log(fitted(fit_3a)),log((fiji$resp-fitted(fit_3a))^2),
     xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2))
abline(0,1)


#c
fits=summary(fit_3a)
xc=as.vector(c(1,0,1,0,0,0,0,1,0,0))
u=exp(fits$coef[,1]%*%xc)
dr = t(u %*% xc)
sec = sqrt(t(dr)%*%fits$cov.un%*%dr)
CIc = c(u-1.96*sec,u+1.96*sec)

#d
xd = c(1,0,0,0,0,1,0,0,1,1)
ud = exp(fits$coef[,1]%*%xd)
drd = t(ud %*% xd)
sed = sqrt(t(drd)%*%fits$cov.un%*%drd)
CId = c(ud-1.645*sed,ud+1.645*sed)
