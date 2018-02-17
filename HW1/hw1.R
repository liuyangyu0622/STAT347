# Problem 1 ---------------------------------------------------------------

install.packages("faraway")
library(faraway)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

#a
#fit the logistic model
load("D:/Study/UChicago/Second Quarter/STAT34700/HW1/HIPP.RData")
hipp=data.frame(HIPP)
lmod <- glm(spikes ~ xN + yN, family = binomial, hipp)
summary(lmod)

#Diagnostics
#Residual Plot
linpred <- predict(lmod)
predprob <- predict(lmod, type="response")
hippre <- mutate(hipp, residuals=residuals(lmod), linpred=predict(lmod))
gdf <- group_by(hippre, cut(linpred, breaks=unique(quantile(linpred,(1:100)/101))))
diagdf <- summarise(gdf, residuals=mean(residuals), linpred=mean(linpred))
plot(residuals ~ linpred, diagdf, xlab="linear predictor",main="Residual Plot")
#QQ plot
qqnorm(residuals(lmod))
#Half-normal plot
halfnorm(hatvalues(lmod))


#b
#interactions
lmodinter = glm(spikes ~ xN + yN + xN:yN , family = binomial, data=hipp)
sumary(lmodinter)
anova(lmod, lmodinter, test = "Chi")

#non-linear function
hipp$distance=sqrt((hipp$xN)^2+(hipp$yN)^2)
#use distance describe the location in the cage
lmodimprove=glm(spikes ~ xN + yN + distance , family = binomial, data=hipp)
summary(lmodimprove)
anova(lmod,lmodimprove,test="Chi")

#c
hippsub = hipp[1:NROW(hipp) %% 20 == 1, ]
lmodsub <- glm(spikes ~ xN + yN + distance, family = binomial, data=hippsub)
sumary(lmodsub)

#d
hipp1=subset(hippsub, spikes == 1)
hipp0=subset(hippsub, spikes == 0)
hipp0rs=hipp0[sample(1:nrow(hipp0), 100, replace=FALSE),]
hipptotal=rbind(hipp1,hipp0rs)
lmodtotal=glm(spikes ~ xN + yN + distance, family = binomial, data=hipptotal)
summary(lmodtotal)

#e
#fit the model with history information
lmodhis=glm(spikes ~ xN + yN + distance + spikes.hist.1 + spikes.hist.2 + spikes.hist.3
            + spikes.hist.4 + spikes.hist.5 + spikes.hist.6 + spikes.hist.7 + spikes.hist.8
            + spikes.hist.9 + spikes.hist.10 + spikes.hist.11 + spikes.hist.12 + spikes.hist.13
            + spikes.hist.14 + spikes.hist.15 + spikes.hist.16 + spikes.hist.17 + spikes.hist.18
            + spikes.hist.19 + spikes.hist.20, family = binomial, data=hipp)
summary(lmodhis)
anova(lmod,lmodhis,test="Chi")

#plot of observed proportion vs predicted probability
#prediction with history information
linpredhis <- predict(lmodhis)
predprobhis <- predict(lmodhis, type="response")
dfsm <- na.omit(hipp)
dfsm <- mutate(dfsm, predprobhis=predict(lmodhis,type="response"))
gdf <- group_by(dfsm, cut(linpredhis, breaks=unique(quantile(linpredhis,(1:100)/101))))
hldf <- summarise(gdf, spikes=sum(spikes), ppred=mean(predprobhis), count=n())
hldf <- mutate(hldf, se.fit=sqrt(ppred*(1-ppred)/count))
ggplot(hldf,aes(x=ppred,y=spikes/count,ymin=spikes/count-2*se.fit,ymax=
                  spikes/count+2*se.fit))+
       geom_point() + geom_linerange(color=grey(0.75)) + geom_abline(intercept=0,slope=1) +
       xlab("Predicted Probability")+ylab("Observed Proportion")

#prediction without history information
linpred <- predict(lmodimprove)
predprob <- predict(lmodimprove, type="response")
hippsm <- na.omit(hipp)
hippsm <- mutate(hippsm, predprob=predict(lmodimprove,type="response"))
ghipp <- group_by(hippsm, cut(linpred, breaks=unique(quantile(linpred,(1:100)/101))))
hlhipp <- summarise(ghipp, spikes=sum(spikes), ppred=mean(predprob), count=n())
hlhipp <- mutate(hlhipp, se.fit=sqrt(ppred*(1-ppred)/count))
ggplot(hlhipp,aes(x=ppred,y=spikes/count,ymin=spikes/count-2*se.fit,ymax=
                  spikes/count+2*se.fit))+
  geom_point() + geom_linerange(color=grey(0.75)) + geom_abline(intercept=0,slope=1) +
  xlab("Predicted Probability")+ylab("Observed Proportion")

# Problem 2 ---------------------------------------------------------------

#a
logdose = c(0,1,2,3,4,5)
mortality = c(0,2/9,3/8,5/7,7/9,10/11)
mort = c(0,2,3,5,7,10)
unmort = c(7,7,5,2,2,1)
dose = data.frame(mort,unmort,mortality,logdose)
dose$total = dose$mort + dose$unmort

#plot the mortality rate vs log dose
plot(c(0,6),c(0,1.5),type="n",xlab="log dose",ylab="mortality rate",main="Mortality Rate v.s. Log Dose")
points(dose$logdose,dose$mortality,pch=16)

#fit the logistic model
glmfit = glm(cbind(mort,unmort) ~ logdose, family = binomial(), data=dose)
sumary(glmfit)

#superimpose the fitted values
x = seq(0,6,0.1)
pre=predict(glmfit,data.frame(logdose=x),type="response",se.fit=TRUE)
lines(x,pre$fit)

#b
#estimate LD50
LD50=-glmfit$coef[1]/glmfit$coef[2]
glmfitsum=summary(glmfit)
#use delta method to calculate the CI for LD50
dr <- c(-1/glmfit$coef[2],glmfit$coef[1]/glmfit$coef[2]^2)
se = sqrt(dr %*% glmfitsum$cov.un %*% dr)[,]
c(LD50-1.96*se,LD50+1.96*se)

#c
#get the covariance matrix of coefficients
cm = vcov(glmfit)
#solve the function
a = glmfit$coef[2]^2-1.96^2*cm[2,2]
b = 2*(1.96^2*cm[1,2]-glmfit$coef[1]*glmfit$coef[2])
c = glmfit$coef[1]^2-1.96^2*cm[1,1]
ro2 = (-b + sqrt(b^2-4*a*c))/(2*a)
ro1 = (-b - sqrt(b^2-4*a*c))/(2*a)
#get the 95% CI for pho
CI = c(ro1, ro2)
ro = glmfit$coef[1]/glmfit$coef[2]

#e

#nonparametric
nrows=dim(dose)[1]
aa=sapply(1:nrows,pr<-function(x) rep(dose$logdose[x],dose$total[x]),simplify="array")
aaa=unlist(aa)
# Insert affected 1's and unaffected 0's for each concentration level
bb=sapply(1:nrows,pr<-function(x) c(rep(1,dose$mort[x]),rep(0,dose$unmort[x])),simplify="array")
bbb=unlist(bb)
# Combine concentration and binary vector of affected/unaffected.
casedose=cbind(aaa,bbb)
# Create a list that can be sampled.
data=split(casedose,row(casedose))
# Now 500 times resample from the cases and estimate a model
set.seed(123)
sampsize=500
cfsnon=matrix(rep(0,sampsize*2),c(sampsize,2))
for (j in seq(sampsize))
{
  n=length(data)
  ss=sample(data,n,replace=TRUE)
  # Make the sample into a nx2 matrix
  dd=t(matrix(unlist(ss),nrow=2,ncol=n))
  # Make the sampled list into a data frame with counts for each concentration
  # level found in the sample
  ee=table(dd[,1],dd[,2])
  # Reestimate the model from the resampled data.
  glmsam2=glm(ee[,1:2]~dose$logdose,family=binomial())
  cfsnon[j,]=glmsam2$coefficients
}
phonon=cfsnon[,1]/cfsnon[,2]
phonondata=data.frame(phonon)
#Calculate 95% CI for pho
apply(phonondata,2,function(x) quantile(x,c(0.025,0.975),na.rm=TRUE))

#parametric
preds=matrix(glmfit$fitted.values)
nums=dose$total
set.seed(123)
# Number of simulations.
sampsiz=500

# Matrix of coeficients of each sample. sampsiz x 2
cfs=matrix(rep(0,sampsiz*2),c(sampsiz,2))
for (j in seq(sampsiz))
{
  # Simulate the data for each log-concentration level
  aff=mapply(rbinom,rep(1,length(nums)),nums,preds)
  unaff=nums-aff
  # Fit model to simulated data
  glmsam1=glm(cbind(aff,unaff)~dose$logdose, family=binomial())
  # Store coefficients.
  cfs[j,]=glmsam1$coefficients
}
pho=cfs[,1]/cfs[,2]
phodata=data.frame(pho)
#Calculate 95% CI for pho
apply(phodata,2,function(x) quantile(x,c(0.025,0.975)))

#e
#transform the predictor
dose$logdosenull = dose$logdose - 4
#fit the model
nullfit=glm(cbind(mort,unmort) ~ logdosenull - 1, family = binomial,data = dose)
#get LR(4)
LR4=2*(logLik(glmfit)-logLik(nullfit))
#calculate the p-value
pchisq(11.59227,df=1,lower.tail = FALSE)


# Problem 3 ---------------------------------------------------------------

#a
data(orings)
oringsfit=glm(cbind(damage, 6-damage) ~ temp, family = binomial, orings)
sumary(oringsfit)

#b
orings$binary = orings$damage
orings[1,3]=1
fit_binary = glm(binary ~ temp, family = binomial, data=orings)
sumary(fit_binary)

#c

#d
orings1=subset(orings, binary == 1)
orings0=subset(orings, binary == 0)
orings0rs=orings0[sample(1:nrow(orings0), 7, replace=FALSE),]
oringstotal = rbind(orings1, orings0rs)
fit_remedy = glm(binary ~ temp, family = binomial, data = oringstotal)
summary(fit_remedy)

#e
