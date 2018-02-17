library("MASS")
library("nnet")
library(dplyr)
library(ggplot2)
library(gridExtra)
# Multinom on housing data
gts=group_by(housing,Type,Sat)
gtsf=summarise(gts,Freq=sum(Freq))
gtf=group_by(gtsf,Type)
gtfm=mutate(gtf,etotal=sum(Freq),prop=Freq/etotal)

gcs=group_by(housing,Cont,Sat)
gcsf=summarise(gcs,Freq=sum(Freq))
gcf=group_by(gcsf,Cont)
gcfm=mutate(gcf,etotal=sum(Freq),prop=Freq/etotal)

g1=ggplot(gcfm,aes(x=Cont,y=prop,group=Sat,linetype=Sat))+geom_line()

g2=ggplot(gtfm,aes(x=Type,y=prop,group=Sat,linetype=Sat))+geom_line()

grid.arrange(g1,g2)


# Make the data into a matrix
hous=as.matrix(housing$Freq)
dim(hous)=c(3,24)
hous=t(hous)
# Get all factor predictor combinations
housepreds=housing[seq(1,72,3),2:4]
rownames(housepreds)=seq(24)

ahouse.mult=multinom(hous~Infl+Type+Cont,data=housepreds)
ahouseind.mult=multinom(hous~1,data=housepreds)

summary(ahouse.mult)

housind.mult=multinom(Sat~1,data=housing,weights=Freq)
house.mult=multinom(Sat~Infl+Type+Cont,data=housing,weights=Freq)


# Get chisq and deviance statistics out of multinom output
gof<-function(mod,ldata)
{
  # Number of predictor combinations
  ll=dim(ldata)[1]
  # Number of outcomes
  cc=dim(ldata)[2]

  # Total for each combination of levels
  RR=rowSums(ldata)
  # Total in each outcome
  CC=colSums(ldata)
  # proportions in column sums - marginal proportions of outcomes
  CC=t(matrix(CC/sum(CC)))
  CCC=rep(1,ll)%*%CC

  # Matrix of row sums
  RRR=RR%*%t(rep(1,cc))

  # Fitted probs
  PFIT=mod$fitted.values
  FIT=PFIT[!duplicated(PFIT),]
  # Expected counts.
  EXP=FIT*RRR
  # Chisq stat for mod
  CHISQ=sum((ldata-EXP)^2/EXP)
  # Degrees of freedom - Number of predictors * (Number of outcomes - 1) - Number of estimated parameters.
  # Number of estimated parametes = Number of columns in model matrix * (Number of outcomes - 1)
  dof=(ll*(cc-1)-length(coefficients(mod)))

  # Deviance
  DEV=2*sum(ldata*log(ldata/EXP))
  # Likelihood ratio to independent model
  LR2IND=sum(EXP*log(FIT/CCC))
  RET=list(chisq=CHISQ,dof=dof,dev=DEV,lr2ind=LR2IND,fit=FIT,exp=EXP,ccc=CCC)
  return(RET)
}

RET=gof(ahouse.mult,hous)
RETind=gof(ahouseind.mult,hous)

# Getting probabilities
ccc=coefficients(ahouse.mult)
mmm=model.matrix(ahouse.mult)
pred=mmm%*%t(ccc)
predd=cbind(rep(0,24),pred)
epred=exp(predd)
sepred=matrix(rowSums(epred))
probs=epred/(sepred%*%t(rep(1,3)))
# Verfiy you got the same thing...
probs-ahouse.mult$fitted.values
cbind(housepreds,ahouse.mult$fitted.values)

# Add an interaction
ahouse_b.mult=multinom(hous~Infl+Type*Cont,data=housepreds)
RETb=gof(ahouse_b.mult,hous)

delta_dev=RET$dev-RETb$dev
delta_dof=RET$dof-RETb$dof

pchisq(delta_dev,delta_dof)

# Proportional odds model
# The \thetas are \theta_Low and \theta_Med
house.plr=polr(Sat~Infl+Type+Cont,data=housing,weights=Freq)


# Likelihood ratio statistic between multinomial and proporional odds model. To compare two likelihoods you can use the case based deviance reported by the polr and multinom functions.
lrdel=house.plr$deviance-house.mult$deviance
dfdel=house.mult$edf-house.plr$edf
# Check lrs on chisq with dfdel d.f
1-pchisq(lrdel,dfdel)

# Merge two levels -left with two levels - do binomial
nhouse=cbind(hous[,1]+hous[,2],hous[,3])
nhouse.bin=glm(nhouse~Infl+Type+Cont,data=housepreds,family=binomial)
# Compare coefficients - quite similar
nhouse.bin
house.plr

probs.bin=predict(nhouse.bin,type="response")

# Compare probability of high Sat from binomial and ordinal model
cbind(1-probs.bin,probs.plr[,3])

# Use Poisson model to estimate the multinomial model
# Make the Sat factor unordered so the contrasts used are treatment.
housing$Sat=factor(housing$Sat, ordered=FALSE)
# Infl*Cont*Type enumerates the possible configurations of the predictor factors
# Each one will get an estimated coeficient. The real multinomial model is read
# the coeficients containing Sat
housepois=glm(Freq~Sat*(Infl+Cont+Type)+Infl*Cont*Type,data=housing, family=poisson())

ii=grep("Sat",names(housepois$coefficients))
cc=housepois$coefficients[ii]
cc

# To add an interaction term, we need to always multiply it by the Sat factor
housepoisa=glm(Freq~Sat*(Infl+Cont+Type+Cont*Type)+Infl*Cont*Type,data=housing, family=poisson())
# Compare the two models
an=anova(housepois,housepoisa)
# P-value
1-pchisq(an$Deviance[2],an$Df[2])
