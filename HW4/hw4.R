# 1(a)
data(leafblotch)
View(leafblotch)
str(leafblotch)
plot(blotch~as.numeric(site),leafblotch,xlab="site",main="blotch against site")
plot(blotch~as.numeric(variety),leafblotch,xlab="variety",main="blotch against variety")

# 1(b)
leafblotch$affected <- leafblotch$blotch*10000
leafblotch$unaffected <- 10000 - leafblotch$affected
View(leafblotch)
fit1b <- glm(blotch~site+variety,leafblotch,family = binomial)
summary(fit1b)
df.residual(fit1b)
deviance(fit1b)
pchisq(deviance(fit1b),72,lower=F)

# 1(c)
fit1c <- glm(blotch~site+variety,leafblotch,family = quasibinomial)
sumary(fit1c)
(sigma2 <- sum(residuals(fit1b,type = "pearson")^2)/df.residual(fit1b))

# 1(d)
plot(residuals(fit1c)~fitted(fit1c),main="Residuals vs Predicted Values",
     xlab="Predicted Values",ylab="Residuals")
# fitted or predict?
plot(residuals(fit1c)~predict(fit1c),main="Residuals vs Predicted Values",
     xlab="Predicted Values",ylab="Residuals")

# 1(e)

miu = 1/(fitted(fit1c)*(1-fitted(fit1c)))
fit_1e = glm(blotch~site+variety,family=quasibinomial,data=leafblotch,weights = miu)
plot(fit_1e)
summary(fit_1e)


# 1(f)
miu = c(rep(0,90))
for (i in 1:90) { 
miu[i] = fitted(meed)[i]^2*(1-fitted(meed)[i])^2
}
fitt = glm(cbind(blotch,1-blotch)~site+variety,family=quasibinomial,data=leafblotch)
fit_1f = glm(cbind(blotch,1-blotch)~site+variety,family=binomial,data=leafblotch,weights = miu*(1-miu))
summary(fitt)

# 3(a)
library(faraway)
library(ggplot2)
library(lme4)
library(RLRsim)
data(lawn)
View(lawn)
ggplot(lawn, aes(y=time, x=machine, shape=manufact, col=speed))+
  geom_point()+xlab("machine")+
  ggtitle("time against machine\ngrouped by speed and manufacturers")

# 3(b)
fit3b <- lm(time~manufact+machine+speed,lawn)
sumary(fit3b)

# 3(c)
fit3c <- lmer(time~manufact+speed+manufact:speed+(1|machine),lawn)
summary(fit3c)

# 3(d)
# test the significance of interaction
fit3c1 <- lmer(time~manufact+speed+manufact:speed+(1|machine),lawn,REML = F)
fit3d1 <- lmer(time~manufact+speed+(1|machine),lawn,REML = F)
anova(fit3d1,fit3c1)
# p-value > 0.05, cannot reject the null, we can remove interaction

fit3d2 <- lmer(time~speed+(1|machine),lawn,REML = F)
fit3d3 <- lmer(time~manufact+(1|machine),lawn,REML = F)
anova(fit3d2,fit3d1)
# p-value > 0.05, we can remove manufact
anova(fit3d3,fit3d1)
# p-value < 0.05, we cannot remove speed
fit3d <- lmer(time~speed+(1|machine),lawn,REML = F)

# 3(e) ?
fit3ef = lmer(time~speed+(1|machine),lawn)
fit3es = lm(time~speed,lawn)
exactLRT(fit3ef,fit3es)

# 3(f)
fit3f <- lmer(time~speed+(1|manufact)+(1|manufact:machine),lawn)
summary(fit3f3)
exactRLRT(fit3f1,fit3f,fit3f2)
exactRLRT(fit3f2,fit3f,fit3f1)

# 3(g)
confint(fit3f, method="boot")
