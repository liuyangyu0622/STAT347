library(geeM)
LinkFun <- function(mu){return(log(mu/(1-mu)))}
InvLink <- function(theta){return(exp(theta)/(1+exp(theta)))}
InvLinkDeriv <- function(theta){return(exp(theta)/(1+exp(theta))^2)}
VarFun <- function(mu){return(mu^2*(1-mu)^2)}
FunList <- list(LinkFun, VarFun, InvLink, InvLinkDeriv)
meed=geem(blotch~site+variety,family=FunList,data=leafblotch)
summary(meed)
