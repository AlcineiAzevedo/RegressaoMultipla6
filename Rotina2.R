remove(list=ls())
setwd("D:/Backup Pendrive/_PacotesR/_R cran/_R cran/ExpAnalysis3d/data")
D=read.table("Dados3.txt",h=T)

Trat=paste(D$P,D$K)
Bloco=as.factor(D$Rep)
Prod=D$Prod

anv=aov(Prod~Bloco+Trat)
anova(anv)

#zi=a+bxi+cxi²+dyi+fyi²+gxiyi+ ei

Y=Prod
X=cbind(1,D$P,D$P^2,D$K,D$K^2,D$P*D$K)
B=solve(t(X)%*%X)%*%t(X)%*%Y
Yp=X%*%B
C=sum(Y)^2/100
SQreg=t(Yp)%*%Yp-C
SQtrat=84113  
SQdesvReg=SQtrat-SQreg


X=cbind(1,D$P)
B=solve(t(X)%*%X)%*%t(X)%*%Y
Yp=X%*%B
C=sum(Y)^2/100
SQ1c=t(Yp)%*%Yp-C

X=cbind(1,D$P,D$P^2)
B=solve(t(X)%*%X)%*%t(X)%*%Y
Yp=X%*%B
C=sum(Y)^2/100
SQ2c=t(Yp)%*%Yp-C
SQeqx=SQ2c-SQ1c

X=cbind(1,D$P,D$P^2,D$K)
B=solve(t(X)%*%X)%*%t(X)%*%Y
Yp=X%*%B
C=sum(Y)^2/100
SQ3c=t(Yp)%*%Yp-C
SQely=SQ3c-SQ2c


X=cbind(1,D$P,D$P^2,D$K,D$K^2)
B=solve(t(X)%*%X)%*%t(X)%*%Y
Yp=X%*%B
C=sum(Y)^2/100
SQ4c=t(Yp)%*%Yp-C
SQeQy=SQ4c-SQ3c


X=cbind(1,D$P,D$P^2,D$K,D$K^2,D$P*D$K)
B=solve(t(X)%*%X)%*%t(X)%*%Y
Yp=X%*%B
C=sum(Y)^2/100
SQ5c=t(Yp)%*%Yp-C
SQeQy=SQ5c-SQ4c


###########################################
#############################
X=cbind(1,D$P,D$P^2,D$K,D$K^2,D$P*D$K)
B=solve(t(X)%*%X)%*%t(X)%*%Y
Var=diag(solve(t(X)%*%X)*17.8)
B/sqrt(Var)


####################################################
##########################
###
install.packages("ExpAnalysis3d")
library(ExpAnalysis3d)
?ExpAnalysis3d

AjustarRegressao(D,design = 2)
