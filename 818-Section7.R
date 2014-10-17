#PS818 Section 7
#17 October 2014
#Ordered and Multinomial Responses, Parallel Regressions I

setwd("/Users/sbouchat/Dropbox/WISC/Classes/2014-Fall/TAing/818/818Section")

library(foreign) 
library(xtable)  
library(MASS) 
library(boot)
library(graphics)
library(arm)
library(stargazer)

#######################################
# 1: Ordered Logit
#######################################
# More fun with dead snails
data(snails)

#Just turning our DV into a factor variable...

snails$deathrange<-NA

snails$deathrange[snails$Deaths==0] <-0
snails$deathrange[snails$Deaths==1 | snails$Deaths==2 | snails$Deaths==3 | snails$Deaths==4 | snails$Deaths==5 | snails$Deaths==6 | snails$Deaths==7] <-1
snails$deathrange[snails$Deaths==9 | snails$Deaths==10 | snails$Deaths==11 | snails$Deaths==12 | snails$Deaths==14 | snails$Deaths==16] <-2

snails$deathfactor<-as.factor(snails$deathrange)
attach(snails)

#Estimating the model
ologit<-polr(deathfactor~ Temp+Rel.Hum+Exposure, data=snails, Hess=TRUE, method="logistic")

#Some different ways to get a summary
coef(summary(ologit))
ologit.tab<-xtable(coef(summary(ologit)))
stargazer(ologit, title="Results", align=TRUE, dep.var.labels=c("Death"), covariate.labels=c("Temperature, Rel Humidity, Exposure"))

#What about counterfactual scenarios? Let's vary relative humidity over its range
coef<-coef(ologit)
vcov<-vcov(ologit)

#But look at what vcov produces
vcov

#We don't want these last two rows or last two columns
var.covar<-vcov[-4:-5,-4:-5]

betatil<-t(mvrnorm(100000,coef,var.covar))
tau<-ologit$zeta
X<-cbind(mean(Temp), min(Rel.Hum):max(Rel.Hum), mean(Exposure))

p.0<-plogis(tau[1]-X%*%coef)
p.1<-plogis(tau[2]-X%*%coef) - plogis(tau[1]-X%*%coef)
p.2<-1-plogis(tau[2]-X%*%coef)


plot(min(Rel.Hum):max(Rel.Hum),p.0,type="l",col="violetred4",ylim=c(0,1),lwd=2,ylab="Predicted Probability of Snail Death",xlab="Relative Humidity")
lines(min(Rel.Hum):max(Rel.Hum),p.1,col="darkolivegreen2",lwd=2)
lines(min(Rel.Hum):max(Rel.Hum),p.2,col="royalblue2",lwd=2)
legend("topright",legend=c("P(0): Low Death","P(1): Medium Death","P(2): High Death"),col=c("violetred4","darkolivegreen2","royalblue2"),lty=1,lwd=2)


#Two scenarios: minimum and maximum exposure

temp.values<-seq(min(Temp), max(Temp), by=.1)
X.minex<-cbind(temp.values, mean(Rel.Hum), min(Exposure))
X.maxex<-cbind(mean(Temp), min(Rel.Hum):max(Rel.Hum), max(Exposure))

p.0.minex<-plogis(tau[1]-X.minex%*%betatil)
p.1.minex<-plogis(tau[2]-X.minex%*%betatil) - plogis(tau[1]-X.minex%*%betatil)
p.2.minex<-1-plogis(tau[2]-X.minex%*%betatil)

pred.0.minex<-apply(p.0.minex, 1, quantile, c(.025,.975))
pred.1.minex<-apply(p.1.minex, 1, quantile, c(.025,.975))
pred.2.minex<-apply(p.2.minex, 1, quantile, c(.025,.975))



plot(temp.values, pred.0.minex[1,], col="white", 
xlim=c(min(Temp),max(Temp)), ylim=c(0,1),
     xlab = "Temperature", 
     ylab = "Predicted Death",
     lwd=3, type="l", main = "Predicted Effect of Temperature on Death")
	 
	 segments(x0=temp.values, y0=pred.1.minex[1,], 
	          x1=temp.values, y1=pred.1.minex[2,], 
	          col="violetred3", lwd=2)
		 	 segments(x0=temp.values, y0=pred.2.minex[1,], 
		 	          x1=temp.values, y1=pred.2.minex[2,], 
		 	          col="royalblue2", lwd=2)
legend("topleft",legend=c("P(0): Low Death","P(2): High Death"),col=c("violetred3","royalblue2"),lty=1,lwd=2)



