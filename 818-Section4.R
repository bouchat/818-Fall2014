#PS818 Section 4
#26 September 2014
#Quantiles, GLM, etc.

setwd("/Users/sbouchat/Dropbox/WISC/Classes/2013-Fall/MLE/ProblemSets")

library(foreign) 
library(xtable)  
library(MASS) 
library(boot)
library(graphics)
library(arm)


cox.data<-read.dta("/Users/sbouchat/Downloads/coxappend.dta")

#We will use glm to fit lots of models
help(glm)

model1<-glm(smdp~enpv+enps+eneth,family=binomial(link="logit"),data=cox.data)

summary(model1)

coefficients<-coef(model1)
var.covar.matrix<-vcov(model1)

#Can try different specifications if we want

model2<-glm(smdp~enpv+enps+eneth,family=binomial(link="probit"),data=cox.data)

summary(model2)

#Any differences here?

#We are going to use the fact that MLE is asymptotically consistent and asymptotically normally distributed to run some simulations
#lets draw 10000 coefficient vectors from a mv normal with mean point estimates
#variance is variance.covariance.matrix
beta.tilde<-mvrnorm(10000,coefficients,var.covar.matrix)

#Let's look at predicted probability of smdp by level of enps

#First, we generate a vector of values of enps from min to max by .01 (or whatever range we want)
enps.values <- seq(from=min(cox.data$enps),to=max(cox.data$enps),by=.1)

#maybe we want to look plus minus a standard deviation
enps.other.values<-seq(from=mean(cox.data$enps)-sd(cox.data$enps),to=mean(cox.data$enps)+sd(cox.data$enps),by=.01)

#Now we make a vector of all of the xvalues at their mean

x.vector<-c(1,#intercept
           mean(cox.data$enpv),#set enpv to the mean
            NA,#this is where that enps values vector will go eventually
            median(cox.data$eneth)#we hold eneth at median for kicks
            )

#Now we make a matrix-ify the x.vector into the number of columns needed for enps.values
X.matrix<-matrix(x.vector,nrow=length(x.vector),ncol=length(enps.values))

#Or for the other values
X.matrix.2<-matrix(x.vector,nrow=length(x.vector),ncol=length(enps.other.values))

#Notice that the NA slot for enps is three rows down (including the intercept)
#We replace the third row down from the top with the matrix of filler values that we made
X.matrix[3,]<-enps.values

X.matrix.2[3,]<-enps.other.values

help(inv.logit)

#Now we take all of the coefficient estimates from the beta tilde matrix 
#We multiply them by the xvalues we generated to get the linear part of the model
#and we use the link function to get the matrix of predicted probabilities

pred.prob<-inv.logit(beta.tilde %*% X.matrix)

#We find the quantiles of all of these predicted probabilities to plot

help(apply)
help(quantile)

pred.prob<-apply(pred.prob, 2, quantile, c(0.025,0.975))

#we set up the plot

plot(enps.values, pred.prob[1,], col="white", xlim=c(min(cox.data$enps), max(cox.data$enps)), 
     ylim=c(0,1),
     xlab = "range of enps values", 
     ylab = "Predicted Probability of whatever smdp is",
     lwd=3, main = "Effect of enps on smdp decisions")

segments(x0=enps.values, y0=pred.prob[1,], 
         x1=enps.values, y1=pred.prob[2,], 
         col="blue", lwd=0.05)

dev.off()