#PS818 Section 6
#10 October 2014
#Crossvalidation


library(foreign) 
library(xtable)  
library(MASS) 
library(boot)
library(graphics)
library(arm)
library(bootstrap)
library(ROCR)

# More fun with dead snails
data(snails)

snails$deathHL<-NA
snails$deathHL[snails$Deaths>=9]<-1
snails$deathHL[snails$Deaths<9]<-0
attach(snails)


# Basic cross-validation using cv.glm

# Process for cross-validation
# 1. Divide data into k groups
#    (for logit, pick phat cutoff for 1 / 0)
# 2. Fit k-1 groups; get Bhat, use Bhat to predict kth group
# 3. Rotate
# 4. Compare cross-validated data to real data


model1<-glm(deathHL ~ Temp,
           family = binomial(link = "logit"), data = snails)

model2<-glm(deathHL ~ Temp+Rel.Hum+Exposure,
           family = binomial(link = "logit"), data = snails)


#6-fold cross-validation
cv.err1 <- cv.glm(snails, model1, K=6)
cv.err2 <- cv.glm(snails, model2, K=6)
summary(cv.err1)
cv.err1$delta #first one is the 6-fold error; second is leave-one-out
cv.err2$delta


# Today we'll be doing crossvalidation with crossval. You can also do it by hand entirely or with cv.glm if the problem allows (hint: you cannot use cv.glm on your problem set)
help(crossval)

x<-as.matrix(cbind(Temp, Rel.Hum, Exposure))
x2<-as.matrix(cbind(Temp))
y<-as.vector(deathHL)

beta.fit<-function(x,y){
	fit.1<-glm(y~x[,1]+x[,2]+x[,3], family=binomial(link="logit"))
}

beta.predict<-function(fit.1, x){
	inv.logit(cbind(1,x)%*%coef(fit.1))
}

beta.fit2<-function(x2,y){
	fit.2<-glm(y~x2, family=binomial(link="logit"))
}

beta.predict2<-function(fit.2,x2){
	inv.logit(cbind(1,x2)%*%coef(fit.2))
}

results1<-crossval(x,y,beta.fit, beta.predict,ngroup=10)
results1
results2<-crossval(x2,y,beta.fit2, beta.predict2,ngroup=10)
pred.1<-prediction(results1$cv.fit, deathHL)
pred.2<-prediction(results2$cv.fit, deathHL)
roc1<-performance(pred.1, "tpr", "fpr")
roc2<-performance(pred.2, "tpr","fpr")
plot(roc1, col="red")
plot(roc2, add=TRUE, col="blue")

# How to do area under the curve just in case
auc1<-performance(pred.1,measure="auc")
auc2<-performance(pred.2,measure="auc")


