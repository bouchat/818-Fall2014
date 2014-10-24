#PS818 Section 8
#24 October 2014
#Ordered and Multinomial Responses, Parallel Regressions II

setwd("/Users/sbouchat/Dropbox/WISC/Classes/2014-Fall/TAing/818/818Section")

library(foreign) 
library(xtable)  
library(MASS) 
library(boot)
library(graphics)
library(arm)
library(stargazer)
library(apsrtable)
library(ggplot2)
library(nnet)
library(VGAM)
library(rms)

############################################
# Basics of Multiplicative Interaction Terms
############################################

data(snails)

# Let's practice here with a logit
snails$deathHL<-NA
snails$deathHL[snails$Deaths>=9]<-1
snails$deathHL[snails$Deaths<9]<-0
attach(snails)

# Remember that using the * notation means that base terms are included, while using : means they are not
fit1<-glm(deathHL~Rel.Hum*Exposure, family = binomial(link = "logit"))
fit2<-glm(deathHL~Rel.Hum + Exposure, family = binomial(link = "logit"))

# How do we know which model is preferred? Check out the AIC and BIC
apsrtable(fit1, fit2)

# What about marginal effects?
# Let's just do this for the 1st quantile and the median of relative humidity and go back to the "linear" model

fit.snails<-lm(Deaths~(Rel.Hum*Exposure), data=snails)
summary(fit.snails)

# Specify our quartile values of interest
firstq<-quantile(Rel.Hum)[2]
median<-median(Rel.Hum)

# Extract the coefficients and variance covariance matrix
Betas<-fit.snails$coefficients
VarCovar<-vcov(fit.snails)

# Marginal effects time! First take a look at what "Betas" gives you
Betas
# Notice that the 2nd column is Relative Humidity and the 4th is the interaction

# For the marginal effects then, we want the coefficient on the base term just for Relative Humidity (that 2nd column) PLUS the coefficient in the 4th column, which in this case we're going to multiply by the quartile value we picked above
ME.firstq<-Betas[2]+Betas[4]*firstq
ME.median<-Betas[2]+Betas[4]*median

# Standard errors time now. This is a sort of long way. Again, there are other ways of doing this--perhaps you'll find something more elegant that you like better

# Take a look at VarCovar to figure out which rows and columns you want to pull
VarCovar
SE.firstq<-sqrt(VarCovar[2,2] + VarCovar[4,4]*firstq^2 + 2*firstq*VarCovar[2,4])
SE.median<-sqrt(VarCovar[2,2] + VarCovar[4,4]*median^2 + 2*median*VarCovar[2,4])

# We can summarize this in a table, or we could do it in a coefplot like so. Notice that this is not super informative where snails are concerned, because as we already saw, the interaction term model was not preferred, in addition to which this is a small dataset without much variation. But ther operations are the same
Coefs<-c(ME.firstq,ME.median)
SEs<-c(SE.firstq, SE.median)
coefplot(Coefs, SEs, CI=2, ylab="Deaths",varnames=c("1st Q","Median"), main="Marginal Effects", mar=c(1,4,5.25,2))



####################################
# Multinomials
####################################

# Here we'll do the analog of what you have to do on your problem sets, again with our friends the dead snails

# Creating categories for our DV again

snails$deathrange<-NA

snails$deathrange[snails$Deaths==0] <-0
snails$deathrange[snails$Deaths==1 | snails$Deaths==2 | snails$Deaths==3 | snails$Deaths==4 | snails$Deaths==5 | snails$Deaths==6 | snails$Deaths==7] <-1
snails$deathrange[snails$Deaths==9 | snails$Deaths==10 | snails$Deaths==11 | snails$Deaths==12 | snails$Deaths==14 | snails$Deaths==16] <-2

snails$deathfactor<-as.factor(snails$deathrange)
attach(snails)

# Recall how we estimated the ordered version last time
ologit<-polr(deathfactor~ Temp+Rel.Hum+Exposure, data=snails, Hess=TRUE, method="logistic")
summary(ologit)

# Now let's do this as a multinomial. I'm using the nnet package for this. There are other ways to do it also
# Note that my DV is still as a factor, though

mnlogit<-multinom(deathfactor~ Temp+Rel.Hum+Exposure, data=snails, Hess=TRUE)

# Again we can use stargazer (if we want) to make a pretty table of results
stargazer(mnlogit, title="Dead Snails", align=TRUE, dep.var.labels=c("Amount of Death"), covariate.labels=c("Temp", "Rel Humidity", "Exposure"))

# How are these results different?


##########################################
# Parallel Regressions (Proportional Odds)
##########################################
# Now that we've estimated a multinomial version, let's check the parallel regressions assumption 
# For these we will be using the VGAM package. You may have to install it first. Also make sure you have rms loaded
# This is another situation where our "test" is usually visual

# Round up the usual suspects
Coef.mn<-coef(mnlogit)
varcovar.mn<-vcov(mnlogit)
SEmn<-matrix(sqrt(diag(varcovar.mn)), nrow=2, byrow=TRUE)

# Let's preserve variable names, shall we?
rownames(SEmn)<-rownames(Coef.mn)
colnames(SEmn)<-colnames(Coef.mn)

# Specify the ncut (and k if you use it; I didn't here and you'll see why) that you'll need for the for loop. ncut is just the number of categories for your DV minus the reference category. k will be the number of coefficients in your model less the intercept. For example:
ncut<-2
k<-3

# Ok. Plotting time. First let's open a par environment. Specify this, as usual, according to your number of covariates. Here I have 3 that I want to arrange each as their own row
par(mfrow=c(3,1))
# The next step is what appears to be a pretty epic for loop. Do not despair! Just appropriate John's code
for(i in 1:k){
plot(1:ncut, Coef.mn[,i], type="b", lty=2, xlab="Death", ylab=colnames(Coef.mn)[i],ylim=c((min(Coef.mn[,i])-2*mean(SEmn[,i])),(max(Coef.mn[,i])+2*mean(SEmn[,i]))))
abline(h=0, lty=1, lwd=.75, col=grey(.5))
}


# You can also test this using vglm and a chi squared test between the ordered and multinomial versions of your model

model.ord<-vglm(deathfactor~Temp+Rel.Hum+Exposure, data=snails, family=propodds)

model.multinom<-vglm(deathfactor~Temp+Rel.Hum+Exposure, data=snails, family=multinomial)

test<-pchisq(deviance(model.ord)-deviance(model.multinom), df=df.residual(model.ord)-df.residual(model.multinom), lower.tail=FALSE)


