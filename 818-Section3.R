#PS818 Section 3
#19 September 2014
#For Loops & Functions


#######################################
# 1: FOR LOOPS
#######################################

for(i in 1:20){
	print(i)
}

vector<-c(2,5,98,734)
for(i in vector){
	print(i)
}

for(i in c(398,7894)){
	print(i)
}

#General steps for writing a simple for loop
# (1) Create an empty vector/matrix of the right size
# (2) Specify the range over which your mathematical process will take place
# (3) Write the operation that you want the for loop to perform iteratively
# (4) Include the objection into which you're saving the output of this operation (hint: the empty vector/matrix you initially created)


#First we've created a new vector, and we've told R to fill it with NA's in 11 slots
new.vector<-rep(NA,11)
for(i in 1:11){ #Here we've started the for loop, and we're applying it to each of these 11 slots
	new.vector[i]<-i*5 #Now we're going to write over the original object and fill each slot with a multiple of 5
}
new.vector

#This vector just has a single slot that we'll write over with a "sum" process
sum<-0 
for(i in 1:60){
	sum<-sum+i
}

#######################################
# 2: FUNCTIONS (REVIEW)
#######################################

logistic.function<-function(x){
	1/(1+exp(-x))
}

#Remember that you can write and solve functions over sequences
logistic.function(-1:1)

#Remember that we can also do functions of multiple variables
multi.function<-function(x,y){
	2*x+y
}

multi.function(4,6)
multi.function(6,4)

rm(list=ls())

#######################################
# 2: LIKELIHOODS & OPTIMIZATION
#######################################

# MLE starts with the data and makes an assumption about the distribution. We use that to write down a joint probability that we turn into a likelihood 

# Likelihood Function: joint pdf or pmf of having seen the data we observed

# Example: 
# Coin: {H,H,T,H,T,H,H}. What if we want to show that this coin is fair?
# p(H): p*p*(1-p)*p*(1-p)*p*p = p^5(1-p)^2
#W hat is the value of p that maximizes this expression?
# p^y(1-p)^(n-y) [n=trials, y=successes]
# Max with respect to p to get the likelihood function--> log to make computation easier
# ylog(p)+(n-y)log(1-p)
# dl/dp = y/p - (n-y)/(1-p) = 0
# log is concave
# y/p = (n-y)/(1-p) --> y=np, y/n=phat
# Joint probability of having seen your data if your data is iid is just the joint product (because, statistical independence)

# Using functions and optimize together to maximize
x.squared<-function(x){x^2+4}
# min at 4, argmin = 0
minimum<-optimize(x.squared,interval=c(-1,1))
maximum<-optimize(x.squared, interval=c(-1,1), maximum=TRUE)
#By default optimize finds a minimum; only works in univariate case

minimum
# min = argmin
# objective = minimum
maximum
# As soon as optimize finds a single maximum, it stops, so don't do this to find multiple critical points. This function, for example, has 2.

curve(x.squared,xlim=c(-1,1), ylab=expression(f(x)),main=expression(f(x)==x^2))
abline(v=minimum$min, col="blue")
abline(v=maximum$max,col="green")

# If the derivative is undefined, it might still work as a maximum because it might be a corner solution

######################
# Multivariate optima
######################

# Critical points for a multivariate function are where the derivative with respect to each variable is 0 (or undefined)

# x^2 + y^2 and -x^2-y^2 and x^2 -y^2 all have a critical point at (0,0)

# Hessian is the second derivative of a multivariable function stored in a matrix

# Take the first derivative of each of these (the first of these) you get 2x and 2y. The second derivatives have to be done wrt x and y, so there are actually 4: 2,0,2,0

#HESSIAN: 
#   x                y
#x |d^2f/dx^2=2   d^2f/dy^2=0|
#y |0                2|

# For this to be positive definite you need to make sure the squares are positive (so upper left corner, then the whole thing)

############
# Bracket this, just fyi:
#Principal minors are the eigenvalues (multiplier) of the matrix (minor is the determinant of a smaller square submatrix)
#2
#4-0 = 4
# Therefore, we are at a minimum b/c 2nd derivative is positive
############

# What you need to know for doing this by hand:
# If the upper left corner is pos, max; neg, min
# If the whole thing is opposite signed from the upper left it's a saddle point
# For 2x2 case, determinant neg=saddle point is sufficient

Adding.sqr<-function(x){
	x1<-x[1]
	x2<-x[2]
	3*x1^2+x2^2+1
}

# what does this look like?
fun.for.plotting<-function(x1,x2){
	3*x1^2+x2^2+1
}

x1.holder<- -5:1/1
x2.holder<- -5:5/1
# making lattices ourselves
# /1 is in increments of 1

y<-outer(x1.holder,x2.holder,fun.for.plotting)
persp(x1.holder,x2.holder,y,main=expression(f(x[1],x[2])==3*x[1]^2 -x[2]^2+1), theta=0, xlab=expression(x[1]),ylab=expression(x[2]),zlab=expression(f(x)))

#new, generic optimize command
min.x1.x2<-optim(c(1,1),fn=Adding.sqr)
min.x1.x2
help(optim)
#convergence 0 means it did converge, convergence anything other than 0 means it did not converge therefore you have not found a maximum

neg.Add.sqr<-function(x){
	x1<-x[1]
	x2<-x[2]
	-1*(3*x1+x2^2+1)
}

max.x1.x2<-optim(c(1,1),fn=neg.Add.sqr)
max.x1.x2
help(optim)

# Real life
library(foreign)
cox.data<-read.dta("/Users/sbouchat/Downloads/coxappend.dta")
attach(cox.data)
ols<-lm(ml~enpv,data=cox.data)
detach(cox.data)
summary(ols)

new.data<-data.frame(y=cox.data$ml,x=cox.data$enpv)
sum.of.square.errors<-function(data,parameters){
	with(data,sum((parameters[1]+parameters[2]*x-y)^2))
}

find.min.RSS<-optim(par=c(0,1),sum.of.square.errors,data=new.data)
find.min.RSS
# Remember that small sum of squared residuals means we've fit the model to the data well

# What does that look like
plot(y~x,data=new.data)
abline(a=find.min.RSS$par[1],b=find.min.RSS$par[2],col="green")

# * Many thanks to Emily Sellars for past years' section materials! *
