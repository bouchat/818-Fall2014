#Welcome to R! I'm going to demonstrate today using the R console.
#You can also use RStudio.
#Lesson 1: Comment on your code using the '#' symbol.

#####################################
# STEP 1: SETTING UP YOUR SCRIPT FILE
#####################################

#Begin the script file with a label for yourself and others:

#File Name: PS 818 Section 1
#Data: [Insert dataset name here]
#Author: Sarah Bouchat
#Date: 5 Sept 2014

#Next, set your working directory. This way you can save throughout without naming new file paths.
#A working directory lets R know where to look for files and data you reference, and where to deposit output you save
setwd("/Users/sbouchat/Dropbox/WISC/Classes/2014-Fall/TAing/818Section")
#If you are using Winstat or the SSCC computers, use your U drive this way
setwd("U:/Section")
#In order to do this, you must have a folder named "Section" (or whatever you'd like to call it) in your U drive already. To get to the U drive, click on the Windows button on the menu bar, and then your name. It should be in the list of drives on the left

#####################################
# STEP 2: LOADING PACKAGES
#####################################

#After you set the working directory and before you load your data, load the packages you need

#Load packages by calling them from the library
library(foreign)
library (MASS)
library(xtable)

#If you've never used a package before, you may have to install it first:
install.packages("reshape")
library(reshape)

#R often automatically loads dependent packages for you, but if not it will let you know

#Don't know what package you need? The internet is your friend. Try StackExchange or CRAN

#####################################
# STEP 3: DATA
#####################################

#There are many ways to enter data into R

#First, you can enter it manually
#Use '<-' to assign data to a variable/object

#Scalar examples
scalar<-2
X<-3

#Vector or matrix examples
set<-c(1,2,3,4,5,6)
matrix.1<-matrix(set,nrow=3, ncol=2, byrow=F)
matrix.2<-matrix(1,2,3)
matrix.3<-diag(3)

#Need help? You can always ask R
help(matrix)
?matrix
??matrix
#These help pages will often at least tell you the arguments that a function takes. If you need more help...to the google!

#Note that the 'data' can be numeric or string
streetnames<-c("Langdon", "Park", "University", "Observatory")

#Check that R remembers what we've entered and see what each object looks like
scalar
X
matrix.1
matrix.2
matrix.3

#Be careful! R is case sensitive
counting.is.fun<-c(1,2,3)
counting.is.fun
Counting.is.fun

#You can always remove variables with 'rm()'
rm(counting.is.fun)
counting.is.fun

#Check what variables remain in the workspace with 'ls()' (that is list objects)
ls()

#Burn it down: remove everything from the workspace! 
rm(list=ls())

#You can save your work history this way
savehistory(file="filename.Rhistory")

#You can also save objects that you've created (for example, images) directly
pdf(file="myprettypicture.pdf")
math<-function(x){x+1}
values<-c(0:10)
math(values)
plot(curve(math, xlim=c(0,12), ylab="Function Values"))
dev.off()

#The other main way to enter data is to pull in a dataset from outside R
#These datasets can be .csv, .dta, etc. formats
#Note that I'm pulling in data here that you don't have access to. Try this at home with your own data file

#To use these different data file formats, you need to have the 'foreign' package installed and to tell R what kind of file it's reading
apsr<-read.dta("apsr.dta")

#After you load it, get to know your data
summary(apsr)
names(apsr)
head(apsr)

#Need to change something but want to do it with a tiny gui?
fix(apsr)

#You need to either constantly reference the dataset or attach it to reference variables
mean(apsr$local_total)
attach(apsr)
mean(local_total)

#####################################
# STEP 4: BASICS OF COMPUTATION IN R 
# (REVIEW ON YOUR OWN)
#####################################

#You can use R as a fancy calculator if you want
1+1
9^2
sqrt(2)

#You can also store calculations for later use:
Addition<-2+2
Multiplication<-3*5000

#We'll also be using many of R's probability functions in this class
#Don't worry about these right now, just keep them in mind for later

#Use R to find the cdf
pnorm(2,mean=0, sd=1)
pnorm(3,4,5)
x<-pnorm(3,4,5)

#Look up quantiles
qnorm(.5, 4,5)
qnorm(x,4,5)
help(qnorm)

#Find the density
#How likely it is that a fair coin turns up heads exactly 4 times in 10 trials?
dbinom(4,size=10,prob=0.5)

#####################################
# STEP 5: MLE in R 
#####################################

#### FUNCTIONS ####

#Defining your own functions

myfavefunction<-function(x){
	x^2
	}
myfavefunction(2)
myfavefunction(10)


secondfavefunction<-function(y){
	log(y)
	}
	
secondfavefunction(5)
log(5)

#You can also use functions for sequences of values
myseq<-c(1:10)
myfavefunction(myseq)
myfavefunction(c(1:10))
myfavefunction(seq(1,2,3))

#Let's talk about logits
logistic<-function(x){
	1/(1+exp(-x))
	}
logistic(-1:1)

#You can also use functions that reference more than one variable
multivar<-function(n,m){
	2*(n-m)
	}

multivar(10,3)
multivar(3,10)


#### OPTIMIZATION ####

#Try this out:
#Find the minimum of f: 2*x^2+10 in the interval [0,1]
f<-function(x){2*x^2 +10}
min<-optimize(f, interval=c(0,1))

#Now find the maximum
max<-optimize(f, interval=c(0,1), maximum=TRUE)

min
max

#Plotting your optimization exercises
curve(f, xlim = c(0, 1), ylab = expression(f(x)),
main = expression(f(x) == 2*x^2 +10))
abline(v=min$min, col="blue")
abline(v=max$max, col="green")


#### OLS REGRESSION ####
#Synethic data example
y<-rnorm(1000,mean=5, sd=.5)
rnorm<-rnorm(1000, mean=10, sd=1)
x<-rep(0,1000)
for(i in 1:1000){
	x[i]<-mean(sample(rnorm, 10, replace=TRUE, prob=NULL))
}
m<-rep(0,1000)
for(i in 1:1000){
	m[i]<-median(sample(rnorm, 10, replace=TRUE, prob=NULL))
}

simpleOLS<-lm(y~x+m)
summary(simpleOLS)

#Version with actual data
sillyOLS<-lm(ln_gdpcap~distance_hnhcmc + agr_out + secondary)
summary(sillyOLS)

#####################################
# DESIDERATA: OTHER USEFUL FUNCTIONS
#####################################

#Find out the class of a vector or object
x<-matrix(1,2,3)
class(x)

#Make a dataframe!
stuff<-c("a", "b", "c")
things<-c(1,2,3)
stuffandthings<-data.frame(stuff, things)

#Bind columns using 'cbind'
a<-c(1,2,3)
b<-c(4,5,6)
g<-cbind(a,b)

#All of the apply functions (apply, sapply, lapply, tapply, etc.)
#Find out more: ??apply
#Use tapply to generate summary statistics
#Example: 
tapply(age, local_total, mean)
#Gives me the mean age of each representative by the number of times they mentioned a "local" issue
	
#If you want to see these just enumerated, use 'table'
table(age, local_total)


#***Many thanks to Emily Sellars for previous years' section notes!***
