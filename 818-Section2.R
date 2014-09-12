#PS818 Section 2
#12 September 2014
#Matrix Operations and Graphics


#######################################
# 1: REMINDER: LOADING DATA & PACKAGES
#######################################

library(foreign)
wdi<-read.csv("/Users/sbouchat/Downloads/wdi2000.csv")
attributes(wdi)
summary(wdi)

#Practice creating a "new" variable
wdi$log.gdp<-log(wdi$gdp)

#Dealing with missing data
wdi.nona<-na.omit(wdi)

#######################################
# 2: MATRIX OPERATIONS
#######################################

#Creating a matrix from scratch
#Examples:
#(x'x)^-1x'y
#e'e = (y-XB)'(y-XB)

#cbind lets us bind columns of variables
X<-as.matrix(cbind(wdi.nona$dptshots,wdi.nona$gdpcap))

#Or we can create a matrix from a single variable
Y<-as.matrix(wdi.nona$NO2)

#What are the dimensions of x and y? We need to know the dimensions if we want to do operations with matrices
dim(X)
dim(Y)

#If we're going to construct a model, though, we need a constant. If you don't include one, you impose 0 as an intercept. 
#We'll just fill in a "1" in the intercept column
cbind(1,wdi.nona$dptshots,wdi.nona$gdpcap)

#Take the transpose of a matrix using t()
Xprime<-t(X)

#What if we want to multiply matrices? Remember: we need to know their dimensions, and make sure they're correct

#Multiply matrices in R using "%*%"
XprimeX<-Xprime%*%X
#How many dimensions will XprimeX have?

#Note: If you just use "*" to multiply with matrices, you should not receive an error, but R will multiply element-by-element

#What is the inverse of a matrix and why is it important? 
#Inverse: Multiply by a matrix --> identity matrix

Identity<-diag(2)

#Verify how the identity matrix works
identical(XprimeX,XprimeX%*%Identity)

#What needs to hold in order to take the inverse a matrix?
#Inverse requires that the matrix be square, have full rank

XprimeX.inverse<-solve(XprimeX) 
#This is the inversion process: "solve" the matrix

#Reminder: you cannot take the inverse a non-square matrix
X.inverse<-solve(X)

XprimeX%*%XprimeX.inverse
round(XprimeX%*%XprimeX.inverse,digit=10)

#Use the Matrix library to compute rank of your matrix
library(Matrix)
rankMatrix(X)

#Example
x1<-c(1,2)
x2<-c(2,4)
perfectly.colinear<-cbind(x1,x2)
rankMatrix(perfectly.colinear)
solve(perfectly.colinear)
y1<-c(1,2)
y2<-c(2,4.00000000000001)
imperfectly.colinear<-cbind(y1,y2)
solve(notquite.colinear)
rankMatrix(notquite.colinear)

#Also remember that your variables need to vary
Z<-as.matrix(cbind(wdi.nona$dptshots,wdi.nona$netfdi))
Zprime<-t(Z)
ZprimeZ<-Zprime%*%Z
ZprimeZ.inverse<-solve(ZprimeZ)
rankMatrix(ZprimeZ)

#This is a good example of why you should look at your data
plot(wdi.nona$netfdi)

#Speaking of which...

#######################################
# 2: GRAPHICS
#######################################

library(foreign) 
library(stats)
library(xtable)  
library(MASS)
#need this to load truehist()
library(arm)
#need this one for coefplot()
library(lattice)
library(scatterplot3d)
library(ggplot2)

#### BASICS ####

#Looking at your data: plot the kernel density of variables to see their distribution
attach(trees)
plot(density(trees$Height), main="Trees by Height")

#We can also do simple histograms
mtcars
hist(mtcars$mpg)

#Manually adjust the characteristics of your graph
hist(mtcars$mpg, breaks=c(10,20,30,40), main="Name of my graph", col="blue", border="red", ylab="Frequency", xlab="Miles per Gallon")

#Note that if you set a vector of "breaks" manually it must span your data (you can also just tell R how many breaks you want it to have and let R figure it out)
hist(mtcars$mpg, breaks=c(10,12,14,16,18,20,30,40), main="Name of my graph", col="blue", border="red", ylab="Frequency", xlab="Miles per Gallon")

#Values for your data and values for your axes should be proportional
hist(mtcars$mpg, breaks=c(10,20,30,40), xlim=c(0,80), ylim=c(0,20))

#You can also change other characteristics of graphs that your help function might not mention--be creative, but get your point across
hist(mtcars$mpg, breaks=c(10,20,30,40), main="Name of my graph", col="blue", border="red", ylab="Frequency", xlab="Miles per Gallon", col.lab="blue")

#We can also do basic scatterplots to look at our data, and fit a regression line
attach(mtcars)
plot(hp, mpg) 
abline(lm(mpg~hp))
title("Regression of Miles per Gallon on Horsepower")

#Change what kinds of points you use
plot(wt, mpg)
plot(wt, mpg, pch=16)
#There are many options for this argument, but a few useful ones are: 1: default open circle, 0: open square, 2: open triangle, 16: filled circle, 15: filled square, 17: filled triangle, *: *

#You can also manually set the colors, and in some cases fill and border colors. pch=c(21:25) have border and fill color options. With these numbers, use additional arguments col= for the border and bg= for the fill colors.


#What if we want the size of those points to have meaning?
plot(hp, mpg) 
plot(hp,mpg, cex=wt)
abline(lm(mpg~hp), col="red")



#There are other ways of distinguishing data. Let's try a line graph

#We'll graph number of hours slept over a single week. Grad school is rough.
me<-c(6,3,8,3,1.5,11,9)
roommate<-c(8,2.5,9,4,7,4.5,3)

#Create a plot with the appropriate y axis (when do you get to sleep more than 12 hours?). We'll leave off axis and axis labels so we can do them ourselves
plot(me, type="o", col="blue", ylim=c(0,12), axes=FALSE, ann=FALSE)
#Remember to tell R what kind of punctuation we want for each day. "o" is "overplotted" because we are doing both points and lines, whereas "p" is for points and "l" is for lines. 

#Use "lines" to add a line to a preexisting plot. Make sure we can distinguish my sleep from my roommate's using line type and point type
lines(roommate, type="o", pch=22, lty=2, col="red")

#Just like points, there are many "line type" (lty) options: 0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash. You can also call them using character strings: "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash", where "blank" uses ‘invisible lines’ (i.e., does not draw them)

#Create a title outside the main plot command, and change the font for fun
title(main="Sleep: A Study", font.main=4)

#Let's label our axes too, shall we?
axis(1, at=1:7, lab=c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"))
axis(2, at=0:12)

#Alternatively, we could have done axis labels/super labels with:
title(xlab="Days")
title(ylab="Hours Slept")

#Hm, how should we let the reader know whose line is whose? We'll come back to that



#Perhaps I want to arrange graphs side-by-side
par(mfrow=c(1,2))
plot(gear, mpg)
plot(carb, mpg, pch=16)
#Note that the arguments for mfrow matter. The first is the number of rows, the second is the number of columns

#One of R's strengths relative to other software is graphics. Walk through these examples on your own to see the types of arguments they take and the types of graphics you can produce
example(plot)                       
example(barplot)                     
example(boxplot)
example(dotchart)
example(coplot)
example(hist)
example(fourfoldplot)
example(stars)
example(image)
example(contour)
example(filled.contour)
example(persp) 

#One type that you may use often in this course is the boxplot. Let's walk through a built-in R example
par(mfrow=c(1,1))
boxplot(len ~ dose, data = ToothGrowth,boxwex = 0.25, at = 1:3-0.2, subset = supp == "VC", col = "yellow", main = "Guinea Pigs' Tooth Growth", xlab = "Vitamin C dose mg", ylab = "tooth length", xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")
boxplot(len~dose, data=ToothGrowth, add=TRUE, boxwex=0.25, at=1:3+0.2, subset=supp=="OJ", col="orange")
legend(2, 9, c("Ascorbic acid", "Orange juice"),
fill = c("yellow", "orange"))
#boxwex is adjusting the size of the boxes, while "at" is telling R where to place them, just like we did above when we specified where the x and y axes should be

#Ah, a legend! The third set of arguments creates a legend from scratch. The first two arguments of legend() tell R where to place it in x,y dimensions. From there, you can label the meaning of colors, and specify which colors appear in the legend
boxplot(len ~ dose, data = ToothGrowth,boxwex = 0.25, at = 1:3 - 0.2, subset = supp == "VC", col = "yellow", main = "Guinea Pigs' Tooth Growth", xlab = "Vitamin C dose mg", ylab = "tooth length", xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")
boxplot(len~dose, data=ToothGrowth, add=TRUE, boxwex=0.25, at=1:3+0.2, subset=supp=="OJ", col="orange")
legend(3, 10, c("Ascorbic acid", "Orange juice"),
fill = c("yellow", "orange"))


#Boxplots are just one way of representing data with confidence bounds, or representing outliers. Another graphical way to avoid cumbersome tables are coefficient plots, which we'll discuss a lot in this class. You can use these for summary statistics or for regression results. You need to load the "arm" package to use coefplot()
treereg<-lm(Volume~Girth+Height)
coefplot(treereg)


#Want to do more advanced graphics? We'll come to these in a later section, but you can play around with the lattice and ggplot2 packages


#***Many thanks to Emily Sellars for previous years' section notes!***
