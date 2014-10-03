#PS818 Section 5
#3 October 2014
#ROC plots and Separation plots


library(foreign) 
library(xtable)  
library(MASS) 
library(boot)
library(graphics)
library(arm)
library(ROCR)
library(separationplot)
library(RColorBrewer)
# install.packages("coefplot2",
 # repos="http://www.math.mcmaster.ca/bolker/R",
# type="source")
library(coefplot2)
library(ggplot2)


#######################################
# 0: OTHER WAYS TO DROP OR RECODE DATA
#######################################

#Let's use our good friends the snails as an example
data(snails)

#Let's say that the 96th row contained one NA--that lab tech fell asleep and knocked over the snails and they escaped, perhaps
snails.nomiss<-snails[-96,]
#This just drops the single "missing" observation in row 96
attach(snails.nomiss)

#Note that the documentation for this data thinks this experiment is unusual in its relatively low fatality rate. That might suggest that we want to bin some of the low mortality observations. Let's pretend we don't think there's that much difference between 7 and 8 deaths or 14 and 16 deaths, etc., and just recode into "high death" and "low death" cases

#Here's just one way to do that

snails.nomiss$deathHL<-NA
#We've created a new empty variable rather than rewriting the old Deaths variable

snails.nomiss$deathHL[snails.nomiss$Deaths>=9]<-1
snails.nomiss$deathHL[snails.nomiss$Deaths<9]<-0

#You could also specify other ranges if you were doing a multinomial setup in this way:

snails.nomiss$deathrange<-NA

snails.nomiss$deathrange[snails.nomiss$Deaths==0] <-0
snails.nomiss$deathrange[snails.nomiss$Deaths==1 | snails.nomiss$Deaths==2 | snails.nomiss$Deaths==3 | snails.nomiss$Deaths==4 | snails.nomiss$Deaths==5 | snails.nomiss$Deaths==6 | snails.nomiss$Deaths==7] <-1
snails.nomiss$deathrange[snails.nomiss$Deaths==9 | snails.nomiss$Deaths==10 | snails.nomiss$Deaths==11 | snails.nomiss$Deaths==12 | snails.nomiss$Deaths==14 | snails.nomiss$Deaths==16] <-2


#######################################
# 1: ROC PLOTS
#######################################
fit.glm1<-glm(deathHL ~ Temp,
           family = binomial(link = "logit"), data = snails.nomiss)
summary(fit.glm1)

fit.glm2<-glm(deathHL ~ Temp+Rel.Hum+Exposure,
           family = binomial(link = "logit"), data = snails.nomiss)
summary(fit.glm2)

pred.1<-prediction(fit.glm1$fitted.values, snails.nomiss$deathHL)
pred.2<-prediction(fit.glm2$fitted.values, snails.nomiss$deathHL)
roc1<-performance(pred.1,"tpr","fpr")
roc2<-performance(pred.2,"tpr","fpr")
plot(roc1,col="red")
plot(roc2,add=T,col="blue")


#######################################
# 2: SEPARATION PLOTS
#######################################

separationplot(pred=fit.glm1$fitted.values, actual=snails.nomiss$deathHL,type="rect", line=TRUE, show.expected=TRUE, width=3,heading="Restricted Model")

separationplot()

separationplot(pred=fit.glm2$fitted.values, actual=snails.nomiss$deathHL,type="rect", line=TRUE, show.expected=TRUE, width=3,heading="Full Model")

separationplot()


#######################################
# 3: GGPLOT2, MULTIPLOT, AND COLORS
#######################################

#Let's start with some basic examples in ggplot2

# Let's make a barchart based on the built-in mtcars dataset
ggplot(mtcars, aes(factor(cyl)))+geom_bar()
	# Note that by default, this uses stat="bin", which gives the count in each category
	#Also note that if you want, you can save ggplots to objects

#geom_bar() can also take aesthetic arguments like this
ggplot(mtcars, aes(factor(cyl)))+geom_bar(fill="white", colour="blue")

#ggplot2 comes with an attractive gray background. But you can also change that theme if you want using + theme_bw()
ggplot(mtcars, aes(factor(cyl)))+geom_bar(fill="pink", colour="purple")+theme_bw()

#What about line graphs? Here's an example using the diamonds dataset in ggplot2
ggplot(diamonds, aes(clarity))+geom_freqpoly(aes(group = cut, colour = cut))+theme_bw()
#These are the default multiple color settings in ggplot2, btw



#You can specify your own color palette with a series of #s for specific color codes

redblue.palette<-c("#FF0A28","#5BC6E8","#464747")

#Go to this website to translate the color codes for what you want: http://www.rapidtables.com/web/color/RGB_Color.htm 

#Note per the website that some color numbers appear differently in different software


#There's a lot to be done here with color theory if you're so inclined, but one thing to keep in mind always is your audience. Here are two prefab colorblind friendly color palettes to try

cbPalettegray <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cbbPaletteblack <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Note that your color palette has to be the same length as the variables you're matching it to
cbbPaletteblack3 <- c("#0072B2", "#D55E00", "#CC79A7")

#Here's just one example of how to fill in your own colors with ggplot2. You can also use premade colors with scale_colour_brewer() and scale_fill_brewer() once you've loaded the RColorBrewer package
qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(cyl))+theme_bw()+scale_fill_manual(values=cbbPaletteblack3)


########################
# Multiplot
########################

#Multiplot is a function that lives inside ggplot2, but works a little bit like par or add() in regular plot. 

#On some occassions and for some kinds of models, multiplot won't load, but there's good news! We have the documentation because, open source. So you can specify and study the function yourself.

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
 if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Here's an example to walk through courtesy of the internets, using yet another adorable dataset included in an R package (ChickWeight)

#Plot 1
p1 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
    geom_line() +
    ggtitle("Growth curve for individual chicks")

#Plot 2
p2 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) +
    geom_point(alpha=.3) +
    geom_smooth(alpha=.2, size=1) +
    ggtitle("Fitted growth curve per diet")

#Plot 3
p3 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) +
    geom_density() +
    ggtitle("Final weight, by diet")

#Plot 4
p4 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) +
    geom_histogram(colour="black", binwidth=50) +
    facet_grid(Diet ~ .) +
    ggtitle("Final weight, by diet") +
    theme(legend.position="none")        
	# We took the legend out here because it would be redundant

# Now use multiplot to put these all together in 2 columns
multiplot(p1, p2, p3, p4, cols=2)

#Applications for this include, if you want to produce a coefplot() with more customization than the standard coefplot() package. Using multiplot, you can stack point estimates and standard errors from different models into a single plot to show how they change depending on the specification













