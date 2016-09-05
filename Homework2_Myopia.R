#install packages needed and input them to the lib for R to reference
#NOTE: aplore3 is a package that has all datasets used from the Hosmer book
install.packages("aplore3")
library(aplore3)

#bring variables in to environment
head(myopia, n=10)
spheq <- myopia$spheq
myopic <- myopia$myopic
gender <- myopia$gender
sporthr <- myopia$sporthr
readhr <- myopia$readhr
comphr <- myopia$comphr
studyhr <- myopia$studyhr
tvhr <- myopia$tvhr

#logistic regression model
myop.gm <- glm(formula = myopic ~ spheq + gender + sporthr + readhr + comphr + studyhr + tvhr, family = binomial(link=logit))
summary(myop.gm)

#denoting a column of betas and std.errors
beta <- coef(summary(myop.gm))[,1]
sd <- coef(summary(myop.gm))[,2]

#manually performing a confidence interval
UpperConf = beta + 1.96*sd
LowerConf = beta - 1.96*sd
Conf <- c(LowerConf,beta,UpperConf)
Conf <- matrix(Conf,nrow=3)
Conf

#testing different confidence interval methods
#likelihood approach
confint(myop.gm)

#walds approach
confint.default(myop.gm)

#set up the range of x in our function models
x <- seq(from = -0.7, to = 4.4, by = 0.05)

#gender is defaulted to male, female has beta 0.56
fem <- function(x){
  fem <- 0.68157 - 4.13929*x + 0.56506 
  return(fem)}

#to get male we take off the female to just the male effect
mal <- function(x){
  mal <- 0.68157 - 4.13929*x
  return(mal)}

#our model is currently linear since we performed the logit
#so we do an inverse logit to get our true values
Invlogit<-function(g)
{
  Invlogit<-exp(g)/(1+exp(g))
  return(Invlogit)
}

#creating the plot using the invlogit function created
plot(x,Invlogit(fem(x)),pch="F",cex=.5,ylim=c(0,1),ylab="Probability of Myopia",xlab="Spherical Equivalent Refraction",col=2)
points(x,Invlogit(mal(x)),pch="M",cex=.5)
title(main=paste("Probability of Myopia by Gender", "\n-Female (red)  -Male (black)"))
#the mean line
lines(c(-0.7,4.4),c(.5,.5),col=4)
#saving the plot as a .png
dev.copy(png,"myfile.png",width=8,height=6,units="in",res=100)
dev.off()

#time stamp
Sys.time()

