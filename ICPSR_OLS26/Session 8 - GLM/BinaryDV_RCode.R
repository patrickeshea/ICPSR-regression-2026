## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

rm(list=ls())

# 1. Estimating a Binary Response Model
# 1.1. Using glm(), Data Example , Court.dta
#install.packages("faraway")
library(faraway)
require(foreign)
mydata<-read.dta("data/Court.dta")

#The variable of interest is Supreme Court's decision making in cases 
#involving habeas corpus. propetit is
#the dependent variable, coded as 1 if the decision is pro-petitioner, 
#and 0 otherwise. If we theorize that the Court's decision is 
#influenced by its political preferences (measured as liberalism), we can
#empirically explore this theoretical argument by including liberal 
#as the key explanatory variable.
#This variable is scaled between 0 and 1. We rescale it 
#into a 0-to-100 scale in the subsequent analysis.


# rescale the "liberal" variable
mydata$liberal2<-round(mydata$liberal*100,2)
attach(mydata)
head(mydata)
summary(mydata)


# Describe data
hist(mydata$liberal2)

library(lattice)
densityplot(mydata$liberal2)

barplot(sort(table(mydata$propetit),decreasing=TRUE),las=2)


plot(propetit ~ liberal2, mydata, xlim=c(0,100), 
     ylim = c(0,1), xlab="Liberalism", 
     ylab="Observed Petition Outcome")

##Fit LM
lmod <- lm(propetit ~ liberal2, mydata)

##Fit LM line
abline(lmod)

sumary(lmod)


# Logit Model
logitmod <- glm(propetit ~ liberal2, family=binomial(link=logit), mydata)

# Probit Model
probitmod<-glm(propetit ~ liberal2, family=binomial(link=probit), mydata)

summary(logitmod)
summary(probitmod)



library(stargazer)
stargazer(lmod,logitmod,probitmod, type = "text")







#Predicted Probility for median liberalism

summary(liberal2)

#From logit
exp(-3.2828+0.06929*41)/(1+exp(-3.2828+0.06929*41))
#From probit
pnorm(-1.9731+0.04176*41)


summary(mydata$liberal2)
# Prediction when liberal is at its min, median, and max
ilogit(-3.2828+0.06929*21)
pnorm (-1.9731+0.04176*21)
ilogit(-3.2828+0.06929*41)
pnorm (-1.9731+0.04176*41)
ilogit(-3.2828+0.06929*83.11)
pnorm (-1.9731+0.04176*83.11)


# "phat" from glm
logit.phat<-logitmod$fitted.values
probit.phat<-probitmod$fitted.values

xb1<-0.06929*liberal2
xb2<-0.04176*liberal2


plot(logit.phat~xb1,lty=1,type="l",xlim=c(1,6),xlab="xb",ylab="Predicted Probability")

plot(probit.phat~xb2,lty=2,type="l",col="blue",xlim=c(0,4),xlab="xb",ylab="Predicted Probability")


# Plot predicted probabilities against one another to see how closely they match. 
require(ggplot2)


par(mfrow=c(1,2))
plot(logit.phat~xb1,lty=1,type="lines",xlim=c(1,6),
     xlab="Logit Model",ylab="Predicted Probability")
plot(probit.phat~xb2,lty=2,type="lines",col="blue",
     xlim=c(0,4),xlab="Probit Model",ylab="Predicted Probability")

par(mfrow=c(1,1))



# Plot the logit and probit curve

plot(propetit~liberal2,mydata, 
     xlim=c(0,100), ylim = c(0,1), 
     xlab="Liberalism", ylab="Prob of Granting Relief")
x <- seq(0,100,1)
lines(x,ilogit(-3.2828+0.06929*x),lty=1,col="red",lwd=2) # Plug in estimated intercept and slope, logit curve
lines(x,pnorm(-1.9731+0.04176*x),lty=2,col="blue",lwd=2) # Plug in estimated intercept and slope, probit curve

##Marginal Effects

library(mfx)

logitmfx(propetit ~ liberal2, data=mydata)
probitmfx(propetit ~ liberal2, data=mydata)



# Obtaining odds ratios (only logit)
# Mean effects
exp(logitmod$coefficients)
# With CIs
exp(confint(logitmod))

##Alternate way to obtain odds ratio
library(mfx)
logitor(propetit ~ liberal2, data=mydata)


# 2. Consider Alternative Model Specification

# 2.1 Alternative Model specification
# logitmod2<-glm(propetit ~ liberal2+usparty+ineffcou+multpet, family=binomial(link=logit), mydata)

# Using update()
logitmod2<-update(logitmod, .~.+usparty+ineffcou+multpet)
summary(logitmod2)



library(stargazer)
stargazer(logitmod, logitmod2, type = "text")

# Obtaining odds ratios and their CIs
round(exp(cbind(Estimate=coef(logitmod2),confint(logitmod2))),2)


#Alternative ways to present results

library(effects)

plot(allEffects(logitmod))
plot(effect("liberal2", logitmod))
plot(allEffects(logitmod2))
plot(effect("liberal2", logitmod2))

##Some predictions look weird in R, given that the DV is bounded by 0 and 1



##Predictions in scale of the GLM
logit.phat2<-predict(logitmod)
probit.phat2<-predict(probitmod)

##Predictions in scale of the response
logit.phat3<-predict(logitmod, type="response")
probit.phat3<-predict(probitmod, type="response")

#Predicted Probabilities in scale of the model
plot(logit.phat2~mydata$liberal2,lty=1,type="l",xlab="x",ylab="Predicted Log Odds")
plot(probit.phat2~mydata$liberal2,lty=2,type="l",col="blue",xlab="x",ylab="Predicted Log Odds")

#Predicted Probabilities in scale of the response
plot(logit.phat3~mydata$liberal2,lty=1,type="l",xlab="x",ylab="Predicted Probability")
plot(probit.phat3~mydata$liberal2,lty=2,type="l",col="blue",xlab="x",ylab="Predicted Probability")


#Predictions in scale of response (untranslated with link function)
xb1<- -3.2828+0.06929*liberal2
xb2<--1.9731+0.04176*liberal2

#Predictions in scale of model (translated with link function)
xb3<-ilogit(-3.2828+0.06929*liberal2)
xb4<-pnorm (-1.9731+0.04176*liberal2)

#Predicted Probabilities in scale of response (untranslated with link function)

plot(logit.phat3~xb1,lty=1,type="l",xlab="xb",ylab="Predicted Probability")
plot(probit.phat3~xb2,lty=2,type="l",col="blue",xlab="xb",ylab="Predicted Probability")

#Predicted Probabilities  in scale of model (i.e. translated with link function)

plot(logit.phat3~xb3,lty=1,type="l",xlab="xb",ylab="Predicted Probability")
plot(probit.phat3~xb4,lty=2,type="l",col="blue",xlab="xb",ylab="Predicted Probability")



##############################################################################
####

# Transforming predict ----------------------------------------------------


#Predict from model (translated, will be linear)
plot(logit.phat2~mydata$liberal2,lty=1,type="l",xlab="x",ylab="Predicted Log Odds")

#Log odds difficult to interpret, so take the e^(ln(odds) to obtain odds predictions)
plot(exp(logit.phat2)~mydata$liberal2,lty=2,type="l",xlab="x",ylab="Predicted Odds")

#Odds (p* / 1-p) are not intuitive, so we can obtain p by multiplying the odds by 1-p
plot((exp(logit.phat2)*(1-logit.phat3))~mydata$liberal2,lty=2,type="l",xlab="x",ylab="Predicted Probabilities")
#Of course, you may not have a good estimate of p, but e^ln(odds)/1+e^[ln(odds)] will give you p as well
plot((exp(logit.phat2)/(1+exp(logit.phat2)))~mydata$liberal2,lty=2,type="l",xlab="x",ylab="Predicted Probabilities")



#Predicted probabilities in response scale
plot(logit.phat3~mydata$liberal2,lty=2,type="l",xlab="x",ylab="Predicted Probabilities (response)")




##Response residuals
resid.res<-residuals(logitmod, type="response")

resid.mod<-residuals(logitmod, type="pear")

plot(resid.res~mydata$liberal2)
plot(resid.mod~mydata$liberal2)



###Another Example


data(mtcars)

# Create a binary dependent variable based on mpg
mtcars$high_mpg <- ifelse(mtcars$mpg >= median(mtcars$mpg), 1, 0)

# Estimate the logistic regression model
model <- glm(high_mpg ~ wt + hp + am, family = binomial(link = "logit"), data = mtcars)

# Set explanatory variables
wt_range <- seq(min(mtcars$wt), max(mtcars$wt), length.out = 100)
x.auto <- data.frame(wt = wt_range, hp = mean(mtcars$hp), am = 0)
x.manual <- data.frame(wt = wt_range, hp = mean(mtcars$hp), am = 1)

# Calculate predicted probabilities
prob.auto <- predict(model, newdata = x.auto, type = "response")
prob.manual <- predict(model, newdata = x.manual, type = "response")

# Create a data frame for plotting
plot_data <- data.frame(Weight = wt_range, 
                        Automatic = prob.auto, 
                        Manual = prob.manual)

# Reshape the data for plotting
library(reshape2)
plot_data_melt <- melt(plot_data, id.vars = "Weight", variable.name = "Transmission", value.name = "Probability")

# Create the plot
library(ggplot2)
ggplot(plot_data_melt, aes(x = Weight, y = Probability, color = Transmission)) +
  geom_line() +
  xlab("Vehicle Weight (1000 lbs)") +
  ylab("Predicted Probability of High MPG") +
  scale_color_manual(values = c("blue", "red"), labels = c("Automatic", "Manual"))




## Assessing Model Fit
#Hosmer-Lemeshow Goodness of Fit
#install.packages("ResourceSelection")

logitmod <- glm(propetit ~ liberal2, 
                family=binomial(link=logit), mydata)
summary(logitmod)

logitmod2<-update(logitmod, .~.+usparty+ineffcou+multpet)
summary(logitmod2)


library(ResourceSelection)

#  Hosmer-Lemeshow -----------------------------------------------------------

hoslem.test(mydata$propetit, fitted(logitmod))
hoslem.test(mydata$propetit, fitted(logitmod2))

plot(logitmod)
#Our model appears to fit  poorly because we have a significant 
##difference between the model and the observed data


#Classification-based methods
y<-mydata$propetit
pred1<-predict(logitmod,type="response")
pred2<-predict(logitmod2,type="response")





# ##ROC -------------------------------------------------------------------

library(ROCR)
predRoc1<-prediction(pred1, mydata$propetit)
perfRoc1<-performance(predRoc1,"tpr","fpr")
plot(perfRoc1,colorize=TRUE)

predRoc2<-prediction(pred2, mydata$propetit)
perfRoc2<-performance(predRoc2,"tpr","fpr")
plot(perfRoc2,colorize=TRUE)

#OR
#install.packages("pROC")
require(pROC)
plot.roc(y,pred1,col="red")
plot.roc(y,pred2,col="blue")

#the overall predictive power of the model (across all
#possible thresholds) can be summarized in terms of the
#area under the ROC curve since the ROC is defined on
#the unit square.

library(pROC)
auc(mydata$propetit, pred1)
auc(mydata$propetit, pred2)






#  Likelihood-Ratio -------------------------------------------------------


# Likelihood-Ratio based approach
# Compare AICs
extractAIC(logitmod)
extractAIC(logitmod2)
# Model 2 produces slightly smaller AIC than Model 1


# LR Test to compare model fit
require(lmtest)
lrtest(logitmod,logitmod2)


# # Heatmap Plot ----------------------------------------------------------

#we can plot heat map to compare goodness of fit. See Esarey and
#Pierce's 2012 Political Analysis paper for more theoretical discussions on their new approach. When
#reading a heat map plot, we also compare model predictions with smoothed empirical predictions.
#The 45-degree line references a perfect fit. Any deviance from that line suggests a loss in goodness
#of fit. P-value legend shows if any deviance is statistically significant (dark color means statistical
#signifcance). The following two heat map plots show that model 2 outperforms model 1.

#install.packages("heatmapFit")
require(heatmapFit)
heatmap.fit(y,pred1,reps=1000,legend=FALSE)
heatmap.fit(y,pred2,reps=1000,legend=FALSE)


# ####Separation PLOT -----------------------------------------------------


library(separationplot)

separationplot(pred=logitmod$fitted.values, actual=logitmod$y, type="rect",
               line=TRUE, show.expected=TRUE,  width = 9, height = 1.2, heading="Probability of Y", zerosfirst)

separationplot(pred=logitmod2$fitted.values, actual=logitmod2$y, type="rect",
               line=TRUE, show.expected=TRUE,  width = 9, height = 1.2, heading="Probability of Y", zerosfirst)

#Perfect Predicted Model
e<-plogis(seq(-10, 3.02, length.out=500))
sum(e) # should be 118
f<-c(rep(0, 382), rep(1, 118))
separationplot(pred=e, actual=f, type="rect", show.expected=T)


##############################################################################
###Interactions - 

# Interactions ------------------------------------------------------------


logitint <- glm(propetit ~ liberal2*usparty +ineffcou +multpet, 
                family=binomial(link=logit), mydata)
summary(logitint)

summary(mydata)


library(interplot)
interplot(m = logitint, var1 = "usparty", var2 = "liberal2", , hist = TRUE)
interplot(m = logitint, var1 = "liberal2", var2 = "usparty")

mydata2<-read.dta("data/jcrv10.dta")

logitint2 <- glm(wl3~ratio*dem+init+concap+ally1, 
                 family=binomial(link=logit), mydata2)
summary(logitint2)
interplot(m = logitint2, var1 = "ratio", var2 = "dem", hist="TRUE")+ 
  # Add labels for X and Y axes
  xlab("Democracy)") +
  ylab("Marginal Effect of Increased Interest Rates") +
  geom_hline(yintercept = 0, linetype = "dashed")

interplot(m = logitint2, var1 = "ratio", var2 = "dem", hist="FALSE", 
          predPro = TRUE, var2_vals = c(-10, 10))+ 
  # Add labels for X and Y axes
  ggtitle("Conditional Predicted Probabilities") +
  scale_colour_discrete(guide = guide_legend(title = "Dem"), labels = c("Low", "High")) + 
  scale_fill_discrete(guide = guide_legend(title = "Dem"), labels = c("Low", "High")) +
  theme(legend.position = c(.2, .8), legend.justification = c(0, .5))







