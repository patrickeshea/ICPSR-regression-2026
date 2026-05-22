## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

rm(list=ls())

library(faraway)
require(foreign)
mydata<-read.dta("data/urbanbias.dta")
mydata2 <- mydata[ which(mydata$democ==0),]
mydata2 <- mydata2[ which(mydata2$default_lead!="NA"),]


# Day 4 - Estimating Binary Response Models -------------------------------

#creating new scale of imports for interpretation purposes
mydata2$foodimp_gdp2<-mydata2$foodimp_gdp*100

hist(mydata2$foodimp_gdp2)
#1. Logit and Probit


logit1 <- glm(default_lead ~ foodimp_gdp2, 
              family=binomial(link=logit), mydata2)
probit1 <- glm(default_lead ~ foodimp_gdp2, 
               family=binomial(link=probit), mydata2)
summary(logit1)
summary(probit1)

library(stargazer)
stargazer(logit1,probit1, type = "text")


##2. Interpret

#What can you infer from the output of the regression?
## Food imports have positive and statistically signficant effect (or association) with the ln(odds of default)

#Provide substantiative interpretations
#Odd Ratio
exp(logit1$coefficients[2])
#A unit (percentage) change in food imports increase the odds of default by 8%

summary(mydata2$foodimp_gdp2)

#Note that the minimum is negative which seems nonsensical (unless this is measured in net imports, which isn't clear)
# So I'll look at food imports at 0, mean, and max
ilogit(logit1$coefficients[1]+logit1$coefficients[2]*0)
pnorm(probit1$coefficients[1]+probit1$coefficients[2]*0)
ilogit(logit1$coefficients[1]+logit1$coefficients[2]*mean(mydata2$foodimp_gdp2))
pnorm(probit1$coefficients[1]+probit1$coefficients[2]*mean(mydata2$foodimp_gdp2))
ilogit(logit1$coefficients[1]+logit1$coefficients[2]*max(mydata2$foodimp_gdp2))
pnorm(probit1$coefficients[1]+probit1$coefficients[2]*max(mydata2$foodimp_gdp2))

library(mfx)
logitmfx(default_lead ~ foodimp_gdp2, data=mydata2)
probitmfx(default_lead ~ foodimp_gdp2, data=mydata2)

##Predictions in scale of the GLM
logit.phat2<-predict(logit1)
probit.phat2<-predict(probit1)

##Predictions in scale of the response
logit.phat3<-predict(logit1, type="response")
probit.phat3<-predict(probit1, type="response")


#Predict from model (translated, will be linear)
plot(logit.phat2~mydata2$foodimp_gdp2,lty=1,type="l",
     xlab="x",ylab="Predicted Log Odds" )


plot(logit.phat3 ~ mydata2$foodimp_gdp2,lty=2,type="p",
     xlab="x",ylab="Predicted Probabilities")

library(effects)

plot(allEffects(logit1))

#################################################

logit2 <- glm(default_lead ~ foodimp_gdp2 + wdi_urban, 
              family=binomial(link=logit), mydata2)
probit2 <- glm(default_lead ~ foodimp_gdp2 + wdi_urban, 
               family=binomial(link=probit), mydata2)
summary(logit2)
summary(probit2)

##2. Interpret
#Provide substantiative interpretations
#Odd Ratio
exp(logit2$coefficients[3])
#A unit (percentage) change in food imports increase the odds of default by 2.7%

summary(mydata2$wdi_urban)

#Note that the minimum is negative which seems nonsensical (unless this is measured in net imports, which isn't clear)
# So I'll look at food imports at 0, mean, and max
ilogit(logit2$coefficients[1]+logit2$coefficients[3]*8.12)
pnorm (probit2$coefficients[1]+probit2$coefficients[3]*8.12)
ilogit(logit2$coefficients[1]+logit2$coefficients[3]*43)
pnorm (probit2$coefficients[1]+probit2$coefficients[3]*43)
ilogit(logit2$coefficients[1]+logit2$coefficients[3]*86)
pnorm (probit2$coefficients[1]+probit2$coefficients[3]*86)


##Predictions plots
library(effects)

plot(allEffects(logit1))
plot(effect("wdi_urban", logit2))
plot(allEffects(probit2))
plot(effect("wdi_urban", probit2))






logitmod <- glm(default_lead ~ foodimp_gdp+wdi_urban+ln_gdppc+chg_gdp+debtgdp + inflation + trade, 
                family=binomial(link=logit), mydata2)
summary(logitmod)
logitmod2 <- glm(default_lead ~ foodimp_gdp*wdi_urban+ln_gdppc+chg_gdp+debtgdp + inflation + trade, 
                 family=binomial(link=logit), mydata2)
summary(logitmod2)

exp(logitmod$coefficients)
library(effects)
plot(allEffects(logitmod2), typical=median)

library(interplot)
interplot(m = logitmod2, var1 = "foodimp_gdp", var2 = "wdi_urban",  hist = TRUE)
interplot(m = logitmod2, var1 = "wdi_urban", var2 = "foodimp_gdp", hist = TRUE)


# Diagnostics -------------------------------------------------------------


library(ResourceSelection)

#  Hosmer-Lemeshow -----------------------------------------------------------

hoslem.test(mydata2$default_lead, fitted(logitmod))
hoslem.test(mydata2$default_lead, fitted(logitmod2))

y<-mydata2$default_lead
pred1<-predict(logitmod,type="response")
pred2<-predict(logitmod2,type="response")

require(pROC)
plot.roc(y,pred1,col="red")
plot.roc(y,pred2,col="blue")


library(pROC)
auc(mydata2$default_lead, pred1)
auc(mydata2$default_lead, pred2)

extractAIC(logitmod)
extractAIC(logitmod2)

require(lmtest)
lrtest(logitmod,logitmod2)


require(heatmapFit)
heatmap.fit(y,pred1,reps=1000,legend=FALSE)
heatmap.fit(y,pred2,reps=1000,legend=FALSE)




# ####Separation PLOT -----------------------------------------------------


library(separationplot)

separationplot(pred=logitmod$fitted.values, actual=logitmod$y, type="rect",
               line=TRUE, show.expected=TRUE,  width = 9, height = 1.2, heading="Probability of Y", zerosfirst)

separationplot(pred=logitmod2$fitted.values, actual=logitmod2$y, type="rect",
               line=TRUE, show.expected=TRUE,  width = 9, height = 1.2, heading="Probability of Y", zerosfirst)

