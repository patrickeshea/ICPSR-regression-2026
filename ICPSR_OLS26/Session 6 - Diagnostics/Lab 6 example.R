## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

# Install and load necessary packages
# install.packages("WDI")
# install.packages("countrycode")
library(WDI)
library(haven)
library(dplyr)
library(ggplot2)
library(car)
library(countrycode)
library(lmtest)
library(plm)
# Download data from the World Bank
aid_data <- WDI(indicator = c("DT.ODA.ODAT.GN.ZS","SE.PRM.CMPT.ZS","SH.DYN.MORT", "SH.STA.MMRT", "SL.EMP.VULN.ZS"), 
                country = "all", start = 1960, end = 2020, extra = TRUE)

# Tidy the dataset
aid_data2 <- aid_data %>%
  filter(!is.na(iso3c)) %>%  # Remove aggregate observations
  mutate(year = as.integer(year))

# Load and merge the Polity IV dataset
polity4 <- read_dta("data/polity4.dta")

aid_data3 <- merge(aid_data2, polity4, by = c("iso3c", "year"), all.x = TRUE)


# Identify a continuous variable to proxy aid effectiveness
# For example, let's use "SH.DYN.MORT" (Mortality rate, under-5, per 1,000 live births)


# Check assumptions about normality
ggplot(aid_data, aes(x = SH.DYN.MORT)) +
  geom_density() +
  labs(title = "Distribution of Under-5 Mortality Rate")

# Fit a regression with under-5 mortality as the dependent variable
model1 <- lm(SH.DYN.MORT ~ DT.ODA.ODAT.GN.ZS, data = aid_data3)
summary(model1)

# Include polity2 as a control
model2 <- lm(SH.DYN.MORT~ DT.ODA.ODAT.GN.ZS + polity2, data = aid_data3)
summary(model2)

# Create a binary measure of democracy (demdum)
aid_data3$demdum <- ifelse(aid_data3$polity2 >= 6, 1, 0)

# Fit a regression with the binary democracy measure
model3 <- lm(SH.DYN.MORT ~ DT.ODA.ODAT.GN.ZS + demdum, data = aid_data3)
summary(model3)

# Interact democracy and aid
model4 <- lm(SH.DYN.MORT ~ DT.ODA.ODAT.GN.ZS * demdum, data = aid_data3)
summary(model4)

# Present the results in a table
stargazer::stargazer(model1, model2, model3, model4, type = "text")

# Present the results in a graph
new_data <- expand.grid(DT.ODA.ODAT.GN.ZS = seq(min(aid_data3$DT.ODA.ODAT.GN.ZS, na.rm = TRUE),
                                                max(aid_data3$DT.ODA.ODAT.GN.ZS, na.rm = TRUE),
                                                length.out = 100),
                        demdum = c(0, 1))
new_data$predicted <- predict(model4, newdata = new_data)




ggplot(new_data, aes(x = DT.ODA.ODAT.GN.ZS, y = predicted, color = factor(demdum))) +
  geom_line() +
  labs(title = "Interaction Effect of Aid and Democracy on Under-5 Mortality",
       x = "Aid as % of Government Expenses",
       y = "Under-5 Mortality Rate",
       color = "Democracy") +
  theme_minimal()

# Run diagnostic tests
# Residual plot
plot(model4, which = 1)

plot(model4)

residualPlots(model4)

# Q-Q plot
qqPlot(model4, main = "Q-Q Plot")

# Breusch-Pagan test for heteroscedasticity
bptest(model4)


library(sandwich)
coeftest(model4, vcov = vcovHC(model4, type = "HC1"))


# Variance Inflation Factors (VIF)
vif(model4)





## ============================================================
##Additional code to see if we can solve these problems
## ============================================================
## The diagnostics above likely revealed two main issues:
##   1. Serial correlation — our data are country-year panels, so
##      observations within a country are not independent over time.
##      Mortality rates are highly persistent: a country with high
##      child mortality in 2005 almost certainly has high mortality
##      in 2006 too. Pooled OLS ignores this dependence.
##   2. Heteroskedasticity — the variance of residuals is not constant
##      across fitted values, which the Breusch-Pagan test confirms.
##
## Below, we try two specification changes to see if they help.
## This is the iterative workflow: diagnose -> adjust -> re-check.
## ============================================================


## ---- Strategy 1: Add a lagged dependent variable ----
## Including last year's mortality rate as a predictor serves two purposes:
##   - It absorbs much of the temporal persistence (serial correlation),
##     since most of the variation in mortality is explained by its own past.
##   - It shifts the interpretation: the other coefficients now capture
##     effects on the *change* in mortality, conditional on last year's level.
##
## To create a proper lag, we need to tell R this is panel data (country-year).
## The plm package's pdata.frame() handles this.

## First attempt — this may fail if there are duplicate country-year rows
## in the data (e.g., from regional aggregates in the WDI download).
# aid_data3_p <- pdata.frame(aid_data3, index = c("iso3c", "year"))

aid_data3_p <- pdata.frame(aid_data3, index = c("iso3c", "year"))

aid_data3_p$lag_SH.DYN.MORT <- lag(aid_data3_p$SH.DYN.MORT, k = 1)

model_lag <- lm(SH.DYN.MORT ~ lag_SH.DYN.MORT + DT.ODA.ODAT.GN.ZS * demdum, data = aid_data3_p)


## ---- Data cleaning: remove duplicate country-year observations ----
## Check whether duplicates exist
duplicated_rows <- aid_data3[duplicated(aid_data3[c("iso3c", "year")]), ]
head(duplicated_rows)

##  If duplicates appear, they are likely regional/income-group
## aggregates that share an iso3c code, or data entry artifacts.
## We drop them so that each country-year appears exactly once.
aid_data3_unique <- aid_data3[!duplicated(aid_data3[c("iso3c", "year")]), ]

## Now create the panel data frame and the lagged variable
aid_data3_p <- pdata.frame(aid_data3_unique, index = c("iso3c", "year"))
aid_data3_p$lag_SH.DYN.MORT <- lag(aid_data3_p$SH.DYN.MORT, k = 1)

## Re-estimate the interaction model with the lagged DV
model_lag <- lm(SH.DYN.MORT ~ lag_SH.DYN.MORT + DT.ODA.ODAT.GN.ZS * demdum, data = aid_data3_p)
summary(model_lag)

## Compare the R-squared and residual standard error to model4.
## The lagged DV will likely absorb a large share of variance.
## Look at the coefficient on aid — has it changed in size or significance?
## What does that tell us about how much of the earlier result was driven
## by slow-moving, persistent differences between countries?

## Re-run diagnostic plots to see if the problems improved
plot(model_lag)

## Check the residual vs. fitted plot and the Q-Q plot.
##  Is heteroskedasticity less severe?
## If problems remain, we might need further adjustments.


## ---- Strategy 2: Log-transform the aid variable ----
## Aid as a share of GNI is right-skewed: most countries receive modest
## amounts, but a few receive very large shares. This skewness can drive
## heteroskedasticity and give outliers excessive influence.
## Taking log(aid + 1) compresses the right tail.
## The +1 ensures the log is defined for countries with zero aid.

model_lag2 <- lm(SH.DYN.MORT ~ lag_SH.DYN.MORT + log(DT.ODA.ODAT.GN.ZS + 1) * demdum, data = aid_data3_p)
summary(model_lag2)

plot(model_lag2)

## Compare model_lag2 to model_lag.
## Does the log transformation improve the diagnostic plots?
## How does the interpretation of the aid coefficient change?
## (Recall: with a log-transformed X, a 1% increase in aid is associated
##  with a coefficient/100 unit change in mortality.)
##
## Note: neither the lagged DV nor the log transform is a "fix" for all
## problems. These are specification choices with tradeoffs. A lagged DV
## can introduce bias (Nickell bias) in short panels. Log transforms change
## the substantive interpretation. The goal is to think carefully about
## what is driving the diagnostic issues and whether your adjustments
## are theoretically motivated, not just statistically convenient.



